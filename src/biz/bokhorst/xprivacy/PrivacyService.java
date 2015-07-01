package biz.bokhorst.xprivacy;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import android.annotation.SuppressLint;
import android.app.Notification;
import android.app.NotificationManager;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.Resources;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteDoneException;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteStatement;
import android.graphics.PixelFormat;
import android.net.Uri;
import android.os.Binder;
import android.os.Build;
import android.os.Environment;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.Process;
import android.os.RemoteException;
import android.os.StrictMode;
import android.os.SystemClock;
import android.os.StrictMode.ThreadPolicy;
import android.support.v4.app.NotificationCompat;
import android.text.TextUtils;
import android.util.Log;
import android.util.Patterns;
import android.util.SparseArray;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.Spinner;
import android.widget.TableRow;
import android.widget.TextView;
import android.widget.Toast;

public class PrivacyService extends IPrivacyService.Stub {
	private static int mXUid = -1;
	private static Object mAm;
	private static Context mContext;
	private static String mSecret = null;
	private static Thread mWorker = null;
	private static Handler mHandler = null;
	private static long mOnDemandLastAnswer = 0;
	private static Semaphore mOndemandSemaphore = new Semaphore(1, true);
	private static List<String> mListError = new ArrayList<String>();
	private static IPrivacyService mClient = null;

	private static final String cTableRestriction = "restriction";
	private static final String cTableUsage = "usage";
	private static final String cTableSetting = "setting";

	private static final int cCurrentVersion = 481;
	private static final String cServiceName = "xprivacy481";

	private boolean mCorrupt = false;
	private boolean mNotified = false;
	private SQLiteDatabase mDb = null;
	private SQLiteDatabase mDbUsage = null;
	private SQLiteStatement stmtGetRestriction = null;
	private SQLiteStatement stmtGetSetting = null;
	private SQLiteStatement stmtGetUsageRestriction = null;
	private SQLiteStatement stmtGetUsageMethod = null;
	private ReentrantReadWriteLock mLock = new ReentrantReadWriteLock(true);
	private ReentrantReadWriteLock mLockUsage = new ReentrantReadWriteLock(true);

	private AtomicLong mCount = new AtomicLong(0);
	private AtomicLong mRestricted = new AtomicLong(0);

	private Map<CSetting, CSetting> mSettingCache = new HashMap<CSetting, CSetting>();
	private Map<CRestriction, CRestriction> mAskedOnceCache = new HashMap<CRestriction, CRestriction>();
	private Map<CRestriction, CRestriction> mRestrictionCache = new HashMap<CRestriction, CRestriction>();

	private final long cMaxUsageDataHours = 12;
	private final int cMaxUsageDataCount = 700;
	private final int cMaxOnDemandDialog = 20; // seconds

	private ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors(),
			new PriorityThreadFactory());

	final class PriorityThreadFactory implements ThreadFactory {
		@Override
		public Thread newThread(Runnable r) {
			Thread t = new Thread(r);
			t.setPriority(Thread.MIN_PRIORITY);
			return t;
		}
	}

	// sqlite3 /data/system/xprivacy/xprivacy.db

	private PrivacyService() {
	}

	private static PrivacyService mPrivacyService = null;

	private static String getServiceName() {
		return (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP ? "user." : "") + cServiceName;
	}

	public static void register(List<String> listError, ClassLoader classLoader, String secret, Object am) {
		// Store secret and errors
		mAm = am;
		mSecret = secret;
		mListError.addAll(listError);

		try {
			// Register privacy service
			mPrivacyService = new PrivacyService();

			// @formatter:off
			// public static void addService(String name, IBinder service)
			// public static void addService(String name, IBinder service, boolean allowIsolated)
			// @formatter:on

			// Requires this in /service_contexts
			// xprivacy453 u:object_r:system_server_service:s0

			Class<?> cServiceManager = Class.forName("android.os.ServiceManager", false, classLoader);
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
				Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class,
						boolean.class);
				mAddService.invoke(null, getServiceName(), mPrivacyService, true);
			} else {
				Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class);
				mAddService.invoke(null, getServiceName(), mPrivacyService);
			}

			// This will and should open the database
			Util.log(null, Log.WARN, "Service registered name=" + getServiceName() + " version=" + cCurrentVersion);

			// Publish semaphore to activity manager service
			XActivityManagerService.setSemaphore(mOndemandSemaphore);

			// Get context
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
				Field fContext = null;
				Class<?> cam = am.getClass();
				while (cam != null && fContext == null)
					try {
						fContext = cam.getDeclaredField("mContext");
					} catch (NoSuchFieldException ignored) {
						cam = cam.getSuperclass();
					}

				if (fContext == null)
					Util.log(null, Log.ERROR, am.getClass().getName() + ".mContext not found");
				else {
					fContext.setAccessible(true);
					mContext = (Context) fContext.get(am);
				}
			}

			// Start a worker thread
			mWorker = new Thread(new Runnable() {
				@Override
				public void run() {
					try {
						Looper.prepare();
						mHandler = new Handler();
						Looper.loop();
					} catch (Throwable ex) {
						Util.bug(null, ex);
					}
				}
			});
			mWorker.start();

		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	public static boolean isRegistered() {
		return (mPrivacyService != null);
	}

	public static boolean checkClient() {
		// Runs client side
		try {
			IPrivacyService client = getClient();
			if (client != null)
				return (client.getVersion() == cCurrentVersion);
		} catch (SecurityException ignored) {
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
		return false;
	}

	public static IPrivacyService getClient() {
		// Runs client side
		if (mClient == null)
			try {
				// public static IBinder getService(String name)
				Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
				Method mGetService = cServiceManager.getDeclaredMethod("getService", String.class);
				mClient = IPrivacyService.Stub.asInterface((IBinder) mGetService.invoke(null, getServiceName()));
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		return mClient;
	}

	public static void reportErrorInternal(String message) {
		synchronized (mListError) {
			mListError.add(message);
		}
	}

	public static PRestriction getRestrictionProxy(final PRestriction restriction, boolean usage, String secret)
			throws RemoteException {
		if (isRegistered())
			return mPrivacyService.getRestriction(restriction, usage, secret);
		else {
			IPrivacyService client = getClient();
			if (client == null) {
				Log.w("XPrivacy", "No client for " + restriction);
				PRestriction result = new PRestriction(restriction);
				result.restricted = false;
				return result;
			} else
				return client.getRestriction(restriction, usage, secret);
		}
	}

	public static PSetting getSettingProxy(PSetting setting) throws RemoteException {
		if (isRegistered())
			return mPrivacyService.getSetting(setting);
		else {
			IPrivacyService client = getClient();
			if (client == null) {
				Log.w("XPrivacy", "No client for " + setting + " uid=" + Process.myUid() + " pid=" + Process.myPid());
				Log.w("XPrivacy", Log.getStackTraceString(new Exception("StackTrace")));
				return setting;
			} else
				return client.getSetting(setting);
		}
	}

	// Management

	@Override
	public int getVersion() throws RemoteException {
		enforcePermission(-1);
		return cCurrentVersion;
	}

	@Override
	public List<String> check() throws RemoteException {
		enforcePermission(-1);

		List<String> listError = new ArrayList<String>();
		synchronized (mListError) {
			int c = 0;
			int i = 0;
			while (i < mListError.size()) {
				String msg = mListError.get(i);
				c += msg.length();
				if (c < 10000)
					listError.add(msg);
				else
					break;
				i++;
			}
		}

		return listError;
	}

	@Override
	public boolean databaseCorrupt() {
		return mCorrupt;
	}

	@Override
	public void reportError(String message) throws RemoteException {
		reportErrorInternal(message);
	}

	@Override
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public Map getStatistics() throws RemoteException {
		Map map = new HashMap();
		map.put("restriction_count", mCount.longValue());
		map.put("restriction_restricted", mRestricted.longValue());
		map.put("uptime_milliseconds", SystemClock.elapsedRealtime());
		return map;
	};

	// Restrictions

	@Override
	public void setRestriction(PRestriction restriction) throws RemoteException {
		try {
			enforcePermission(restriction.uid);
			setRestrictionInternal(restriction);
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	private void setRestrictionInternal(PRestriction restriction) throws RemoteException {
		// Validate
		if (restriction.restrictionName == null) {
			Util.log(null, Log.ERROR, "Set invalid restriction " + restriction);
			Util.logStack(null, Log.ERROR);
			throw new RemoteException("Invalid restriction");
		}

		try {
			SQLiteDatabase db = getDb();
			if (db == null)
				return;
			// 0 not restricted, ask
			// 1 restricted, ask
			// 2 not restricted, asked
			// 3 restricted, asked

			mLock.writeLock().lock();
			try {
				db.beginTransaction();
				try {
					// Create category record
					if (restriction.methodName == null) {
						ContentValues cvalues = new ContentValues();
						cvalues.put("uid", restriction.uid);
						cvalues.put("restriction", restriction.restrictionName);
						cvalues.put("method", "");
						cvalues.put("restricted", (restriction.restricted ? 1 : 0) + (restriction.asked ? 2 : 0));
						db.insertWithOnConflict(cTableRestriction, null, cvalues, SQLiteDatabase.CONFLICT_REPLACE);
					}

					// Create method exception record
					if (restriction.methodName != null) {
						ContentValues mvalues = new ContentValues();
						mvalues.put("uid", restriction.uid);
						mvalues.put("restriction", restriction.restrictionName);
						mvalues.put("method", restriction.methodName);
						mvalues.put("restricted", (restriction.restricted ? 0 : 1) + (restriction.asked ? 2 : 0));
						db.insertWithOnConflict(cTableRestriction, null, mvalues, SQLiteDatabase.CONFLICT_REPLACE);
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} finally {
				mLock.writeLock().unlock();
			}

			// Update cache
			synchronized (mRestrictionCache) {
				for (CRestriction key : new ArrayList<CRestriction>(mRestrictionCache.keySet()))
					if (key.isSameMethod(restriction))
						mRestrictionCache.remove(key);

				CRestriction key = new CRestriction(restriction, restriction.extra);
				if (mRestrictionCache.containsKey(key))
					mRestrictionCache.remove(key);
				mRestrictionCache.put(key, key);
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	@Override
	public void setRestrictionList(List<PRestriction> listRestriction) throws RemoteException {
		int uid = -1;
		for (PRestriction restriction : listRestriction)
			if (uid < 0)
				uid = restriction.uid;
			else if (uid != restriction.uid)
				throw new SecurityException();
		enforcePermission(uid);
		for (PRestriction restriction : listRestriction)
			setRestrictionInternal(restriction);
	}

	@Override
	public PRestriction getRestriction(final PRestriction restriction, boolean usage, String secret)
			throws RemoteException {
		long start = System.currentTimeMillis();

		// Translate isolated uid
		restriction.uid = getIsolatedUid(restriction.uid);

		boolean ccached = false;
		boolean mcached = false;
		int userId = Util.getUserId(restriction.uid);
		final PRestriction mresult = new PRestriction(restriction);

		// Disable strict mode
		ThreadPolicy oldPolicy = StrictMode.getThreadPolicy();
		ThreadPolicy newPolicy = new ThreadPolicy.Builder(oldPolicy).permitDiskReads().permitDiskWrites().build();
		StrictMode.setThreadPolicy(newPolicy);

		try {
			// No permissions enforced, but usage data requires a secret

			// Sanity checks
			if (restriction.restrictionName == null) {
				Util.log(null, Log.ERROR, "Get invalid restriction " + restriction);
				return mresult;
			}
			if (usage && restriction.methodName == null) {
				Util.log(null, Log.ERROR, "Get invalid restriction " + restriction);
				return mresult;
			}

			// Get meta data
			Hook hook = null;
			if (restriction.methodName != null) {
				hook = PrivacyManager.getHook(restriction.restrictionName, restriction.methodName);
				if (hook == null)
					// Can happen after updating
					Util.log(null, Log.WARN, "Hook not found in service: " + restriction);
				else if (hook.getFrom() != null) {
					String version = getSetting(new PSetting(userId, "", PrivacyManager.cSettingVersion, null)).value;
					if (version != null && new Version(version).compareTo(hook.getFrom()) < 0)
						if (hook.getReplacedRestriction() == null) {
							Util.log(null, Log.WARN, "Disabled version=" + version + " from=" + hook.getFrom()
									+ " hook=" + hook);
							return mresult;
						} else {
							restriction.restrictionName = hook.getReplacedRestriction();
							restriction.methodName = hook.getReplacedMethod();
							Util.log(null, Log.WARN, "Checking " + restriction + " instead of " + hook);
						}
				}
			}

			// Process IP address
			if (restriction.extra != null && Meta.cTypeIPAddress.equals(hook.whitelist())) {
				int colon = restriction.extra.lastIndexOf(':');
				String address = (colon >= 0 ? restriction.extra.substring(0, colon) : restriction.extra);
				String port = (colon >= 0 ? restriction.extra.substring(colon) : "");

				int slash = address.indexOf('/');
				if (slash == 0) // IP address
					restriction.extra = address.substring(slash + 1) + port;
				else if (slash > 0) // Domain name
					restriction.extra = address.substring(0, slash) + port;
			}

			// Check for system component
			if (!PrivacyManager.isApplication(restriction.uid))
				if (!getSettingBool(userId, PrivacyManager.cSettingSystem, false))
					return mresult;

			// Check if restrictions enabled
			if (usage && !getSettingBool(restriction.uid, PrivacyManager.cSettingRestricted, true))
				return mresult;

			// Check if can be restricted
			if (!PrivacyManager.canRestrict(restriction.uid, getXUid(), restriction.restrictionName,
					restriction.methodName, false))
				mresult.asked = true;
			else {
				// Check cache for method
				CRestriction key = new CRestriction(restriction, restriction.extra);
				synchronized (mRestrictionCache) {
					if (mRestrictionCache.containsKey(key)) {
						mcached = true;
						CRestriction cache = mRestrictionCache.get(key);
						mresult.restricted = cache.restricted;
						mresult.asked = cache.asked;
					}
				}

				if (!mcached) {
					boolean methodFound = false;
					PRestriction cresult = new PRestriction(restriction.uid, restriction.restrictionName, null);

					// Check cache for category
					CRestriction ckey = new CRestriction(cresult, null);
					synchronized (mRestrictionCache) {
						if (mRestrictionCache.containsKey(ckey)) {
							ccached = true;
							CRestriction crestriction = mRestrictionCache.get(ckey);
							cresult.restricted = crestriction.restricted;
							cresult.asked = crestriction.asked;
							mresult.restricted = cresult.restricted;
							mresult.asked = cresult.asked;
						}
					}

					// Get database reference
					SQLiteDatabase db = getDb();
					if (db == null)
						return mresult;

					// Precompile statement when needed
					if (stmtGetRestriction == null) {
						String sql = "SELECT restricted FROM " + cTableRestriction
								+ " WHERE uid=? AND restriction=? AND method=?";
						stmtGetRestriction = db.compileStatement(sql);
					}

					// Execute statement
					mLock.readLock().lock();
					try {
						db.beginTransaction();
						try {
							if (!ccached)
								try {
									synchronized (stmtGetRestriction) {
										stmtGetRestriction.clearBindings();
										stmtGetRestriction.bindLong(1, restriction.uid);
										stmtGetRestriction.bindString(2, restriction.restrictionName);
										stmtGetRestriction.bindString(3, "");
										long state = stmtGetRestriction.simpleQueryForLong();
										cresult.restricted = ((state & 1) != 0);
										cresult.asked = ((state & 2) != 0);
										mresult.restricted = cresult.restricted;
										mresult.asked = cresult.asked;
									}
								} catch (SQLiteDoneException ignored) {
								}

							if (restriction.methodName != null)
								try {
									synchronized (stmtGetRestriction) {
										stmtGetRestriction.clearBindings();
										stmtGetRestriction.bindLong(1, restriction.uid);
										stmtGetRestriction.bindString(2, restriction.restrictionName);
										stmtGetRestriction.bindString(3, restriction.methodName);
										long state = stmtGetRestriction.simpleQueryForLong();
										// Method can be excepted
										if (mresult.restricted)
											mresult.restricted = ((state & 1) == 0);
										// Category asked=true takes precedence
										if (!mresult.asked)
											mresult.asked = ((state & 2) != 0);
										methodFound = true;
									}
								} catch (SQLiteDoneException ignored) {
								}

							db.setTransactionSuccessful();
						} finally {
							db.endTransaction();
						}
					} finally {
						mLock.readLock().unlock();
					}

					// Default dangerous
					if (!methodFound && hook != null && hook.isDangerous())
						if (!getSettingBool(userId, PrivacyManager.cSettingDangerous, false)) {
							if (mresult.restricted)
								mresult.restricted = false;
							if (!mresult.asked)
								mresult.asked = (hook.whitelist() == null);
						}

					// Check whitelist
					if (usage && hook != null && hook.whitelist() != null && restriction.extra != null) {
						String value = getSetting(new PSetting(restriction.uid, hook.whitelist(), restriction.extra,
								null)).value;
						if (value == null) {
							for (String xextra : getXExtra(restriction, hook)) {
								value = getSetting(new PSetting(restriction.uid, hook.whitelist(), xextra, null)).value;
								if (value != null)
									break;
							}
						}
						if (value != null) {
							// true means allow, false means block
							mresult.restricted = !Boolean.parseBoolean(value);
							mresult.asked = true;
						}
					}

					// Fallback
					if (!mresult.restricted && usage && PrivacyManager.isApplication(restriction.uid)
							&& !getSettingBool(userId, PrivacyManager.cSettingMigrated, false)) {
						if (hook != null && !hook.isDangerous()) {
							mresult.restricted = PrivacyProvider.getRestrictedFallback(null, restriction.uid,
									restriction.restrictionName, restriction.methodName);
							Util.log(null, Log.WARN, "Fallback " + mresult);
						}
					}

					// Update cache
					CRestriction cukey = new CRestriction(cresult, null);
					synchronized (mRestrictionCache) {
						if (mRestrictionCache.containsKey(cukey))
							mRestrictionCache.remove(cukey);
						mRestrictionCache.put(cukey, cukey);
					}
					CRestriction ukey = new CRestriction(mresult, restriction.extra);
					synchronized (mRestrictionCache) {
						if (mRestrictionCache.containsKey(ukey))
							mRestrictionCache.remove(ukey);
						mRestrictionCache.put(ukey, ukey);
					}
				}

				// Ask to restrict
				OnDemandResult oResult = new OnDemandResult();
				if (!mresult.asked && usage) {
					oResult = onDemandDialog(hook, restriction, mresult);

					// Update cache
					if (oResult.ondemand && !oResult.once) {
						CRestriction okey = new CRestriction(mresult, oResult.whitelist ? restriction.extra : null);
						synchronized (mRestrictionCache) {
							if (mRestrictionCache.containsKey(okey))
								mRestrictionCache.remove(okey);
							mRestrictionCache.put(okey, okey);
						}
					}
				}

				// Notify user
				if (!oResult.ondemand && mresult.restricted && usage && hook != null && hook.shouldNotify()) {
					notifyRestricted(restriction);
					mresult.time = new Date().getTime();
				}
			}

			// Store usage data
			if (usage && hook != null)
				storeUsageData(restriction, secret, mresult);

		} catch (SQLiteException ex) {
			notifyException(ex);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		} finally {
			StrictMode.setThreadPolicy(oldPolicy);
		}

		long ms = System.currentTimeMillis() - start;
		Util.log(
				null,
				ms < PrivacyManager.cWarnServiceDelayMs ? Log.INFO : Log.WARN,
				String.format("Get service %s%s %d ms", restriction, (ccached ? " (ccached)" : "")
						+ (mcached ? " (mcached)" : ""), ms));

		if (mresult.debug)
			Util.logStack(null, Log.WARN);
		if (usage) {
			mCount.incrementAndGet();
			if (mresult.restricted)
				mRestricted.incrementAndGet();
		}

		return mresult;
	}

	private void storeUsageData(final PRestriction restriction, String secret, final PRestriction mresult)
			throws RemoteException {
		// Check if enabled
		final int userId = Util.getUserId(restriction.uid);
		if (getSettingBool(userId, PrivacyManager.cSettingUsage, true)
				&& !getSettingBool(restriction.uid, PrivacyManager.cSettingNoUsageData, false)) {
			// Check secret
			boolean allowed = true;
			if (Util.getAppId(Binder.getCallingUid()) != getXUid()) {
				if (mSecret == null || !mSecret.equals(secret)) {
					allowed = false;
					Util.log(null, Log.WARN, "Invalid secret restriction=" + restriction);
				}
			}

			if (allowed) {
				mExecutor.execute(new Runnable() {
					public void run() {
						try {
							if (XActivityManagerService.canWriteUsageData()) {
								SQLiteDatabase dbUsage = getDbUsage();
								if (dbUsage == null)
									return;

								// Parameter
								String extra = "";
								if (restriction.extra != null)
									if (getSettingBool(userId, PrivacyManager.cSettingParameters, false))
										extra = restriction.extra;

								// Value
								if (restriction.value != null)
									if (!getSettingBool(userId, PrivacyManager.cSettingValues, false))
										restriction.value = null;

								mLockUsage.writeLock().lock();
								try {
									dbUsage.beginTransaction();
									try {
										ContentValues values = new ContentValues();
										values.put("uid", restriction.uid);
										values.put("restriction", restriction.restrictionName);
										values.put("method", restriction.methodName);
										values.put("restricted", mresult.restricted);
										values.put("time", new Date().getTime());
										values.put("extra", extra);
										if (restriction.value == null)
											values.putNull("value");
										else
											values.put("value", restriction.value);
										dbUsage.insertWithOnConflict(cTableUsage, null, values,
												SQLiteDatabase.CONFLICT_REPLACE);

										dbUsage.setTransactionSuccessful();
									} finally {
										dbUsage.endTransaction();
									}
								} finally {
									mLockUsage.writeLock().unlock();
								}
							}
						} catch (SQLiteException ex) {
							Util.log(null, Log.WARN, ex.toString());
						} catch (Throwable ex) {
							Util.bug(null, ex);
						}
					}
				});
			}
		}
	}

	@Override
	public List<PRestriction> getRestrictionList(PRestriction selector) throws RemoteException {
		List<PRestriction> result = new ArrayList<PRestriction>();
		try {
			enforcePermission(selector.uid);

			PRestriction query;
			if (selector.restrictionName == null)
				for (String sRestrictionName : PrivacyManager.getRestrictions()) {
					PRestriction restriction = new PRestriction(selector.uid, sRestrictionName, null, false);
					query = getRestriction(restriction, false, null);
					restriction.restricted = query.restricted;
					restriction.asked = query.asked;
					result.add(restriction);
				}
			else
				for (Hook md : PrivacyManager.getHooks(selector.restrictionName, null)) {
					PRestriction restriction = new PRestriction(selector.uid, selector.restrictionName, md.getName(),
							false);
					query = getRestriction(restriction, false, null);
					restriction.restricted = query.restricted;
					restriction.asked = query.asked;
					result.add(restriction);
				}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
		return result;
	}

	@Override
	public boolean isRestrictionSet(PRestriction restriction) throws RemoteException {
		try {
			// No permissions required
			boolean set = false;

			SQLiteDatabase db = getDb();
			if (db != null) {
				// Precompile statement when needed
				if (stmtGetRestriction == null) {
					String sql = "SELECT restricted FROM " + cTableRestriction
							+ " WHERE uid=? AND restriction=? AND method=?";
					stmtGetRestriction = db.compileStatement(sql);
				}

				// Execute statement
				mLock.readLock().lock();
				try {
					db.beginTransaction();
					try {
						try {
							synchronized (stmtGetRestriction) {
								stmtGetRestriction.clearBindings();
								stmtGetRestriction.bindLong(1, restriction.uid);
								stmtGetRestriction.bindString(2, restriction.restrictionName);
								stmtGetRestriction.bindString(3, restriction.methodName);
								stmtGetRestriction.simpleQueryForLong();
								set = true;
							}
						} catch (SQLiteDoneException ignored) {
						}

						db.setTransactionSuccessful();
					} finally {
						db.endTransaction();
					}
				} finally {
					mLock.readLock().unlock();
				}
			}

			return set;
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	@Override
	public void deleteRestrictions(int uid, String restrictionName) throws RemoteException {
		try {
			enforcePermission(uid);
			SQLiteDatabase db = getDb();
			if (db == null)
				return;

			mLock.writeLock().lock();
			try {
				db.beginTransaction();
				try {
					if ("".equals(restrictionName))
						db.delete(cTableRestriction, "uid=?", new String[] { Integer.toString(uid) });
					else
						db.delete(cTableRestriction, "uid=? AND restriction=?", new String[] { Integer.toString(uid),
								restrictionName });
					Util.log(null, Log.WARN, "Restrictions deleted uid=" + uid + " category=" + restrictionName);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} finally {
				mLock.writeLock().unlock();
			}

			// Clear caches
			synchronized (mRestrictionCache) {
				mRestrictionCache.clear();
			}
			synchronized (mAskedOnceCache) {
				mAskedOnceCache.clear();
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	// Usage

	@Override
	public long getUsage(List<PRestriction> listRestriction) throws RemoteException {
		long lastUsage = 0;
		try {
			int uid = -1;
			for (PRestriction restriction : listRestriction)
				if (uid < 0)
					uid = restriction.uid;
				else if (uid != restriction.uid)
					throw new SecurityException();
			enforcePermission(uid);
			SQLiteDatabase dbUsage = getDbUsage();

			// Precompile statement when needed
			if (stmtGetUsageRestriction == null) {
				String sql = "SELECT MAX(time) FROM " + cTableUsage + " WHERE uid=? AND restriction=?";
				stmtGetUsageRestriction = dbUsage.compileStatement(sql);
			}
			if (stmtGetUsageMethod == null) {
				String sql = "SELECT MAX(time) FROM " + cTableUsage + " WHERE uid=? AND restriction=? AND method=?";
				stmtGetUsageMethod = dbUsage.compileStatement(sql);
			}

			mLockUsage.readLock().lock();
			try {
				dbUsage.beginTransaction();
				try {
					for (PRestriction restriction : listRestriction) {
						if (restriction.methodName == null)
							try {
								synchronized (stmtGetUsageRestriction) {
									stmtGetUsageRestriction.clearBindings();
									stmtGetUsageRestriction.bindLong(1, restriction.uid);
									stmtGetUsageRestriction.bindString(2, restriction.restrictionName);
									lastUsage = Math.max(lastUsage, stmtGetUsageRestriction.simpleQueryForLong());
								}
							} catch (SQLiteDoneException ignored) {
							}
						else
							try {
								synchronized (stmtGetUsageMethod) {
									stmtGetUsageMethod.clearBindings();
									stmtGetUsageMethod.bindLong(1, restriction.uid);
									stmtGetUsageMethod.bindString(2, restriction.restrictionName);
									stmtGetUsageMethod.bindString(3, restriction.methodName);
									lastUsage = Math.max(lastUsage, stmtGetUsageMethod.simpleQueryForLong());
								}
							} catch (SQLiteDoneException ignored) {
							}
					}

					dbUsage.setTransactionSuccessful();
				} finally {
					dbUsage.endTransaction();
				}
			} finally {
				mLockUsage.readLock().unlock();
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
		return lastUsage;
	}

	@Override
	public List<PRestriction> getUsageList(int uid, String restrictionName) throws RemoteException {
		List<PRestriction> result = new ArrayList<PRestriction>();
		try {
			enforcePermission(-1);
			SQLiteDatabase dbUsage = getDbUsage();
			int userId = Util.getUserId(Binder.getCallingUid());

			mLockUsage.readLock().lock();
			try {
				dbUsage.beginTransaction();
				try {
					String sFrom = Long.toString(new Date().getTime() - cMaxUsageDataHours * 60L * 60L * 1000L);
					Cursor cursor;
					if (uid == 0) {
						if ("".equals(restrictionName))
							cursor = dbUsage.query(cTableUsage, new String[] { "uid", "restriction", "method",
									"restricted", "time", "extra", "value" }, "time>?", new String[] { sFrom }, null,
									null, "time DESC");
						else
							cursor = dbUsage.query(cTableUsage, new String[] { "uid", "restriction", "method",
									"restricted", "time", "extra", "value" }, "restriction=? AND time>?", new String[] {
									restrictionName, sFrom }, null, null, "time DESC");
					} else {
						if ("".equals(restrictionName))
							cursor = dbUsage.query(cTableUsage, new String[] { "uid", "restriction", "method",
									"restricted", "time", "extra", "value" }, "uid=? AND time>?", new String[] {
									Integer.toString(uid), sFrom }, null, null, "time DESC");
						else
							cursor = dbUsage.query(cTableUsage, new String[] { "uid", "restriction", "method",
									"restricted", "time", "extra", "value" }, "uid=? AND restriction=? AND time>?",
									new String[] { Integer.toString(uid), restrictionName, sFrom }, null, null,
									"time DESC");
					}

					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (usage data)");
					else
						try {
							int count = 0;
							while (count++ < cMaxUsageDataCount && cursor.moveToNext()) {
								PRestriction data = new PRestriction();
								data.uid = cursor.getInt(0);
								data.restrictionName = cursor.getString(1);
								data.methodName = cursor.getString(2);
								data.restricted = (cursor.getInt(3) > 0);
								data.time = cursor.getLong(4);
								data.extra = cursor.getString(5);
								if (cursor.isNull(6))
									data.value = null;
								else
									data.value = cursor.getString(6);
								if (userId == 0 || Util.getUserId(data.uid) == userId)
									result.add(data);
							}
						} finally {
							cursor.close();
						}

					dbUsage.setTransactionSuccessful();
				} finally {
					dbUsage.endTransaction();
				}
			} finally {
				mLockUsage.readLock().unlock();
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
		return result;
	}

	@Override
	public void deleteUsage(int uid) throws RemoteException {
		try {
			enforcePermission(uid);
			SQLiteDatabase dbUsage = getDbUsage();

			mLockUsage.writeLock().lock();
			try {
				dbUsage.beginTransaction();
				try {
					if (uid == 0)
						dbUsage.delete(cTableUsage, null, new String[] {});
					else
						dbUsage.delete(cTableUsage, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Usage data deleted uid=" + uid);

					dbUsage.setTransactionSuccessful();
				} finally {
					dbUsage.endTransaction();
				}
			} finally {
				mLockUsage.writeLock().unlock();
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	// Settings

	@Override
	public void setSetting(PSetting setting) throws RemoteException {
		try {
			enforcePermission(setting.uid);
			setSettingInternal(setting);
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	private void setSettingInternal(PSetting setting) throws RemoteException {
		try {
			SQLiteDatabase db = getDb();
			if (db == null)
				return;

			mLock.writeLock().lock();
			try {
				db.beginTransaction();
				try {
					if (setting.value == null)
						db.delete(cTableSetting, "uid=? AND type=? AND name=?",
								new String[] { Integer.toString(setting.uid), setting.type, setting.name });
					else {
						// Create record
						ContentValues values = new ContentValues();
						values.put("uid", setting.uid);
						values.put("type", setting.type);
						values.put("name", setting.name);
						values.put("value", setting.value);

						// Insert/update record
						db.insertWithOnConflict(cTableSetting, null, values, SQLiteDatabase.CONFLICT_REPLACE);
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} finally {
				mLock.writeLock().unlock();
			}

			if (PrivacyManager.cSettingAOSPMode.equals(setting.name))
				if (setting.value == null || Boolean.toString(false).equals(setting.value))
					new File("/data/system/xprivacy/aosp").delete();
				else
					new File("/data/system/xprivacy/aosp").createNewFile();

			// Update cache
			CSetting key = new CSetting(setting.uid, setting.type, setting.name);
			key.setValue(setting.value);
			synchronized (mSettingCache) {
				if (mSettingCache.containsKey(key))
					mSettingCache.remove(key);
				if (setting.value != null)
					mSettingCache.put(key, key);
			}

			// Clear restrictions for white list
			if (Meta.isWhitelist(setting.type))
				for (String restrictionName : PrivacyManager.getRestrictions())
					for (Hook hook : PrivacyManager.getHooks(restrictionName, null))
						if (setting.type.equals(hook.whitelist())) {
							PRestriction restriction = new PRestriction(setting.uid, hook.getRestrictionName(),
									hook.getName());
							Util.log(null, Log.WARN, "Clearing cache for " + restriction);
							synchronized (mRestrictionCache) {
								for (CRestriction mkey : new ArrayList<CRestriction>(mRestrictionCache.keySet()))
									if (mkey.isSameMethod(restriction)) {
										Util.log(null, Log.WARN, "Removing " + mkey);
										mRestrictionCache.remove(mkey);
									}
							}
						}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	@Override
	public void setSettingList(List<PSetting> listSetting) throws RemoteException {
		int uid = -1;
		for (PSetting setting : listSetting)
			if (uid < 0)
				uid = setting.uid;
			else if (uid != setting.uid)
				throw new SecurityException();
		enforcePermission(uid);
		for (PSetting setting : listSetting)
			setSettingInternal(setting);
	}

	@Override
	public PSetting getSetting(PSetting setting) throws RemoteException {
		long start = System.currentTimeMillis();

		// Translate isolated uid
		setting.uid = getIsolatedUid(setting.uid);

		int userId = Util.getUserId(setting.uid);

		// Special case
		if (Meta.cTypeAccountHash.equals(setting.type))
			try {
				setting.type = Meta.cTypeAccount;
				setting.name = Util.sha1(setting.name);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		// Default result
		PSetting result = new PSetting(setting.uid, setting.type, setting.name, setting.value);

		// Disable strict mode
		ThreadPolicy oldPolicy = StrictMode.getThreadPolicy();
		ThreadPolicy newPolicy = new ThreadPolicy.Builder(oldPolicy).permitDiskReads().permitDiskWrites().build();
		StrictMode.setThreadPolicy(newPolicy);

		try {
			// No permissions enforced

			// Check cache
			CSetting key = new CSetting(setting.uid, setting.type, setting.name);
			synchronized (mSettingCache) {
				if (mSettingCache.containsKey(key)) {
					result.value = mSettingCache.get(key).getValue();
					if (result.value == null)
						result.value = setting.value; // default value
					return result;
				}
			}

			// No persmissions required
			SQLiteDatabase db = getDb();
			if (db == null)
				return result;

			// Fallback
			if (!PrivacyManager.cSettingMigrated.equals(setting.name)
					&& !getSettingBool(userId, PrivacyManager.cSettingMigrated, false)) {
				if (setting.uid == 0)
					result.value = PrivacyProvider.getSettingFallback(setting.name, null, false);
				if (result.value == null) {
					result.value = PrivacyProvider.getSettingFallback(
							String.format("%s.%d", setting.name, setting.uid), setting.value, false);
					return result;
				}
			}

			// Precompile statement when needed
			if (stmtGetSetting == null) {
				String sql = "SELECT value FROM " + cTableSetting + " WHERE uid=? AND type=? AND name=?";
				stmtGetSetting = db.compileStatement(sql);
			}

			// Execute statement
			boolean found = false;
			mLock.readLock().lock();
			try {
				db.beginTransaction();
				try {
					try {
						synchronized (stmtGetSetting) {
							stmtGetSetting.clearBindings();
							stmtGetSetting.bindLong(1, setting.uid);
							stmtGetSetting.bindString(2, setting.type);
							stmtGetSetting.bindString(3, setting.name);
							result.value = stmtGetSetting.simpleQueryForString();
							found = true;
						}
					} catch (SQLiteDoneException ignored) {
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} finally {
				mLock.readLock().unlock();
			}

			// Add to cache
			key.setValue(found ? result.value : null);
			synchronized (mSettingCache) {
				if (mSettingCache.containsKey(key))
					mSettingCache.remove(key);
				mSettingCache.put(key, key);
			}

			// Default value
			if (result.value == null)
				result.value = setting.value;

		} catch (SQLiteException ex) {
			notifyException(ex);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		} finally {
			StrictMode.setThreadPolicy(oldPolicy);
		}

		long ms = System.currentTimeMillis() - start;
		Util.log(null, ms < PrivacyManager.cWarnServiceDelayMs ? Log.INFO : Log.WARN,
				String.format("Get service %s %d ms", setting, ms));

		return result;
	}

	@Override
	public List<PSetting> getSettingList(PSetting selector) throws RemoteException {
		List<PSetting> listSetting = new ArrayList<PSetting>();
		try {
			enforcePermission(selector.uid);
			SQLiteDatabase db = getDb();
			if (db == null)
				return listSetting;

			mLock.readLock().lock();
			try {
				db.beginTransaction();
				try {
					Cursor cursor;
					if (selector.type == null)
						cursor = db.query(cTableSetting, new String[] { "type", "name", "value" }, "uid=?",
								new String[] { Integer.toString(selector.uid) }, null, null, null);
					else
						cursor = db.query(cTableSetting, new String[] { "type", "name", "value" }, "uid=? AND type=?",
								new String[] { Integer.toString(selector.uid), selector.type }, null, null, null);
					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (settings)");
					else
						try {
							while (cursor.moveToNext())
								listSetting.add(new PSetting(selector.uid, cursor.getString(0), cursor.getString(1),
										cursor.getString(2)));
						} finally {
							cursor.close();
						}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} finally {
				mLock.readLock().unlock();
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
		return listSetting;
	}

	@Override
	public void deleteSettings(int uid) throws RemoteException {
		try {
			enforcePermission(uid);
			SQLiteDatabase db = getDb();
			if (db == null)
				return;

			mLock.writeLock().lock();
			try {
				db.beginTransaction();
				try {
					db.delete(cTableSetting, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Settings deleted uid=" + uid);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} finally {
				mLock.writeLock().unlock();
			}

			// Clear cache
			synchronized (mSettingCache) {
				mSettingCache.clear();
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	@Override
	public void clear() throws RemoteException {
		try {
			enforcePermission(0);
			SQLiteDatabase db = getDb();
			SQLiteDatabase dbUsage = getDbUsage();
			if (db == null || dbUsage == null)
				return;

			mLock.writeLock().lock();
			try {
				db.beginTransaction();
				try {
					db.execSQL("DELETE FROM " + cTableRestriction);
					db.execSQL("DELETE FROM " + cTableSetting);
					Util.log(null, Log.WARN, "Database cleared");

					// Reset migrated
					ContentValues values = new ContentValues();
					values.put("uid", 0);
					values.put("type", "");
					values.put("name", PrivacyManager.cSettingMigrated);
					values.put("value", Boolean.toString(true));
					db.insertWithOnConflict(cTableSetting, null, values, SQLiteDatabase.CONFLICT_REPLACE);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} finally {
				mLock.writeLock().unlock();
			}

			// Clear caches
			synchronized (mRestrictionCache) {
				mRestrictionCache.clear();
			}
			synchronized (mSettingCache) {
				mSettingCache.clear();
			}
			synchronized (mAskedOnceCache) {
				mAskedOnceCache.clear();
			}
			Util.log(null, Log.WARN, "Caches cleared");

			mLockUsage.writeLock().lock();
			try {
				dbUsage.beginTransaction();
				try {
					dbUsage.execSQL("DELETE FROM " + cTableUsage);
					Util.log(null, Log.WARN, "Usage database cleared");

					dbUsage.setTransactionSuccessful();
				} finally {
					dbUsage.endTransaction();
				}
			} finally {
				mLockUsage.writeLock().unlock();
			}

		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	@Override
	public void flush() throws RemoteException {
		try {
			enforcePermission(0);
			synchronized (mRestrictionCache) {
				mRestrictionCache.clear();
			}
			synchronized (mAskedOnceCache) {
				mAskedOnceCache.clear();
			}
			synchronized (mSettingCache) {
				mSettingCache.clear();
			}
			Util.log(null, Log.WARN, "Service cache flushed");
		} catch (Throwable ex) {
			Util.bug(null, ex);
			throw new RemoteException(ex.toString());
		}
	}

	@Override
	public void dump(int uid) throws RemoteException {
		if (uid == 0) {

		} else {
			synchronized (mRestrictionCache) {
				for (CRestriction crestriction : mRestrictionCache.keySet())
					if (crestriction.getUid() == uid)
						Util.log(null, Log.WARN, "Dump crestriction=" + crestriction);
			}
			synchronized (mAskedOnceCache) {
				for (CRestriction crestriction : mAskedOnceCache.keySet())
					if (crestriction.getUid() == uid && !crestriction.isExpired())
						Util.log(null, Log.WARN, "Dump asked=" + crestriction);
			}
			synchronized (mSettingCache) {
				for (CSetting csetting : mSettingCache.keySet())
					if (csetting.getUid() == uid)
						Util.log(null, Log.WARN, "Dump csetting=" + csetting);
			}
		}
	}

	// Helper classes

	final class OnDemandResult {
		public boolean ondemand = false;
		public boolean once = false;
		public boolean whitelist = false;
	}

	final class OnDemandDialogHolder {
		public View dialog = null;
		public CountDownLatch latch = new CountDownLatch(1);
		public boolean reset = false;
	}

	// Helper methods

	private OnDemandResult onDemandDialog(final Hook hook, final PRestriction restriction, final PRestriction result) {
		final OnDemandResult oResult = new OnDemandResult();
		try {
			int userId = Util.getUserId(restriction.uid);

			// Check if application
			if (!PrivacyManager.isApplication(restriction.uid))
				if (!getSettingBool(userId, PrivacyManager.cSettingOnDemandSystem, false))
					return oResult;

			// Check for exceptions
			if (hook != null && !hook.canOnDemand())
				return oResult;
			if (!PrivacyManager.canRestrict(restriction.uid, getXUid(), restriction.restrictionName,
					restriction.methodName, false))
				return oResult;

			// Check if enabled
			if (!getSettingBool(userId, PrivacyManager.cSettingOnDemand, true))
				return oResult;
			if (!getSettingBool(restriction.uid, PrivacyManager.cSettingOnDemand, false))
				return oResult;

			// Check version
			String version = getSetting(new PSetting(userId, "", PrivacyManager.cSettingVersion, "0.0")).value;
			if (new Version(version).compareTo(new Version("2.1.5")) < 0)
				return oResult;

			// Get activity manager context
			final Context context = getContext();
			if (context == null)
				return oResult;

			long token = 0;
			try {
				token = Binder.clearCallingIdentity();

				// Get application info
				final ApplicationInfoEx appInfo = new ApplicationInfoEx(context, restriction.uid);

				// Check for system application
				if (appInfo.isSystem())
					if (new Version(version).compareTo(new Version("2.0.38")) < 0)
						return oResult;

				// Check if activity manager agrees
				if (!XActivityManagerService.canOnDemand())
					return oResult;

				// Check if activity manager locked
				if (isAMLocked(restriction.uid)) {
					Util.log(null, Log.WARN, "On demand locked " + restriction);
					return oResult;
				}

				// Go ask
				Util.log(null, Log.WARN, "On demand " + restriction);
				mOndemandSemaphore.acquireUninterruptibly();
				try {
					// Check if activity manager still agrees
					if (!XActivityManagerService.canOnDemand())
						return oResult;

					// Check if activity manager locked now
					if (isAMLocked(restriction.uid)) {
						Util.log(null, Log.WARN, "On demand acquired locked " + restriction);
						return oResult;
					}

					Util.log(null, Log.WARN, "On demanding " + restriction);

					// Check if method not asked before
					CRestriction mkey = new CRestriction(restriction, null);
					synchronized (mRestrictionCache) {
						if (mRestrictionCache.containsKey(mkey)) {
							CRestriction mrestriction = mRestrictionCache.get(mkey);
							if (mrestriction.asked) {
								Util.log(null, Log.WARN, "Already asked " + restriction);
								result.restricted = mrestriction.restricted;
								result.asked = true;
								return oResult;
							}
						}
					}

					// Check if category not asked before (once)
					CRestriction ckey = new CRestriction(restriction, null);
					ckey.setMethodName(null);
					synchronized (mAskedOnceCache) {
						if (mAskedOnceCache.containsKey(ckey)) {
							CRestriction carestriction = mAskedOnceCache.get(ckey);
							if (!carestriction.isExpired()) {
								Util.log(null, Log.WARN, "Already asked once category " + restriction);
								result.restricted = carestriction.restricted;
								result.asked = true;
								return oResult;
							}
						}
					}

					// Check if method not asked before once
					synchronized (mAskedOnceCache) {
						if (mAskedOnceCache.containsKey(mkey)) {
							CRestriction marestriction = mAskedOnceCache.get(mkey);
							if (!marestriction.isExpired()) {
								Util.log(null, Log.WARN, "Already asked once method " + restriction);
								result.restricted = marestriction.restricted;
								result.asked = true;
								return oResult;
							}
						}
					}

					// Check if whitelist not asked before
					if (restriction.extra != null && hook != null && hook.whitelist() != null) {
						CSetting skey = new CSetting(restriction.uid, hook.whitelist(), restriction.extra);
						synchronized (mSettingCache) {
							if (mSettingCache.containsKey(skey)) {
								String value = mSettingCache.get(skey).getValue();
								if (value != null) {
									Util.log(null, Log.WARN, "Already asked whitelist " + skey);
									result.restricted = Boolean.parseBoolean(value);
									result.asked = true;
									return oResult;
								}
							}
							for (String xextra : getXExtra(restriction, hook)) {
								CSetting xkey = new CSetting(restriction.uid, hook.whitelist(), xextra);
								if (mSettingCache.containsKey(xkey)) {
									String value = mSettingCache.get(xkey).getValue();
									if (value != null) {
										Util.log(null, Log.WARN, "Already asked whitelist " + xkey);
										result.restricted = Boolean.parseBoolean(value);
										result.asked = true;
										return oResult;
									}
								}
							}
						}
					}

					final OnDemandDialogHolder holder = new OnDemandDialogHolder();

					// Build dialog parameters
					final WindowManager.LayoutParams params = new WindowManager.LayoutParams();
					params.type = WindowManager.LayoutParams.TYPE_PHONE;
					params.flags = WindowManager.LayoutParams.FLAG_DIM_BEHIND;
					params.systemUiVisibility = View.SYSTEM_UI_FLAG_LOW_PROFILE;
					params.dimAmount = 0.85f;
					params.width = WindowManager.LayoutParams.WRAP_CONTENT;
					params.height = WindowManager.LayoutParams.WRAP_CONTENT;
					params.format = PixelFormat.TRANSLUCENT;
					params.softInputMode = WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_HIDDEN;
					params.gravity = Gravity.CENTER;

					// Get window manager
					final WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);

					// Show dialog
					mHandler.post(new Runnable() {
						@Override
						public void run() {
							try {
								// Build dialog
								holder.dialog = getOnDemandView(restriction, hook, appInfo, result, context, holder,
										oResult);

								// Handle reset button
								((Button) holder.dialog.findViewById(R.id.btnReset))
										.setOnClickListener(new View.OnClickListener() {
											@Override
											public void onClick(View view) {
												((ProgressBar) holder.dialog.findViewById(R.id.pbProgress))
														.setProgress(cMaxOnDemandDialog * 20);
												holder.reset = true;
												holder.latch.countDown();
											}
										});

								// Make dialog visible
								wm.addView(holder.dialog, params);

								// Update progress bar
								Runnable runProgress = new Runnable() {
									@Override
									public void run() {
										if (holder.dialog != null && holder.dialog.isShown()) {
											// Update progress bar
											ProgressBar progressBar = (ProgressBar) holder.dialog
													.findViewById(R.id.pbProgress);
											if (progressBar.getProgress() > 0) {
												progressBar.incrementProgressBy(-1);
												mHandler.postDelayed(this, 50);
											}

											// Check if activity manager locked
											if (isAMLocked(restriction.uid)) {
												Util.log(null, Log.WARN, "On demand dialog locked " + restriction);
												((Button) holder.dialog.findViewById(R.id.btnDontKnow)).callOnClick();
											}
										}
									}
								};
								mHandler.postDelayed(runProgress, 50);

								// Enabled buttons after one second
								boolean repeat = (SystemClock.elapsedRealtime() - mOnDemandLastAnswer < 1000);
								mHandler.postDelayed(new Runnable() {
									@Override
									public void run() {
										if (holder.dialog != null && holder.dialog.isShown()) {
											holder.dialog.findViewById(R.id.btnAllow).setEnabled(true);
											holder.dialog.findViewById(R.id.btnDontKnow).setEnabled(true);
											holder.dialog.findViewById(R.id.btnDeny).setEnabled(true);
										}
									}
								}, repeat ? 0 : 1000);

							} catch (NameNotFoundException ex) {
								Util.log(null, Log.WARN, ex.toString());
							} catch (Throwable ex) {
								Util.bug(null, ex);
							}
						}
					});

					// Wait for choice, reset or timeout
					do {
						holder.reset = false;
						boolean choice = holder.latch.await(cMaxOnDemandDialog, TimeUnit.SECONDS);
						if (holder.reset) {
							holder.latch = new CountDownLatch(1);
							Util.log(null, Log.WARN, "On demand reset " + restriction);
						} else if (choice)
							oResult.ondemand = true;
						else
							Util.log(null, Log.WARN, "On demand timeout " + restriction);
					} while (holder.reset);
					mOnDemandLastAnswer = SystemClock.elapsedRealtime();

					// Dismiss dialog
					mHandler.post(new Runnable() {
						@Override
						public void run() {
							View dialog = holder.dialog;
							if (dialog != null)
								wm.removeView(dialog);
						}
					});

				} finally {
					mOndemandSemaphore.release();
				}
			} finally {
				Binder.restoreCallingIdentity(token);
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		return oResult;
	}

	@SuppressLint("InflateParams")
	private View getOnDemandView(final PRestriction restriction, final Hook hook, ApplicationInfoEx appInfo,
			final PRestriction result, Context context, final OnDemandDialogHolder holder, final OnDemandResult oResult)
			throws NameNotFoundException, RemoteException {
		// Get resources
		String self = PrivacyService.class.getPackage().getName();
		Resources resources = context.getPackageManager().getResourcesForApplication(self);

		// Reference views
		final View view = LayoutInflater.from(context.createPackageContext(self, 0)).inflate(R.layout.ondemand, null);
		ImageView ivAppIcon = (ImageView) view.findViewById(R.id.ivAppIcon);
		TextView tvUid = (TextView) view.findViewById(R.id.tvUid);
		TextView tvAppName = (TextView) view.findViewById(R.id.tvAppName);
		TextView tvCategory = (TextView) view.findViewById(R.id.tvCategory);
		TextView tvFunction = (TextView) view.findViewById(R.id.tvFunction);
		TextView tvParameters = (TextView) view.findViewById(R.id.tvParameters);
		TableRow rowParameters = (TableRow) view.findViewById(R.id.rowParameters);
		TextView tvDefault = (TextView) view.findViewById(R.id.tvDefault);
		TextView tvInfoCategory = (TextView) view.findViewById(R.id.tvInfoCategory);
		final CheckBox cbExpert = (CheckBox) view.findViewById(R.id.cbExpert);
		final CheckBox cbCategory = (CheckBox) view.findViewById(R.id.cbCategory);
		final CheckBox cbOnce = (CheckBox) view.findViewById(R.id.cbOnce);
		final Spinner spOnce = (Spinner) view.findViewById(R.id.spOnce);
		final LinearLayout llWhiteList = (LinearLayout) view.findViewById(R.id.llWhiteList);
		final CheckBox cbWhitelist = (CheckBox) view.findViewById(R.id.cbWhitelist);
		final CheckBox cbWhitelistExtra1 = (CheckBox) view.findViewById(R.id.cbWhitelistExtra1);
		final CheckBox cbWhitelistExtra2 = (CheckBox) view.findViewById(R.id.cbWhitelistExtra2);
		final CheckBox cbWhitelistExtra3 = (CheckBox) view.findViewById(R.id.cbWhitelistExtra3);
		ProgressBar mProgress = (ProgressBar) view.findViewById(R.id.pbProgress);
		Button btnDeny = (Button) view.findViewById(R.id.btnDeny);
		Button btnDontKnow = (Button) view.findViewById(R.id.btnDontKnow);
		Button btnAllow = (Button) view.findViewById(R.id.btnAllow);

		final int userId = Util.getUserId(Process.myUid());
		boolean expert = getSettingBool(userId, PrivacyManager.cSettingODExpert, false);
		boolean category = getSettingBool(userId, PrivacyManager.cSettingODCategory, true);
		boolean once = getSettingBool(userId, PrivacyManager.cSettingODOnce, false);
		expert = expert || !category || once;
		final boolean whitelistDangerous = (hook != null && hook.isDangerous() && hook.whitelist() != null);

		// Set values
		if ((hook != null && hook.isDangerous()) || appInfo.isSystem())
			view.setBackgroundResource(R.color.color_dangerous_dialog);
		else
			view.setBackgroundResource(android.R.color.background_dark);

		// Application information
		ivAppIcon.setImageDrawable(appInfo.getIcon(context));
		tvUid.setText(Integer.toString(appInfo.getUid()));
		tvAppName.setText(TextUtils.join(", ", appInfo.getApplicationName()));

		// Restriction information
		int catId = resources.getIdentifier("restrict_" + restriction.restrictionName, "string", self);
		tvCategory.setText(resources.getString(catId));
		tvFunction.setText(restriction.methodName);
		if (restriction.extra == null)
			rowParameters.setVisibility(View.GONE);
		else
			tvParameters.setText(restriction.extra);
		String defaultAction = resources.getString(result.restricted ? R.string.title_deny : R.string.title_allow);
		tvDefault.setText(defaultAction);

		// Help
		int helpId = resources.getIdentifier("restrict_help_" + restriction.restrictionName, "string", self);
		tvInfoCategory.setText(resources.getString(helpId));

		// Expert mode
		cbExpert.setChecked(expert);
		if (expert) {
			for (View child : Util.getViewsByTag((ViewGroup) view, "details"))
				child.setVisibility(View.VISIBLE);
			for (View child : Util.getViewsByTag((ViewGroup) view, "nodetails"))
				child.setVisibility(View.GONE);
		}
		if (expert || whitelistDangerous)
			llWhiteList.setVisibility(View.VISIBLE);

		// Category
		cbCategory.setChecked(category);

		// Once
		cbOnce.setChecked(once);
		int osel = Integer
				.parseInt(getSetting(new PSetting(userId, "", PrivacyManager.cSettingODOnceDuration, "0")).value);
		spOnce.setSelection(osel);

		// Whitelisting
		if (hook != null && hook.whitelist() != null && restriction.extra != null) {
			cbWhitelist.setText(resources.getString(R.string.title_whitelist, restriction.extra));
			cbWhitelist.setVisibility(View.VISIBLE);
			String[] xextra = getXExtra(restriction, hook);
			if (xextra.length > 0) {
				cbWhitelistExtra1.setText(resources.getString(R.string.title_whitelist, xextra[0]));
				cbWhitelistExtra1.setVisibility(View.VISIBLE);
			}
			if (xextra.length > 1) {
				cbWhitelistExtra2.setText(resources.getString(R.string.title_whitelist, xextra[1]));
				cbWhitelistExtra2.setVisibility(View.VISIBLE);
			}
			if (xextra.length > 2) {
				cbWhitelistExtra3.setText(resources.getString(R.string.title_whitelist, xextra[2]));
				cbWhitelistExtra3.setVisibility(View.VISIBLE);
			}
		}

		cbExpert.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				setSettingBool(userId, "", PrivacyManager.cSettingODExpert, isChecked);
				if (!isChecked) {
					setSettingBool(userId, "", PrivacyManager.cSettingODCategory, true);
					setSettingBool(userId, "", PrivacyManager.cSettingODOnce, false);
					cbCategory.setChecked(true);
					cbOnce.setChecked(false);
					cbWhitelist.setChecked(false);
					cbWhitelistExtra1.setChecked(false);
					cbWhitelistExtra2.setChecked(false);
					cbWhitelistExtra3.setChecked(false);
				}

				for (View child : Util.getViewsByTag((ViewGroup) view, "details"))
					child.setVisibility(isChecked ? View.VISIBLE : View.GONE);
				for (View child : Util.getViewsByTag((ViewGroup) view, "nodetails"))
					child.setVisibility(isChecked ? View.GONE : View.VISIBLE);

				if (!whitelistDangerous)
					llWhiteList.setVisibility(isChecked ? View.VISIBLE : View.GONE);
			}
		});

		// Category, once and whitelist exclude each other
		cbCategory.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				if (isChecked) {
					cbWhitelist.setChecked(false);
					cbWhitelistExtra1.setChecked(false);
					cbWhitelistExtra2.setChecked(false);
					cbWhitelistExtra3.setChecked(false);
				}
			}
		});
		cbOnce.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				if (isChecked) {
					cbWhitelist.setChecked(false);
					cbWhitelistExtra1.setChecked(false);
					cbWhitelistExtra2.setChecked(false);
					cbWhitelistExtra3.setChecked(false);
				}
			}
		});
		cbWhitelist.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				if (isChecked) {
					cbCategory.setChecked(false);
					cbOnce.setChecked(false);
					cbWhitelistExtra1.setChecked(false);
					cbWhitelistExtra2.setChecked(false);
					cbWhitelistExtra3.setChecked(false);
				}
			}
		});
		cbWhitelistExtra1.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				if (isChecked) {
					cbCategory.setChecked(false);
					cbOnce.setChecked(false);
					cbWhitelist.setChecked(false);
					cbWhitelistExtra2.setChecked(false);
					cbWhitelistExtra3.setChecked(false);
				}
			}
		});
		cbWhitelistExtra2.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				if (isChecked) {
					cbCategory.setChecked(false);
					cbOnce.setChecked(false);
					cbWhitelist.setChecked(false);
					cbWhitelistExtra1.setChecked(false);
					cbWhitelistExtra3.setChecked(false);
				}
			}
		});
		cbWhitelistExtra3.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				if (isChecked) {
					cbCategory.setChecked(false);
					cbOnce.setChecked(false);
					cbWhitelist.setChecked(false);
					cbWhitelistExtra1.setChecked(false);
					cbWhitelistExtra2.setChecked(false);
				}
			}
		});

		// Setup progress bar
		mProgress.setMax(cMaxOnDemandDialog * 20);
		mProgress.setProgress(cMaxOnDemandDialog * 20);

		btnAllow.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				// Allow
				result.restricted = false;
				result.asked = true;

				if (cbWhitelist.isChecked())
					onDemandWhitelist(restriction, null, result, oResult, hook);
				else if (cbWhitelistExtra1.isChecked())
					onDemandWhitelist(restriction, getXExtra(restriction, hook)[0], result, oResult, hook);
				else if (cbWhitelistExtra2.isChecked())
					onDemandWhitelist(restriction, getXExtra(restriction, hook)[1], result, oResult, hook);
				else if (cbWhitelistExtra3.isChecked())
					onDemandWhitelist(restriction, getXExtra(restriction, hook)[2], result, oResult, hook);
				else {
					setSettingBool(userId, "", PrivacyManager.cSettingODCategory, cbCategory.isChecked());
					setSettingBool(userId, "", PrivacyManager.cSettingODOnce, cbOnce.isChecked());

					if (cbOnce.isChecked())
						onDemandOnce(restriction, cbCategory.isChecked(), result, oResult, spOnce);
					else
						onDemandChoice(restriction, cbCategory.isChecked(), false);
				}
				holder.latch.countDown();
			}
		});

		btnDontKnow.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				// Deny once
				result.restricted = !(hook != null && hook.isDangerous());
				result.asked = true;
				onDemandOnce(restriction, false, result, oResult, spOnce);
				holder.latch.countDown();
			}
		});

		btnDeny.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				// Deny
				result.restricted = true;
				result.asked = true;

				if (cbWhitelist.isChecked())
					onDemandWhitelist(restriction, null, result, oResult, hook);
				else if (cbWhitelistExtra1.isChecked())
					onDemandWhitelist(restriction, getXExtra(restriction, hook)[0], result, oResult, hook);
				else if (cbWhitelistExtra2.isChecked())
					onDemandWhitelist(restriction, getXExtra(restriction, hook)[1], result, oResult, hook);
				else if (cbWhitelistExtra3.isChecked())
					onDemandWhitelist(restriction, getXExtra(restriction, hook)[2], result, oResult, hook);
				else {
					setSettingBool(userId, "", PrivacyManager.cSettingODCategory, cbCategory.isChecked());
					setSettingBool(userId, "", PrivacyManager.cSettingODOnce, cbOnce.isChecked());

					if (cbOnce.isChecked())
						onDemandOnce(restriction, cbCategory.isChecked(), result, oResult, spOnce);
					else
						onDemandChoice(restriction, cbCategory.isChecked(), true);
				}
				holder.latch.countDown();
			}
		});

		return view;
	}

	private String[] getXExtra(PRestriction restriction, Hook hook) {
		List<String> listResult = new ArrayList<String>();
		if (restriction.extra != null && hook != null)
			if (hook.whitelist().equals(Meta.cTypeFilename)) {
				// Top folders of file name
				File file = new File(restriction.extra);
				for (int i = 1; i <= 3 && file != null; i++) {
					String parent = file.getParent();
					if (!TextUtils.isEmpty(parent))
						listResult.add(parent + File.separatorChar + "*");
					file = file.getParentFile();
				}

			} else if (hook.whitelist().equals(Meta.cTypeIPAddress)) {
				// sub-domain or sub-net
				int colon = restriction.extra.lastIndexOf(':');
				String address = (colon >= 0 ? restriction.extra.substring(0, colon) : restriction.extra);
				if (Patterns.IP_ADDRESS.matcher(address).matches()) {
					int dot = address.lastIndexOf('.');
					listResult.add(address.substring(0, dot) + ".*"
							+ (colon >= 0 ? restriction.extra.substring(colon) : ""));
					if (colon >= 0)
						listResult.add(address.substring(0, dot) + ".*:*");
				} else {
					int dot = restriction.extra.indexOf('.');
					if (dot > 0) {
						listResult.add('*' + restriction.extra.substring(dot));
						if (colon >= 0)
							listResult.add('*' + restriction.extra.substring(dot, colon) + ":*");
					}
				}

			} else if (hook.whitelist().equals(Meta.cTypeUrl)) {
				// Top folders of file name
				Uri uri = Uri.parse(restriction.extra);
				if ("file".equals(uri.getScheme())) {
					File file = new File(uri.getPath());
					for (int i = 1; i <= 3 && file != null; i++) {
						String parent = file.getParent();
						if (!TextUtils.isEmpty(parent))
							listResult.add(parent + File.separatorChar + "*");
						file = file.getParentFile();
					}
				}

			} else if (hook.whitelist().equals(Meta.cTypeMethod) || hook.whitelist().equals(Meta.cTypeTransaction)
					|| hook.whitelist().equals(Meta.cTypeAction)) {
				String[] component = restriction.extra.split(":");
				if (component.length == 2)
					listResult.add(component[0] + ":*");
			}

		return listResult.toArray(new String[0]);
	}

	private void onDemandWhitelist(PRestriction restriction, String xextra, PRestriction result,
			OnDemandResult oResult, Hook hook) {
		try {
			Util.log(null, Log.WARN, (result.restricted ? "Black" : "White") + "listing " + restriction + " xextra="
					+ xextra);

			oResult.whitelist = true;

			// Set white/black list
			setSettingInternal(new PSetting(restriction.uid, hook.whitelist(), (xextra == null ? restriction.extra
					: xextra), Boolean.toString(!result.restricted)));

			if (!PrivacyManager.getSettingBool(restriction.uid, PrivacyManager.cSettingWhitelistNoModify, false))
				if (restriction.methodName == null || !PrivacyManager.cMethodNoState.contains(restriction.methodName)) {
					// Mark state as changed
					setSettingInternal(new PSetting(restriction.uid, "", PrivacyManager.cSettingState,
							Integer.toString(ApplicationInfoEx.STATE_CHANGED)));

					// Update modification time
					setSettingInternal(new PSetting(restriction.uid, "", PrivacyManager.cSettingModifyTime,
							Long.toString(System.currentTimeMillis())));
				}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private void onDemandOnce(PRestriction restriction, boolean category, PRestriction result, OnDemandResult oResult,
			Spinner spOnce) {
		oResult.once = true;

		// Get duration
		String value = (String) spOnce.getSelectedItem();
		if (value == null)
			result.time = new Date().getTime() + PrivacyManager.cRestrictionCacheTimeoutMs;
		else {
			char unit = value.charAt(value.length() - 1);
			value = value.substring(0, value.length() - 1);
			if (unit == 's')
				result.time = new Date().getTime() + Integer.parseInt(value) * 1000;
			else if (unit == 'm')
				result.time = new Date().getTime() + Integer.parseInt(value) * 60 * 1000;
			else
				result.time = new Date().getTime() + PrivacyManager.cRestrictionCacheTimeoutMs;

			try {
				int userId = Util.getUserId(restriction.uid);
				String sel = Integer.toString(spOnce.getSelectedItemPosition());
				setSettingInternal(new PSetting(userId, "", PrivacyManager.cSettingODOnceDuration, sel));
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		Util.log(null, Log.WARN, (result.restricted ? "Deny" : "Allow") + " once " + restriction + " category="
				+ category + " until=" + new Date(result.time));

		CRestriction key = new CRestriction(result, null);
		key.setExpiry(result.time);
		if (category) {
			key.setMethodName(null);
			key.setExtra(null);
		}
		synchronized (mAskedOnceCache) {
			if (mAskedOnceCache.containsKey(key))
				mAskedOnceCache.remove(key);
			mAskedOnceCache.put(key, key);
		}
	}

	private void onDemandChoice(PRestriction restriction, boolean category, boolean restrict) {
		try {
			PRestriction result = new PRestriction(restriction);

			// Get current category restriction state
			boolean prevRestricted = false;
			CRestriction key = new CRestriction(restriction.uid, restriction.restrictionName, null, null);
			synchronized (mRestrictionCache) {
				if (mRestrictionCache.containsKey(key))
					prevRestricted = mRestrictionCache.get(key).restricted;
			}

			Util.log(null, Log.WARN, "On demand choice " + restriction + " category=" + category + " restrict="
					+ restrict + " prev=" + prevRestricted);

			if (category || (restrict && restrict != prevRestricted)) {
				// Set category restriction
				result.methodName = null;
				result.restricted = restrict;
				result.asked = category;
				setRestrictionInternal(result);

				// Clear category on change
				for (Hook hook : PrivacyManager.getHooks(restriction.restrictionName, null))
					if (!PrivacyManager.canRestrict(restriction.uid, getXUid(), restriction.restrictionName,
							hook.getName(), false)) {
						result.methodName = hook.getName();
						result.restricted = false;
						result.asked = true;
						setRestrictionInternal(result);
					} else {
						result.methodName = hook.getName();
						result.restricted = restrict && !hook.isDangerous();
						result.asked = category || (hook.isDangerous() && hook.whitelist() == null);
						setRestrictionInternal(result);
					}
			}

			if (!category) {
				// Set method restriction
				result.methodName = restriction.methodName;
				result.restricted = restrict;
				result.asked = true;
				result.extra = restriction.extra;
				setRestrictionInternal(result);
			}

			// Mark state as changed
			setSettingInternal(new PSetting(restriction.uid, "", PrivacyManager.cSettingState,
					Integer.toString(ApplicationInfoEx.STATE_CHANGED)));

			// Update modification time
			setSettingInternal(new PSetting(restriction.uid, "", PrivacyManager.cSettingModifyTime,
					Long.toString(System.currentTimeMillis())));
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private void notifyRestricted(final PRestriction restriction) {
		final Context context = getContext();
		if (context != null && mHandler != null)
			mHandler.post(new Runnable() {
				@Override
				public void run() {
					long token = 0;
					try {
						token = Binder.clearCallingIdentity();

						// Get resources
						String self = PrivacyService.class.getPackage().getName();
						Resources resources = context.getPackageManager().getResourcesForApplication(self);

						// Notify user
						String text = resources.getString(R.string.msg_restrictedby);
						text += " (" + restriction.uid + " " + restriction.restrictionName + "/"
								+ restriction.methodName + ")";
						Toast.makeText(context, text, Toast.LENGTH_LONG).show();

					} catch (NameNotFoundException ex) {
						Util.bug(null, ex);
					} finally {
						Binder.restoreCallingIdentity(token);
					}
				}
			});
	}

	private void notifyException(Throwable ex) {
		Util.bug(null, ex);

		if (mNotified)
			return;

		Context context = getContext();
		if (context == null)
			return;

		try {
			Intent intent = new Intent("biz.bokhorst.xprivacy.action.EXCEPTION");
			intent.putExtra("Message", ex.toString());
			context.sendBroadcast(intent);

			NotificationManager notificationManager = (NotificationManager) context
					.getSystemService(Context.NOTIFICATION_SERVICE);

			// Build notification
			NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
			notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
			notificationBuilder.setContentTitle(context.getString(R.string.app_name));
			notificationBuilder.setContentText(ex.toString());
			notificationBuilder.setWhen(System.currentTimeMillis());
			notificationBuilder.setAutoCancel(true);
			Notification notification = notificationBuilder.build();

			// Display notification
			notificationManager.notify(Util.NOTIFY_CORRUPT, notification);

			mNotified = true;
		} catch (Throwable exex) {
			Util.bug(null, exex);
		}
	}

	private boolean getSettingBool(int uid, String name, boolean defaultValue) throws RemoteException {
		return getSettingBool(uid, "", name, defaultValue);
	}

	private boolean getSettingBool(int uid, String type, String name, boolean defaultValue) throws RemoteException {
		String value = getSetting(new PSetting(uid, type, name, Boolean.toString(defaultValue))).value;
		return Boolean.parseBoolean(value);
	}

	private void setSettingBool(int uid, String type, String name, boolean value) {
		try {
			setSettingInternal(new PSetting(uid, type, name, Boolean.toString(value)));
		} catch (RemoteException ex) {
			Util.bug(null, ex);
		}
	}

	private void enforcePermission(int uid) {
		if (uid >= 0)
			if (Util.getUserId(uid) != Util.getUserId(Binder.getCallingUid()))
				throw new SecurityException("uid=" + uid + " calling=" + Binder.getCallingUid());

		int callingUid = Util.getAppId(Binder.getCallingUid());
		if (callingUid != getXUid() && callingUid != Process.SYSTEM_UID)
			throw new SecurityException("xuid=" + mXUid + " calling=" + Binder.getCallingUid());
	}

	private boolean isAMLocked(int uid) {
		try {
			Object am;
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
				am = mAm;
			else {
				Class<?> cam = Class.forName("com.android.server.am.ActivityManagerService");
				am = cam.getMethod("self").invoke(null);
			}
			boolean locked = Thread.holdsLock(am);
			if (locked) {
				boolean freeze = getSettingBool(uid, PrivacyManager.cSettingFreeze, false);
				if (!freeze)
					freeze = getSettingBool(0, PrivacyManager.cSettingFreeze, false);
				if (freeze)
					return false;
			}
			return locked;
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return false;
		}
	}

	private Context getContext() {
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
			return mContext;
		else {
			// public static ActivityManagerService self()
			// frameworks/base/services/java/com/android/server/am/ActivityManagerService.java
			try {
				Class<?> cam = Class.forName("com.android.server.am.ActivityManagerService");
				Object am = cam.getMethod("self").invoke(null);
				if (am == null)
					return null;
				Field mContext = cam.getDeclaredField("mContext");
				mContext.setAccessible(true);
				return (Context) mContext.get(am);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return null;
			}
		}
	}

	private int getIsolatedUid(int uid) {
		if (PrivacyManager.isIsolated(uid))
			try {
				Field fmIsolatedProcesses = null;
				Class<?> cam = mAm.getClass();
				while (cam != null && fmIsolatedProcesses == null)
					try {
						fmIsolatedProcesses = cam.getDeclaredField("mIsolatedProcesses");
					} catch (NoSuchFieldException ignored) {
						cam = cam.getSuperclass();
					}

				if (fmIsolatedProcesses == null)
					throw new Exception(mAm.getClass().getName() + ".mIsolatedProcesses not found");

				fmIsolatedProcesses.setAccessible(true);
				SparseArray<?> mIsolatedProcesses = (SparseArray<?>) fmIsolatedProcesses.get(mAm);
				Object processRecord = mIsolatedProcesses.get(uid);
				Field fInfo = processRecord.getClass().getDeclaredField("info");
				fInfo.setAccessible(true);
				ApplicationInfo info = (ApplicationInfo) fInfo.get(processRecord);

				Util.log(null, Log.WARN, "Translated isolated uid=" + uid + " into application uid=" + info.uid
						+ " pkg=" + info.packageName);
				return info.uid;
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		return uid;
	}

	private int getXUid() {
		if (mXUid < 0)
			try {
				Context context = getContext();
				if (context != null) {
					PackageManager pm = context.getPackageManager();
					if (pm != null) {
						String self = PrivacyService.class.getPackage().getName();
						ApplicationInfo xInfo = pm.getApplicationInfo(self, 0);
						mXUid = xInfo.uid;
					}
				}
			} catch (Throwable ignored) {
				// The package manager may not be up-to-date yet
			}
		return mXUid;
	}

	private File getDbFile() {
		return new File(Environment.getDataDirectory() + File.separator + "system" + File.separator + "xprivacy"
				+ File.separator + "xprivacy.db");
	}

	private File getDbUsageFile() {
		return new File(Environment.getDataDirectory() + File.separator + "system" + File.separator + "xprivacy"
				+ File.separator + "usage.db");
	}

	private void setupDatabase() {
		try {
			File dbFile = getDbFile();

			// Create database folder
			dbFile.getParentFile().mkdirs();

			// Check database folder
			if (dbFile.getParentFile().isDirectory())
				Util.log(null, Log.WARN, "Database folder=" + dbFile.getParentFile());
			else
				Util.log(null, Log.WARN, "Does not exist folder=" + dbFile.getParentFile());

			if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {
				// Move database from data/xprivacy folder
				File folder = new File(Environment.getDataDirectory() + File.separator + "xprivacy");
				if (folder.exists()) {
					File[] oldFiles = folder.listFiles();
					if (oldFiles != null)
						for (File file : oldFiles)
							if (file.getName().startsWith("xprivacy.db") || file.getName().startsWith("usage.db")) {
								File target = new File(dbFile.getParentFile() + File.separator + file.getName());
								boolean status = Util.move(file, target);
								Util.log(null, Log.WARN, "Moved " + file + " to " + target + " ok=" + status);
							}
					Util.log(null, Log.WARN, "Deleting folder=" + folder);
					folder.delete();
				}

				// Move database from data/application folder
				folder = new File(Environment.getDataDirectory() + File.separator + "data" + File.separator
						+ PrivacyService.class.getPackage().getName());
				if (folder.exists()) {
					File[] oldFiles = folder.listFiles();
					if (oldFiles != null)
						for (File file : oldFiles)
							if (file.getName().startsWith("xprivacy.db")) {
								File target = new File(dbFile.getParentFile() + File.separator + file.getName());
								boolean status = Util.move(file, target);
								Util.log(null, Log.WARN, "Moved " + file + " to " + target + " ok=" + status);
							}
					Util.log(null, Log.WARN, "Deleting folder=" + folder);
					folder.delete();
				}
			}

			// Set database file permissions
			// Owner: rwx (system)
			// Group: rwx (system)
			// World: ---
			Util.setPermissions(dbFile.getParentFile().getAbsolutePath(), 0770, Process.SYSTEM_UID, Process.SYSTEM_UID);
			File[] files = dbFile.getParentFile().listFiles();
			if (files != null)
				for (File file : files)
					if (file.getName().startsWith("xprivacy.db") || file.getName().startsWith("usage.db"))
						Util.setPermissions(file.getAbsolutePath(), 0770, Process.SYSTEM_UID, Process.SYSTEM_UID);

		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private SQLiteDatabase getDb() {
		synchronized (this) {
			// Check current reference
			if (mDb != null && !mDb.isOpen()) {
				mDb = null;
				Util.log(null, Log.ERROR, "Database not open");
			}

			if (mDb == null)
				try {
					setupDatabase();

					// Create/upgrade database when needed
					File dbFile = getDbFile();
					SQLiteDatabase db = SQLiteDatabase.openOrCreateDatabase(dbFile, null);

					// Check database integrity
					if (db.isDatabaseIntegrityOk())
						Util.log(null, Log.WARN, "Database integrity ok");
					else {
						// http://www.sqlite.org/howtocorrupt.html
						mCorrupt = true;
						Util.log(null, Log.ERROR, "Database corrupt");
						Cursor cursor = db.rawQuery("PRAGMA integrity_check", null);
						try {
							while (cursor.moveToNext()) {
								String message = cursor.getString(0);
								Util.log(null, Log.ERROR, message);
							}
						} finally {
							cursor.close();
						}
						db.close();

						// Backup database file
						File dbBackup = new File(dbFile.getParentFile() + File.separator + "xprivacy.backup");
						dbBackup.delete();
						dbFile.renameTo(dbBackup);

						File dbJournal = new File(dbFile.getAbsolutePath() + "-journal");
						File dbJournalBackup = new File(dbBackup.getAbsolutePath() + "-journal");
						dbJournalBackup.delete();
						dbJournal.renameTo(dbJournalBackup);

						Util.log(null, Log.ERROR, "Old database backup: " + dbBackup.getAbsolutePath());

						// Create new database
						db = SQLiteDatabase.openOrCreateDatabase(dbFile, null);
						Util.log(null, Log.ERROR, "New, empty database created");
					}

					// Update migration status
					if (db.getVersion() > 1) {
						Util.log(null, Log.WARN, "Updating migration status");
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								ContentValues values = new ContentValues();
								values.put("uid", 0);
								if (db.getVersion() > 9)
									values.put("type", "");
								values.put("name", PrivacyManager.cSettingMigrated);
								values.put("value", Boolean.toString(true));
								db.insertWithOnConflict(cTableSetting, null, values, SQLiteDatabase.CONFLICT_REPLACE);

								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					// Upgrade database if needed
					if (db.needUpgrade(1)) {
						Util.log(null, Log.WARN, "Creating database");
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								// http://www.sqlite.org/lang_createtable.html
								db.execSQL("CREATE TABLE restriction (uid INTEGER NOT NULL, restriction TEXT NOT NULL, method TEXT NOT NULL, restricted INTEGER NOT NULL)");
								db.execSQL("CREATE TABLE setting (uid INTEGER NOT NULL, name TEXT NOT NULL, value TEXT)");
								db.execSQL("CREATE TABLE usage (uid INTEGER NOT NULL, restriction TEXT NOT NULL, method TEXT NOT NULL, restricted INTEGER NOT NULL, time INTEGER NOT NULL)");
								db.execSQL("CREATE UNIQUE INDEX idx_restriction ON restriction(uid, restriction, method)");
								db.execSQL("CREATE UNIQUE INDEX idx_setting ON setting(uid, name)");
								db.execSQL("CREATE UNIQUE INDEX idx_usage ON usage(uid, restriction, method)");
								db.setVersion(1);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}

					}

					if (db.needUpgrade(2)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						// Old migrated indication
						db.setVersion(2);
					}

					if (db.needUpgrade(3)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								db.execSQL("DELETE FROM usage WHERE method=''");
								db.setVersion(3);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					if (db.needUpgrade(4)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								db.execSQL("DELETE FROM setting WHERE value IS NULL");
								db.setVersion(4);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					if (db.needUpgrade(5)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								db.execSQL("DELETE FROM setting WHERE value = ''");
								db.execSQL("DELETE FROM setting WHERE name = 'Random@boot' AND value = 'false'");
								db.setVersion(5);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					if (db.needUpgrade(6)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								db.execSQL("DELETE FROM setting WHERE name LIKE 'OnDemand.%'");
								db.setVersion(6);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					if (db.needUpgrade(7)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								db.execSQL("ALTER TABLE usage ADD COLUMN extra TEXT");
								db.setVersion(7);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					if (db.needUpgrade(8)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								db.execSQL("DROP INDEX idx_usage");
								db.execSQL("CREATE UNIQUE INDEX idx_usage ON usage(uid, restriction, method, extra)");
								db.setVersion(8);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					if (db.needUpgrade(9)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								db.execSQL("DROP TABLE usage");
								db.setVersion(9);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					if (db.needUpgrade(10)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								db.execSQL("ALTER TABLE setting ADD COLUMN type TEXT");
								db.execSQL("DROP INDEX idx_setting");
								db.execSQL("CREATE UNIQUE INDEX idx_setting ON setting(uid, type, name)");
								db.execSQL("UPDATE setting SET type=''");
								db.setVersion(10);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					if (db.needUpgrade(11)) {
						Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
						mLock.writeLock().lock();
						try {
							db.beginTransaction();
							try {
								List<PSetting> listSetting = new ArrayList<PSetting>();
								Cursor cursor = db.query(cTableSetting, new String[] { "uid", "name", "value" }, null,
										null, null, null, null);
								if (cursor != null)
									try {
										while (cursor.moveToNext()) {
											int uid = cursor.getInt(0);
											String name = cursor.getString(1);
											String value = cursor.getString(2);
											if (name.startsWith("Account.") || name.startsWith("Application.")
													|| name.startsWith("Contact.") || name.startsWith("Template.")) {
												int dot = name.indexOf('.');
												String type = name.substring(0, dot);
												listSetting
														.add(new PSetting(uid, type, name.substring(dot + 1), value));
												listSetting.add(new PSetting(uid, "", name, null));

											} else if (name.startsWith("Whitelist.")) {
												String[] component = name.split("\\.");
												listSetting.add(new PSetting(uid, component[1], name.replace(
														component[0] + "." + component[1] + ".", ""), value));
												listSetting.add(new PSetting(uid, "", name, null));
											}
										}
									} finally {
										cursor.close();
									}

								for (PSetting setting : listSetting) {
									Util.log(null, Log.WARN, "Converting " + setting);
									if (setting.value == null)
										db.delete(cTableSetting, "uid=? AND type=? AND name=?",
												new String[] { Integer.toString(setting.uid), setting.type,
														setting.name });
									else {
										// Create record
										ContentValues values = new ContentValues();
										values.put("uid", setting.uid);
										values.put("type", setting.type);
										values.put("name", setting.name);
										values.put("value", setting.value);

										// Insert/update record
										db.insertWithOnConflict(cTableSetting, null, values,
												SQLiteDatabase.CONFLICT_REPLACE);
									}
								}

								db.setVersion(11);
								db.setTransactionSuccessful();
							} finally {
								db.endTransaction();
							}
						} finally {
							mLock.writeLock().unlock();
						}
					}

					Util.log(null, Log.WARN, "Running VACUUM");
					mLock.writeLock().lock();
					try {
						db.execSQL("VACUUM");
					} catch (Throwable ex) {
						Util.bug(null, ex);
					} finally {
						mLock.writeLock().unlock();
					}

					Util.log(null, Log.WARN, "Database version=" + db.getVersion());
					mDb = db;
				} catch (Throwable ex) {
					mDb = null; // retry
					Util.bug(null, ex);
					try {
						OutputStreamWriter outputStreamWriter = new OutputStreamWriter(new FileOutputStream(
								"/cache/xprivacy.log", true));
						outputStreamWriter.write(ex.toString());
						outputStreamWriter.write("\n");
						outputStreamWriter.write(Log.getStackTraceString(ex));
						outputStreamWriter.write("\n");
						outputStreamWriter.close();
					} catch (Throwable exex) {
						Util.bug(null, exex);
					}
				}

			return mDb;
		}
	}

	private SQLiteDatabase getDbUsage() {
		synchronized (this) {
			// Check current reference
			if (mDbUsage != null && !mDbUsage.isOpen()) {
				mDbUsage = null;
				Util.log(null, Log.ERROR, "Usage database not open");
			}

			if (mDbUsage == null)
				try {
					// Create/upgrade database when needed
					File dbUsageFile = getDbUsageFile();
					SQLiteDatabase dbUsage = SQLiteDatabase.openOrCreateDatabase(dbUsageFile, null);

					// Check database integrity
					if (dbUsage.isDatabaseIntegrityOk())
						Util.log(null, Log.WARN, "Usage database integrity ok");
					else {
						dbUsage.close();
						dbUsageFile.delete();
						new File(dbUsageFile + "-journal").delete();
						dbUsage = SQLiteDatabase.openOrCreateDatabase(dbUsageFile, null);
						Util.log(null, Log.WARN, "Deleted corrupt usage data database");
					}

					// Upgrade database if needed
					if (dbUsage.needUpgrade(1)) {
						Util.log(null, Log.WARN, "Creating usage database");
						mLockUsage.writeLock().lock();
						try {
							dbUsage.beginTransaction();
							try {
								dbUsage.execSQL("CREATE TABLE usage (uid INTEGER NOT NULL, restriction TEXT NOT NULL, method TEXT NOT NULL, extra TEXT NOT NULL, restricted INTEGER NOT NULL, time INTEGER NOT NULL)");
								dbUsage.execSQL("CREATE UNIQUE INDEX idx_usage ON usage(uid, restriction, method, extra)");
								dbUsage.setVersion(1);
								dbUsage.setTransactionSuccessful();
							} finally {
								dbUsage.endTransaction();
							}
						} finally {
							mLockUsage.writeLock().unlock();
						}
					}

					if (dbUsage.needUpgrade(2)) {
						Util.log(null, Log.WARN, "Upgrading usage database from version=" + dbUsage.getVersion());
						mLockUsage.writeLock().lock();
						try {
							dbUsage.beginTransaction();
							try {
								dbUsage.execSQL("ALTER TABLE usage ADD COLUMN value TEXT");
								dbUsage.setVersion(2);
								dbUsage.setTransactionSuccessful();
							} finally {
								dbUsage.endTransaction();
							}
						} finally {
							mLockUsage.writeLock().unlock();
						}
					}

					Util.log(null, Log.WARN, "Changing to asynchronous mode");
					try {
						dbUsage.rawQuery("PRAGMA synchronous=OFF", null);
					} catch (Throwable ex) {
						Util.bug(null, ex);
					}

					Util.log(null, Log.WARN, "Usage database version=" + dbUsage.getVersion());
					mDbUsage = dbUsage;
				} catch (Throwable ex) {
					mDbUsage = null; // retry
					Util.bug(null, ex);
				}

			return mDbUsage;
		}
	}
}

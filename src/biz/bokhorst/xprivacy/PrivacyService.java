package biz.bokhorst.xprivacy;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
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
import java.util.concurrent.locks.ReentrantReadWriteLock;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.content.ContentValues;
import android.content.Context;
import android.content.DialogInterface;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.Resources;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteDoneException;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteStatement;
import android.os.Binder;
import android.os.Build;
import android.os.Environment;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.Process;
import android.os.RemoteException;
import android.os.StrictMode;
import android.os.StrictMode.ThreadPolicy;
import android.text.TextUtils;
import android.util.Log;
import android.util.Patterns;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TableRow;
import android.widget.TextView;
import android.widget.Toast;

public class PrivacyService {
	private static int mXUid = -1;
	private static boolean mRegistered = false;
	private static boolean mUseCache = false;
	private static String mSecret = null;
	private static Thread mWorker = null;
	private static Handler mHandler = null;
	private static Semaphore mOndemandSemaphore = new Semaphore(1, true);
	private static List<String> mListError = new ArrayList<String>();
	private static IPrivacyService mClient = null;

	private static final String cTableRestriction = "restriction";
	private static final String cTableUsage = "usage";
	private static final String cTableSetting = "setting";

	private static final int cCurrentVersion = 317;
	private static final String cServiceName = "xprivacy305";

	// TODO: define column names
	// sqlite3 /data/system/xprivacy/xprivacy.db

	public static void register(List<String> listError, String secret) {
		// Store secret and errors
		mSecret = secret;
		mListError.addAll(listError);

		try {
			// Register privacy service
			// @formatter:off
			// public static void addService(String name, IBinder service)
			// public static void addService(String name, IBinder service, boolean allowIsolated)
			// @formatter:on
			Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
				Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class,
						boolean.class);
				mAddService.invoke(null, cServiceName, mPrivacyService, true);
			} else {
				Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class);
				mAddService.invoke(null, cServiceName, mPrivacyService);
			}

			// This will and should open the database
			mRegistered = true;
			Util.log(null, Log.WARN, "Service registered name=" + cServiceName);

			// Publish semaphore to activity manager service
			XActivityManagerService.setSemaphore(mOndemandSemaphore);

			// Get memory class to enable/disable caching
			// http://stackoverflow.com/questions/2630158/detect-application-heap-size-in-android
			int memoryClass = (int) (Runtime.getRuntime().maxMemory() / 1024L / 1024L);
			mUseCache = (memoryClass >= 32);
			Util.log(null, Log.WARN, "Memory class=" + memoryClass + " cache=" + mUseCache);

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
		return mRegistered;
	}

	public static boolean checkClient() {
		// Runs client side
		try {
			IPrivacyService client = getClient();
			if (client != null)
				return (client.getVersion() == cCurrentVersion);
		} catch (RemoteException ex) {
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
				mClient = IPrivacyService.Stub.asInterface((IBinder) mGetService.invoke(null, cServiceName));
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		// Disable disk/network strict mode
		// TODO: hook setThreadPolicy
		try {
			ThreadPolicy oldPolicy = StrictMode.getThreadPolicy();
			ThreadPolicy newpolicy = new ThreadPolicy.Builder(oldPolicy).permitDiskReads().permitDiskWrites()
					.permitNetwork().build();
			StrictMode.setThreadPolicy(newpolicy);
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

	public static PRestriction getRestriction(final PRestriction restriction, boolean usage, String secret)
			throws RemoteException {
		if (isRegistered())
			return mPrivacyService.getRestriction(restriction, usage, secret);
		else {
			IPrivacyService client = getClient();
			if (client == null) {
				Log.w("XPrivacy", "No client for " + restriction);
				Log.w("XPrivacy", Log.getStackTraceString(new Exception("StackTrace")));
				PRestriction result = new PRestriction(restriction);
				result.restricted = false;
				return result;
			} else
				return client.getRestriction(restriction, usage, secret);
		}
	}

	public static PSetting getSetting(PSetting setting) throws RemoteException {
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

	private static final IPrivacyService.Stub mPrivacyService = new IPrivacyService.Stub() {
		private SQLiteDatabase mDb = null;
		private SQLiteDatabase mDbUsage = null;
		private SQLiteStatement stmtGetRestriction = null;
		private SQLiteStatement stmtGetSetting = null;
		private SQLiteStatement stmtGetUsageRestriction = null;
		private SQLiteStatement stmtGetUsageMethod = null;
		private ReentrantReadWriteLock mLock = new ReentrantReadWriteLock(true);
		private ReentrantReadWriteLock mLockUsage = new ReentrantReadWriteLock(true);

		private boolean mSelectCategory = true;
		private boolean mSelectOnce = false;

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

		// Management

		@Override
		public int getVersion() throws RemoteException {
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
					if (c < 5000)
						listError.add(msg);
					else
						break;
					i++;
				}
			}

			File dbFile = getDbFile();
			if (!dbFile.exists())
				listError.add("Database does not exists");
			if (!dbFile.canRead())
				listError.add("Database not readable");
			if (!dbFile.canWrite())
				listError.add("Database not writable");

			SQLiteDatabase db = getDb();
			if (db == null)
				listError.add("Database not available");
			else if (!db.isOpen())
				listError.add("Database not open");

			return listError;
		}

		@Override
		public void reportError(String message) throws RemoteException {
			reportErrorInternal(message);
		}

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

				synchronized (mLock) {
					mLock.writeLock().lock();
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
						try {
							db.endTransaction();
						} finally {
							mLock.writeLock().unlock();
						}
					}
				}

				// Update cache
				if (mUseCache)
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
			boolean cached = false;
			int userId = Util.getUserId(restriction.uid);
			final PRestriction mresult = new PRestriction(restriction);

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

				// Check for self
				if (Util.getAppId(restriction.uid) == getXUid()) {
					if (PrivacyManager.cIdentification.equals(restriction.restrictionName)
							&& "getString".equals(restriction.methodName)) {
						mresult.asked = true;
						return mresult;
					}
					if (PrivacyManager.cIPC.equals(restriction.restrictionName)) {
						mresult.asked = true;
						return mresult;
					} else if (PrivacyManager.cStorage.equals(restriction.restrictionName)) {
						mresult.asked = true;
						return mresult;
					} else if (PrivacyManager.cSystem.equals(restriction.restrictionName)) {
						mresult.asked = true;
						return mresult;
					} else if (PrivacyManager.cView.equals(restriction.restrictionName)) {
						mresult.asked = true;
						return mresult;
					}
				}

				// Get meta data
				Hook hook = null;
				if (restriction.methodName != null) {
					hook = PrivacyManager.getHook(restriction.restrictionName, restriction.methodName);
					if (hook == null)
						// Can happen after replacing apk
						Util.log(null, Log.WARN, "Hook not found in service: " + restriction);
				}

				// Check for system component
				if (usage && !PrivacyManager.isApplication(restriction.uid))
					if (!getSettingBool(userId, PrivacyManager.cSettingSystem, false))
						return mresult;

				// Check if restrictions enabled
				if (usage && !getSettingBool(restriction.uid, PrivacyManager.cSettingRestricted, true))
					return mresult;

				// Check cache
				if (mUseCache) {
					CRestriction key = new CRestriction(restriction, restriction.extra);
					synchronized (mRestrictionCache) {
						if (mRestrictionCache.containsKey(key)) {
							cached = true;
							CRestriction cache = mRestrictionCache.get(key);
							mresult.restricted = cache.restricted;
							mresult.asked = cache.asked;
						}
					}
				}

				if (!cached) {
					PRestriction cresult = new PRestriction(restriction.uid, restriction.restrictionName, null);
					boolean methodFound = false;

					// No permissions required
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
					synchronized (mLock) {
						mLock.readLock().lock();
						db.beginTransaction();
						try {
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
							try {
								db.endTransaction();
							} finally {
								mLock.readLock().unlock();
							}
						}
					}

					// Default dangerous
					if (!methodFound && hook != null && hook.isDangerous())
						if (!getSettingBool(userId, PrivacyManager.cSettingDangerous, false)) {
							mresult.restricted = false;
							mresult.asked = true;
						}

					// Check whitelist
					if (usage && hook != null && hook.whitelist() != null && restriction.extra != null) {
						String value = getSetting(new PSetting(restriction.uid, hook.whitelist(), restriction.extra,
								null)).value;
						if (value == null) {
							String xextra = getXExtra(restriction, hook);
							if (xextra != null)
								value = getSetting(new PSetting(restriction.uid, hook.whitelist(), xextra, null)).value;
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
					if (mUseCache) {
						CRestriction key = new CRestriction(mresult, restriction.extra);
						synchronized (mRestrictionCache) {
							if (mRestrictionCache.containsKey(key))
								mRestrictionCache.remove(key);
							mRestrictionCache.put(key, key);
						}
					}
				}

				// Ask to restrict
				boolean ondemand = false;
				if (!mresult.asked && usage && PrivacyManager.isApplication(restriction.uid))
					ondemand = onDemandDialog(hook, restriction, mresult);

				// Notify user
				if (!ondemand && mresult.restricted && usage && hook != null && hook.shouldNotify())
					notifyRestricted(restriction);

				// Store usage data
				if (usage && hook != null && hook.hasUsageData())
					storeUsageData(restriction, secret, mresult);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

			long ms = System.currentTimeMillis() - start;
			Util.log(null, Log.INFO,
					String.format("get service %s%s %d ms", restriction, (cached ? " (cached)" : ""), ms));

			return mresult;
		}

		private void storeUsageData(final PRestriction restriction, String secret, final PRestriction mresult)
				throws RemoteException {
			// Check if enabled
			final int userId = Util.getUserId(restriction.uid);
			if (getSettingBool(userId, PrivacyManager.cSettingUsage, true)) {
				// Check secret
				boolean allowed = true;
				if (Util.getAppId(Binder.getCallingUid()) != getXUid()) {
					if (mSecret == null || !mSecret.equals(secret)) {
						allowed = false;
						Util.log(null, Log.WARN, "Invalid secret");
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

									String extra = "";
									if (restriction.extra != null)
										if (getSettingBool(userId, PrivacyManager.cSettingParameters, false))
											extra = restriction.extra;

									synchronized (mLockUsage) {
										mLockUsage.writeLock().lock();
										dbUsage.beginTransaction();
										try {
											ContentValues values = new ContentValues();
											values.put("uid", restriction.uid);
											values.put("restriction", restriction.restrictionName);
											values.put("method", restriction.methodName);
											values.put("restricted", mresult.restricted);
											values.put("time", new Date().getTime());
											values.put("extra", extra);
											dbUsage.insertWithOnConflict(cTableUsage, null, values,
													SQLiteDatabase.CONFLICT_REPLACE);

											dbUsage.setTransactionSuccessful();
										} finally {
											try {
												dbUsage.endTransaction();
											} finally {
												mLockUsage.writeLock().unlock();
											}
										}
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
					for (Hook md : PrivacyManager.getHooks(selector.restrictionName)) {
						PRestriction restriction = new PRestriction(selector.uid, selector.restrictionName,
								md.getName(), false);
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
		public void deleteRestrictions(int uid, String restrictionName) throws RemoteException {
			try {
				enforcePermission(uid);
				SQLiteDatabase db = getDb();
				if (db == null)
					return;

				synchronized (mLock) {
					mLock.writeLock().lock();
					db.beginTransaction();
					try {
						if ("".equals(restrictionName))
							db.delete(cTableRestriction, "uid=?", new String[] { Integer.toString(uid) });
						else
							db.delete(cTableRestriction, "uid=? AND restriction=?",
									new String[] { Integer.toString(uid), restrictionName });
						Util.log(null, Log.WARN, "Restrictions deleted uid=" + uid + " category=" + restrictionName);

						db.setTransactionSuccessful();
					} finally {
						try {
							db.endTransaction();
						} finally {
							mLock.writeLock().unlock();
						}
					}
				}

				// Clear caches
				if (mUseCache)
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

				synchronized (mLockUsage) {
					mLockUsage.readLock().lock();
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
						try {
							dbUsage.endTransaction();
						} finally {
							mLockUsage.readLock().unlock();
						}
					}
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

				synchronized (mLockUsage) {
					mLockUsage.readLock().lock();
					dbUsage.beginTransaction();
					try {
						String sFrom = Long.toString(new Date().getTime() - cMaxUsageDataHours * 60L * 60L * 1000L);
						Cursor cursor;
						if (uid == 0) {
							if ("".equals(restrictionName))
								cursor = dbUsage.query(cTableUsage, new String[] { "uid", "restriction", "method",
										"restricted", "time", "extra" }, "time>?", new String[] { sFrom }, null, null,
										"time DESC");
							else
								cursor = dbUsage.query(cTableUsage, new String[] { "uid", "restriction", "method",
										"restricted", "time", "extra" }, "restriction=? AND time>?", new String[] {
										restrictionName, sFrom }, null, null, "time DESC");
						} else {
							if ("".equals(restrictionName))
								cursor = dbUsage.query(cTableUsage, new String[] { "uid", "restriction", "method",
										"restricted", "time", "extra" }, "uid=? AND time>?",
										new String[] { Integer.toString(uid), sFrom }, null, null, "time DESC");
							else
								cursor = dbUsage.query(cTableUsage, new String[] { "uid", "restriction", "method",
										"restricted", "time", "extra" }, "uid=? AND restriction=? AND time>?",
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
									if (userId == 0 || Util.getUserId(data.uid) == userId)
										result.add(data);
								}
							} finally {
								cursor.close();
							}

						dbUsage.setTransactionSuccessful();
					} finally {
						try {
							dbUsage.endTransaction();
						} finally {
							mLockUsage.readLock().unlock();
						}
					}
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

				synchronized (mLockUsage) {
					mLockUsage.writeLock().lock();
					dbUsage.beginTransaction();
					try {
						if (uid == 0)
							dbUsage.delete(cTableUsage, null, new String[] {});
						else
							dbUsage.delete(cTableUsage, "uid=?", new String[] { Integer.toString(uid) });
						Util.log(null, Log.WARN, "Usage data deleted uid=" + uid);

						dbUsage.setTransactionSuccessful();
					} finally {
						try {
							dbUsage.endTransaction();
						} finally {
							mLockUsage.writeLock().unlock();
						}
					}
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

				synchronized (mLock) {
					mLock.writeLock().lock();
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
						try {
							db.endTransaction();
						} finally {
							mLock.writeLock().unlock();
						}
					}
				}

				// Update cache
				if (mUseCache) {
					CSetting key = new CSetting(setting.uid, setting.type, setting.name);
					key.setValue(setting.value);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							mSettingCache.remove(key);
						if (setting.value != null)
							mSettingCache.put(key, key);
					}
				}

				// Clear restrictions for white list
				if (Meta.isWhitelist(setting.type))
					for (String restrictionName : PrivacyManager.getRestrictions())
						for (Hook hook : PrivacyManager.getHooks(restrictionName))
							if (setting.type.equals(hook.whitelist())) {
								PRestriction restriction = new PRestriction(setting.uid, hook.getRestrictionName(),
										hook.getName());
								Util.log(null, Log.WARN, "Clearing cache for " + restriction);
								synchronized (mRestrictionCache) {
									for (CRestriction key : new ArrayList<CRestriction>(mRestrictionCache.keySet()))
										if (key.isSameMethod(restriction)) {
											Util.log(null, Log.WARN, "Removing " + key);
											mRestrictionCache.remove(key);
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
		@SuppressLint("DefaultLocale")
		public PSetting getSetting(PSetting setting) throws RemoteException {
			int userId = Util.getUserId(setting.uid);
			PSetting result = new PSetting(setting.uid, setting.type, setting.name, setting.value);

			try {
				// No permissions enforced

				// Check cache
				if (mUseCache && setting.value != null) {
					CSetting key = new CSetting(setting.uid, setting.type, setting.name);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key)) {
							result.value = mSettingCache.get(key).getValue();
							return result;
						}
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
				synchronized (mLock) {
					mLock.readLock().lock();
					db.beginTransaction();
					try {
						try {
							synchronized (stmtGetSetting) {
								stmtGetSetting.clearBindings();
								stmtGetSetting.bindLong(1, setting.uid);
								stmtGetSetting.bindString(2, setting.type);
								stmtGetSetting.bindString(3, setting.name);
								String value = stmtGetSetting.simpleQueryForString();
								if (value != null)
									result.value = value;
							}
						} catch (SQLiteDoneException ignored) {
						}

						db.setTransactionSuccessful();
					} finally {
						try {
							db.endTransaction();
						} finally {
							mLock.readLock().unlock();
						}
					}
				}

				// Add to cache
				if (mUseCache && result.value != null) {
					CSetting key = new CSetting(setting.uid, setting.type, setting.name);
					key.setValue(result.value);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							mSettingCache.remove(key);
						mSettingCache.put(key, key);
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return result;
		}

		@Override
		public List<PSetting> getSettingList(int uid) throws RemoteException {
			List<PSetting> listSetting = new ArrayList<PSetting>();
			try {
				enforcePermission(uid);
				SQLiteDatabase db = getDb();
				if (db == null)
					return listSetting;

				synchronized (mLock) {
					mLock.readLock().lock();
					db.beginTransaction();
					try {
						Cursor cursor = db.query(cTableSetting, new String[] { "type", "name", "value" }, "uid=?",
								new String[] { Integer.toString(uid) }, null, null, null);
						if (cursor == null)
							Util.log(null, Log.WARN, "Database cursor null (settings)");
						else
							try {
								while (cursor.moveToNext())
									listSetting.add(new PSetting(uid, cursor.getString(0), cursor.getString(1), cursor
											.getString(2)));
							} finally {
								cursor.close();
							}

						db.setTransactionSuccessful();
					} finally {
						try {
							db.endTransaction();
						} finally {
							mLock.readLock().unlock();
						}
					}
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

				synchronized (mLock) {
					mLock.writeLock().lock();
					db.beginTransaction();
					try {
						db.delete(cTableSetting, "uid=?", new String[] { Integer.toString(uid) });
						Util.log(null, Log.WARN, "Settings deleted uid=" + uid);

						db.setTransactionSuccessful();
					} finally {
						try {
							db.endTransaction();
						} finally {
							mLock.writeLock().unlock();
						}
					}
				}

				// Clear cache
				if (mUseCache)
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

				synchronized (mLock) {
					mLock.writeLock().lock();
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
						try {
							db.endTransaction();
						} finally {
							mLock.writeLock().unlock();
						}
					}
				}

				// Clear caches
				if (mUseCache) {
					synchronized (mRestrictionCache) {
						mRestrictionCache.clear();
					}
					synchronized (mSettingCache) {
						mSettingCache.clear();
					}
				}
				synchronized (mAskedOnceCache) {
					mAskedOnceCache.clear();
				}
				Util.log(null, Log.WARN, "Caches cleared");

				synchronized (mLockUsage) {
					mLockUsage.writeLock().lock();
					dbUsage.beginTransaction();
					try {
						dbUsage.execSQL("DELETE FROM " + cTableUsage);
						Util.log(null, Log.WARN, "Usage database cleared");

						dbUsage.setTransactionSuccessful();
					} finally {
						try {
							dbUsage.endTransaction();
						} finally {
							mLockUsage.writeLock().unlock();
						}
					}
				}

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

		// Helper methods

		private boolean onDemandDialog(final Hook hook, final PRestriction restriction, final PRestriction result) {
			try {
				int userId = Util.getUserId(restriction.uid);

				// Without handler nothing can be done
				if (mHandler == null)
					return false;

				// Check for exceptions
				if (hook != null && !hook.canOnDemand())
					return false;

				// Check if enabled
				if (!getSettingBool(userId, PrivacyManager.cSettingOnDemand, true))
					return false;
				if (!getSettingBool(restriction.uid, PrivacyManager.cSettingOnDemand, false))
					return false;

				// Skip dangerous methods
				final boolean dangerous = getSettingBool(userId, PrivacyManager.cSettingDangerous, false);
				if (!dangerous && hook != null && hook.isDangerous() && hook.whitelist() == null)
					return false;

				// Get am context
				final Context context = getContext();
				if (context == null)
					return false;

				long token = 0;
				try {
					token = Binder.clearCallingIdentity();

					// Get application info
					final ApplicationInfoEx appInfo = new ApplicationInfoEx(context, restriction.uid);

					// Check if system application
					if (!dangerous && appInfo.isSystem())
						return false;

					// Check if activity manager agrees
					if (!XActivityManagerService.canOnDemand())
						return false;

					// Go ask
					Util.log(null, Log.WARN, "On demand " + restriction);
					mOndemandSemaphore.acquireUninterruptibly();
					try {
						// Check if activity manager still agrees
						if (!XActivityManagerService.canOnDemand())
							return false;

						Util.log(null, Log.WARN, "On demanding " + restriction);

						// Check if not asked before
						CRestriction key = new CRestriction(restriction, restriction.extra);
						synchronized (mRestrictionCache) {
							if (mRestrictionCache.containsKey(key))
								if (mRestrictionCache.get(key).asked) {
									Util.log(null, Log.WARN, "Already asked " + restriction);
									return false;
								}
						}
						synchronized (mAskedOnceCache) {
							if (mAskedOnceCache.containsKey(key) && !mAskedOnceCache.get(key).isExpired()) {
								Util.log(null, Log.WARN, "Already asked once " + restriction);
								return false;
							}
						}

						if (restriction.extra != null && hook != null && hook.whitelist() != null) {
							CSetting skey = new CSetting(restriction.uid, hook.whitelist(), restriction.extra);
							CSetting xkey = null;
							String xextra = getXExtra(restriction, hook);
							if (xextra != null)
								xkey = new CSetting(restriction.uid, hook.whitelist(), xextra);
							synchronized (mSettingCache) {
								if (mSettingCache.containsKey(skey)
										|| (xkey != null && mSettingCache.containsKey(xkey))) {
									Util.log(null, Log.WARN, "Already asked " + skey);
									return false;
								}
							}
						}

						final AlertDialogHolder holder = new AlertDialogHolder();
						final CountDownLatch latch = new CountDownLatch(1);

						// Run dialog in looper
						mHandler.post(new Runnable() {
							@Override
							public void run() {
								try {
									// Dialog
									AlertDialog.Builder builder = getOnDemandDialogBuilder(restriction, hook, appInfo,
											dangerous, result, context, latch);
									AlertDialog alertDialog = builder.create();
									alertDialog.getWindow().setType(WindowManager.LayoutParams.TYPE_PHONE);
									alertDialog.getWindow().addFlags(WindowManager.LayoutParams.FLAG_LAYOUT_IN_SCREEN);
									alertDialog.getWindow().setSoftInputMode(
											WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_HIDDEN);
									alertDialog.setCancelable(false);
									alertDialog.setCanceledOnTouchOutside(false);
									alertDialog.show();
									holder.dialog = alertDialog;

									// Progress bar
									final ProgressBar mProgress = (ProgressBar) alertDialog
											.findViewById(R.id.pbProgress);
									mProgress.setMax(cMaxOnDemandDialog * 20);
									mProgress.setProgress(cMaxOnDemandDialog * 20);

									Runnable rProgress = new Runnable() {
										@Override
										public void run() {
											AlertDialog dialog = holder.dialog;
											if (dialog != null && dialog.isShowing() && mProgress.getProgress() > 0) {
												mProgress.incrementProgressBy(-1);
												mHandler.postDelayed(this, 50);
											}
										}
									};
									mHandler.postDelayed(rProgress, 50);
								} catch (Throwable ex) {
									Util.bug(null, ex);
									latch.countDown();
								}
							}
						});

						// Wait for dialog to complete
						if (!latch.await(cMaxOnDemandDialog, TimeUnit.SECONDS)) {
							Util.log(null, Log.WARN, "On demand dialog timeout " + restriction);
							mHandler.post(new Runnable() {
								@Override
								public void run() {
									AlertDialog dialog = holder.dialog;
									if (dialog != null)
										dialog.cancel();
								}
							});
						}
					} finally {
						mOndemandSemaphore.release();
					}
				} finally {
					Binder.restoreCallingIdentity(token);
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

			return true;
		}

		final class AlertDialogHolder {
			public AlertDialog dialog = null;
		}

		private AlertDialog.Builder getOnDemandDialogBuilder(final PRestriction restriction, final Hook hook,
				ApplicationInfoEx appInfo, boolean dangerous, final PRestriction result, Context context,
				final CountDownLatch latch) throws NameNotFoundException {
			// Get resources
			String self = PrivacyService.class.getPackage().getName();
			Resources resources = context.getPackageManager().getResourcesForApplication(self);

			// Reference views
			View view = LayoutInflater.from(context.createPackageContext(self, 0)).inflate(R.layout.ondemand, null);
			ImageView ivAppIcon = (ImageView) view.findViewById(R.id.ivAppIcon);
			TextView tvUid = (TextView) view.findViewById(R.id.tvUid);
			TextView tvAppName = (TextView) view.findViewById(R.id.tvAppName);
			TextView tvAttempt = (TextView) view.findViewById(R.id.tvAttempt);
			TextView tvCategory = (TextView) view.findViewById(R.id.tvCategory);
			TextView tvFunction = (TextView) view.findViewById(R.id.tvFunction);
			TextView tvParameters = (TextView) view.findViewById(R.id.tvParameters);
			TableRow rowParameters = (TableRow) view.findViewById(R.id.rowParameters);
			final CheckBox cbCategory = (CheckBox) view.findViewById(R.id.cbCategory);
			final CheckBox cbOnce = (CheckBox) view.findViewById(R.id.cbOnce);
			final CheckBox cbWhitelist = (CheckBox) view.findViewById(R.id.cbWhitelist);
			final CheckBox cbWhitelistExtra = (CheckBox) view.findViewById(R.id.cbWhitelistExtra);

			// Set values
			if ((hook != null && hook.isDangerous()) || appInfo.isSystem())
				view.setBackgroundColor(resources.getColor(R.color.color_dangerous_dark));

			ivAppIcon.setImageDrawable(appInfo.getIcon(context));
			tvUid.setText(Integer.toString(appInfo.getUid()));
			tvAppName.setText(TextUtils.join(", ", appInfo.getApplicationName()));

			String defaultAction = resources.getString(result.restricted ? R.string.title_deny : R.string.title_allow);
			tvAttempt.setText(resources.getString(R.string.title_attempt) + " (" + defaultAction + ")");

			int catId = resources.getIdentifier("restrict_" + restriction.restrictionName, "string", self);
			tvCategory.setText(resources.getString(catId));
			tvFunction.setText(restriction.methodName);
			if (restriction.extra == null)
				rowParameters.setVisibility(View.GONE);
			else
				tvParameters.setText(restriction.extra);

			cbCategory.setChecked(mSelectCategory);
			cbOnce.setChecked(mSelectOnce);
			cbOnce.setText(String.format(resources.getString(R.string.title_once),
					PrivacyManager.cRestrictionCacheTimeoutMs / 1000));

			if (hook != null && hook.whitelist() != null && restriction.extra != null) {
				cbWhitelist.setText(resources.getString(R.string.title_whitelist, restriction.extra));
				cbWhitelist.setVisibility(View.VISIBLE);
				String xextra = getXExtra(restriction, hook);
				if (xextra != null) {
					cbWhitelistExtra.setText(resources.getString(R.string.title_whitelist, xextra));
					cbWhitelistExtra.setVisibility(View.VISIBLE);
				}
			}

			// Category, once and whitelist exclude each other
			cbCategory.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
				@Override
				public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
					if (isChecked) {
						cbOnce.setChecked(false);
						cbWhitelist.setChecked(false);
						cbWhitelistExtra.setChecked(false);
					}
				}
			});
			cbOnce.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
				@Override
				public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
					if (isChecked) {
						cbCategory.setChecked(false);
						cbWhitelist.setChecked(false);
						cbWhitelistExtra.setChecked(false);
					}
				}
			});
			cbWhitelist.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
				@Override
				public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
					if (isChecked) {
						cbCategory.setChecked(false);
						cbOnce.setChecked(false);
						cbWhitelistExtra.setChecked(false);
					}
				}
			});
			cbWhitelistExtra.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
				@Override
				public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
					if (isChecked) {
						cbCategory.setChecked(false);
						cbOnce.setChecked(false);
						cbWhitelist.setChecked(false);
					}
				}
			});

			// Ask
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context, AlertDialog.THEME_HOLO_DARK);
			alertDialogBuilder.setTitle(resources.getString(R.string.app_name));
			alertDialogBuilder.setView(view);
			alertDialogBuilder.setIcon(resources.getDrawable(R.drawable.ic_launcher));
			alertDialogBuilder.setPositiveButton(resources.getString(R.string.title_deny),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							// Deny
							result.restricted = true;
							if (!cbWhitelist.isChecked() && !cbWhitelistExtra.isChecked()) {
								mSelectCategory = cbCategory.isChecked();
								mSelectOnce = cbOnce.isChecked();
							}
							if (cbWhitelist.isChecked())
								onDemandWhitelist(restriction, null, result, hook);
							else if (cbWhitelistExtra.isChecked())
								onDemandWhitelist(restriction, getXExtra(restriction, hook), result, hook);
							else if (cbOnce.isChecked())
								onDemandOnce(restriction, result);
							else
								onDemandChoice(restriction, cbCategory.isChecked(), true);
							latch.countDown();
						}
					});
			alertDialogBuilder.setNegativeButton(resources.getString(R.string.title_allow),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							// Allow
							result.restricted = false;
							if (!cbWhitelist.isChecked() && !cbWhitelistExtra.isChecked()) {
								mSelectCategory = cbCategory.isChecked();
								mSelectOnce = cbOnce.isChecked();
							}
							if (cbWhitelist.isChecked())
								onDemandWhitelist(restriction, null, result, hook);
							else if (cbWhitelistExtra.isChecked())
								onDemandWhitelist(restriction, getXExtra(restriction, hook), result, hook);
							else if (cbOnce.isChecked())
								onDemandOnce(restriction, result);
							else
								onDemandChoice(restriction, cbCategory.isChecked(), false);
							latch.countDown();
						}
					});
			return alertDialogBuilder;
		}

		private String getXExtra(PRestriction restriction, Hook hook) {
			if (hook != null)
				if (hook.whitelist().equals(Meta.cTypeFilename)) {
					String folder = new File(restriction.extra).getParent();
					if (!TextUtils.isEmpty(folder))
						return folder + File.separatorChar + "*";
				} else if (hook.whitelist().equals(Meta.cTypeIPAddress)) {
					int semi = restriction.extra.lastIndexOf(':');
					String address = (semi >= 0 ? restriction.extra.substring(0, semi) : restriction.extra);
					if (Patterns.IP_ADDRESS.matcher(address).matches()) {
						int dot = address.lastIndexOf('.');
						return address.substring(0, dot + 1) + '*'
								+ (semi >= 0 ? restriction.extra.substring(semi) : "");
					} else {
						int dot = restriction.extra.indexOf('.');
						if (dot > 0)
							return '*' + restriction.extra.substring(dot);
					}
				}
			return null;
		}

		private void onDemandWhitelist(final PRestriction restriction, String xextra, final PRestriction result,
				Hook hook) {
			try {
				// Set the whitelist
				Util.log(null, Log.WARN, (result.restricted ? "Black" : "White") + "listing " + restriction
						+ " xextra=" + xextra);
				setSettingInternal(new PSetting(restriction.uid, hook.whitelist(), (xextra == null ? restriction.extra
						: xextra), Boolean.toString(!result.restricted)));
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		private void onDemandOnce(final PRestriction restriction, final PRestriction result) {
			Util.log(null, Log.WARN, (result.restricted ? "Deny" : "Allow") + " once " + restriction);
			result.time = new Date().getTime() + PrivacyManager.cRestrictionCacheTimeoutMs;
			CRestriction key = new CRestriction(restriction, restriction.extra);
			synchronized (mAskedOnceCache) {
				if (mAskedOnceCache.containsKey(key))
					mAskedOnceCache.remove(key);
				mAskedOnceCache.put(key, key);
			}
		}

		private void onDemandChoice(PRestriction restriction, boolean category, boolean restrict) {
			try {
				int userId = Util.getUserId(restriction.uid);
				PRestriction result = new PRestriction(restriction);

				// Get current category restriction state
				boolean prevRestricted = false;
				CRestriction key = new CRestriction(restriction.uid, restriction.restrictionName, null, null);
				synchronized (mRestrictionCache) {
					if (mRestrictionCache.containsKey(key))
						prevRestricted = mRestrictionCache.get(key).restricted;
				}

				Util.log(null, Log.WARN, "On demand choice " + restriction + " category=" + category + "/"
						+ prevRestricted + " restrict=" + restrict);

				if (category || (restrict && restrict != prevRestricted)) {
					// Set category restriction
					result.methodName = null;
					result.restricted = restrict;
					result.asked = category;
					setRestrictionInternal(result);

					// Clear category on change
					boolean dangerous = getSettingBool(userId, PrivacyManager.cSettingDangerous, false);
					for (Hook md : PrivacyManager.getHooks(restriction.restrictionName)) {
						result.methodName = md.getName();
						result.restricted = (md.isDangerous() && !dangerous ? false : restrict);
						result.asked = category;
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
						Integer.toString(ActivityMain.STATE_CHANGED)));

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

		private boolean getSettingBool(int uid, String name, boolean defaultValue) throws RemoteException {
			return getSettingBool(uid, "", name, defaultValue);
		}

		private boolean getSettingBool(int uid, String type, String name, boolean defaultValue) throws RemoteException {
			String value = getSetting(new PSetting(uid, type, name, Boolean.toString(defaultValue))).value;
			return Boolean.parseBoolean(value);
		}

		private void enforcePermission(int uid) {
			if (uid >= 0)
				if (Util.getUserId(uid) != Util.getUserId(Binder.getCallingUid()))
					throw new SecurityException("uid=" + uid + " calling=" + Binder.getCallingUid());

			int callingUid = Util.getAppId(Binder.getCallingUid());
			if (callingUid != getXUid() && callingUid != Process.SYSTEM_UID)
				throw new SecurityException("xuid=" + mXUid + " calling=" + Binder.getCallingUid());
		}

		private Context getContext() {
			// public static ActivityManagerService self()
			// frameworks/base/services/java/com/android/server/am/ActivityManagerService.java
			try {
				Class<?> cam = Class.forName("com.android.server.am.ActivityManagerService");
				Object am = cam.getMethod("self").invoke(null);
				if (am == null)
					return null;
				return (Context) cam.getDeclaredField("mContext").get(am);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return null;
			}
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
					Util.log(null, Log.ERROR, "Does not exist folder=" + dbFile.getParentFile());

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
					folder.delete();
				}

				// Set database file permissions
				// Owner: rwx (system)
				// Group: rwx (system)
				// World: ---
				Util.setPermissions(dbFile.getParentFile().getAbsolutePath(), 0770, Process.SYSTEM_UID,
						Process.SYSTEM_UID);
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
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									ContentValues values = new ContentValues();
									values.put("uid", 0);
									if (db.getVersion() > 9)
										values.put("type", "");
									values.put("name", PrivacyManager.cSettingMigrated);
									values.put("value", Boolean.toString(true));
									db.insertWithOnConflict(cTableSetting, null, values,
											SQLiteDatabase.CONFLICT_REPLACE);

									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						// Upgrade database if needed
						if (db.needUpgrade(1)) {
							Util.log(null, Log.WARN, "Creating database");
							synchronized (mLock) {
								mLock.writeLock().lock();
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
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(2)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							// Old migrated indication
							db.setVersion(2);
						}

						if (db.needUpgrade(3)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									db.execSQL("DELETE FROM usage WHERE method=''");
									db.setVersion(3);
									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(4)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									db.execSQL("DELETE FROM setting WHERE value IS NULL");
									db.setVersion(4);
									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(5)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									db.execSQL("DELETE FROM setting WHERE value = ''");
									db.execSQL("DELETE FROM setting WHERE name = 'Random@boot' AND value = 'false'");
									db.setVersion(5);
									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(6)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									db.execSQL("DELETE FROM setting WHERE name LIKE 'OnDemand.%'");
									db.setVersion(6);
									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(7)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									db.execSQL("ALTER TABLE usage ADD COLUMN extra TEXT");
									db.setVersion(7);
									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(8)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									db.execSQL("DROP INDEX idx_usage");
									db.execSQL("CREATE UNIQUE INDEX idx_usage ON usage(uid, restriction, method, extra)");
									db.setVersion(8);
									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(9)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									db.execSQL("DROP TABLE usage");
									db.setVersion(9);
									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(10)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									db.execSQL("ALTER TABLE setting ADD COLUMN type TEXT");
									db.execSQL("DROP INDEX idx_setting");
									db.execSQL("CREATE UNIQUE INDEX idx_setting ON setting(uid, type, name)");
									db.execSQL("UPDATE setting SET type=''");
									db.setVersion(10);
									db.setTransactionSuccessful();
								} finally {
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						if (db.needUpgrade(11)) {
							Util.log(null, Log.WARN, "Upgrading from version=" + db.getVersion());
							synchronized (mLock) {
								mLock.writeLock().lock();
								db.beginTransaction();
								try {
									List<PSetting> listSetting = new ArrayList<PSetting>();
									Cursor cursor = db.query(cTableSetting, new String[] { "uid", "name", "value" },
											null, null, null, null, null);
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
													listSetting.add(new PSetting(uid, type, name.substring(dot + 1),
															value));
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
											db.delete(cTableSetting, "uid=? AND type=? AND name=?", new String[] {
													Integer.toString(setting.uid), setting.type, setting.name });
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
									try {
										db.endTransaction();
									} finally {
										mLock.writeLock().unlock();
									}
								}
							}
						}

						Util.log(null, Log.WARN, "Running VACUUM");
						synchronized (mLock) {
							mLock.writeLock().lock();
							try {
								db.execSQL("VACUUM");
							} catch (Throwable ex) {
								Util.bug(null, ex);
							} finally {
								mLock.writeLock().unlock();
							}
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
							Util.log(null, Log.ERROR, "Deleted corrupt usage data database");
						}

						// Upgrade database if needed
						if (dbUsage.needUpgrade(1)) {
							Util.log(null, Log.WARN, "Creating usage database");
							synchronized (mLockUsage) {
								mLockUsage.writeLock().lock();
								dbUsage.beginTransaction();
								try {
									dbUsage.execSQL("CREATE TABLE usage (uid INTEGER NOT NULL, restriction TEXT NOT NULL, method TEXT NOT NULL, extra TEXT NOT NULL, restricted INTEGER NOT NULL, time INTEGER NOT NULL)");
									dbUsage.execSQL("CREATE UNIQUE INDEX idx_usage ON usage(uid, restriction, method, extra)");
									dbUsage.setVersion(1);
									dbUsage.setTransactionSuccessful();
								} finally {
									try {
										dbUsage.endTransaction();
									} finally {
										mLockUsage.writeLock().unlock();
									}
								}
							}
						}

						Util.log(null, Log.WARN, "Running VACUUM");
						synchronized (mLockUsage) {
							mLockUsage.writeLock().lock();
							try {
								dbUsage.execSQL("VACUUM");
							} catch (Throwable ex) {
								Util.bug(null, ex);
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
	};
}

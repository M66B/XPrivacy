package biz.bokhorst.xprivacy;

import java.io.File;
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
import android.database.sqlite.SQLiteStatement;
import android.graphics.Paint;
import android.graphics.Typeface;
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
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.ScrollView;
import android.widget.TableLayout;
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

	private static final int cCurrentVersion = 273;
	private static final String cServiceName = "xprivacy" + cCurrentVersion;

	// TODO: define column names
	// sqlite3 /data/xprivacy/xprivacy.db

	public static void setupDatabase() {
		// This is run from Zygote with root permissions
		try {
			File dbFile = getDbFile();

			// Create database folder
			dbFile.getParentFile().mkdirs();

			// Set database file permissions
			// Owner: rwx (system)
			// Group: --- (system)
			// World: ---
			Util.setPermissions(dbFile.getParentFile().getAbsolutePath(), 0770, Process.SYSTEM_UID, Process.SYSTEM_UID);
			File[] files = dbFile.getParentFile().listFiles();
			if (files != null)
				for (File file : files)
					Util.setPermissions(file.getAbsolutePath(), 0770, Process.SYSTEM_UID, Process.SYSTEM_UID);

			// Move database from app folder
			File folder = new File(Environment.getDataDirectory() + File.separator + "data" + File.separator
					+ PrivacyService.class.getPackage().getName());
			File[] oldFiles = folder.listFiles();
			if (oldFiles != null)
				for (File file : oldFiles)
					if (file.getName().startsWith("xprivacy.db")) {
						File target = new File(dbFile.getParentFile() + File.separator + file.getName());
						boolean status = file.renameTo(target);
						Util.log(null, Log.WARN, "Moving " + file + " to " + target + " ok=" + status);
					}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	public static void register(List<String> listError, String secret) {
		// Store secret and errors
		mSecret = secret;
		mListError.addAll(listError);

		try {
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

			// Hook into activity manager service
			XActivityManagerService.setSemaphore(mOndemandSemaphore);
			XPrivacy.hookAll(XActivityManagerService.getInstances(), secret);

			// Register privacy service
			// @formatter:off
			// public static void addService(String name, IBinder service)
			// public static void addService(String name, IBinder service, boolean allowIsolated)
			// @formatter:on
			Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
				Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class,
						boolean.class);
				Util.log(null, Log.WARN, "Invoking " + mAddService);
				mAddService.invoke(null, cServiceName, mPrivacyService, true);
			} else {
				Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class);
				Util.log(null, Log.WARN, "Invoking " + mAddService);
				mAddService.invoke(null, cServiceName, mPrivacyService);
			}

			Util.log(null, Log.WARN, "Service registered name=" + cServiceName);
			mRegistered = true;
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
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

		// Disable disk strict mode
		try {
			ThreadPolicy oldPolicy = StrictMode.getThreadPolicy();
			ThreadPolicy newpolicy = new ThreadPolicy.Builder(oldPolicy).permitDiskReads().permitDiskWrites().build();
			StrictMode.setThreadPolicy(newpolicy);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		return mClient;
	}

	private static File getDbFile() {
		return new File(Environment.getDataDirectory() + File.separator + "xprivacy" + File.separator + "xprivacy.db");
	}

	public static void reportErrorInternal(String message) {
		synchronized (mListError) {
			mListError.add(message);
		}
	}

	public static PRestriction getRestriction(final PRestriction restriction, boolean usage, String secret)
			throws RemoteException {
		if (mRegistered)
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

	public static PSetting getSetting(PSetting setting) throws RemoteException {
		if (mRegistered)
			return mPrivacyService.getSetting(setting);
		else {
			IPrivacyService client = getClient();
			if (client == null) {
				Log.w("XPrivacy", "No client for " + setting + " uid=" + Process.myUid() + " pid=" + Process.myPid());
				return setting;
			} else
				return client.getSetting(setting);
		}
	}

	private static final IPrivacyService.Stub mPrivacyService = new IPrivacyService.Stub() {
		private ReentrantReadWriteLock mLock = new ReentrantReadWriteLock(true);
		private SQLiteDatabase mDatabase = null;
		private SQLiteStatement stmtGetRestriction = null;
		private SQLiteStatement stmtGetSetting = null;
		private SQLiteStatement stmtGetUsageRestriction = null;
		private SQLiteStatement stmtGetUsageMethod = null;

		private boolean mSelectCategory = true;
		private boolean mSelectOnce = false;

		private Map<CSetting, CSetting> mSettingCache = new HashMap<CSetting, CSetting>();
		private Map<CRestriction, CRestriction> mRestrictionCache = new HashMap<CRestriction, CRestriction>();

		private ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

		private final int cMaxUsageData = 500; // entries
		private final int cMaxOnDemandDialog = 20; // seconds

		// Management

		@Override
		public int getVersion() throws RemoteException {
			return cCurrentVersion;
		}

		@Override
		public List<String> check() throws RemoteException {
			enforcePermission();

			List<String> listError = new ArrayList<String>();
			synchronized (mListError) {
				int c = 0;
				int i = 0;
				while (i++ < mListError.size()) {
					String msg = mListError.get(i);
					c += msg.length();
					if (c < 5000)
						listError.add(msg);
					else
						break;
				}
			}

			File dbFile = getDbFile();
			if (!dbFile.exists())
				listError.add("Database does not exists");
			if (!dbFile.canRead())
				listError.add("Database not readable");
			if (!dbFile.canWrite())
				listError.add("Database not writable");

			SQLiteDatabase db = getDatabase();
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
				enforcePermission();
				setRestrictionInternal(restriction);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		private void setRestrictionInternal(PRestriction restriction) throws RemoteException {
			try {
				if (restriction.restrictionName == null) {
					Util.log(null, Log.ERROR, "Set invalid restriction " + restriction);
					return;
				}

				SQLiteDatabase db = getDatabase();
				// 0 not restricted, ask
				// 1 restricted, ask
				// 2 not restricted, asked
				// 3 restricted, asked

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

				// Update cache
				if (mUseCache)
					synchronized (mRestrictionCache) {
						if (restriction.methodName == null) {
							// Clear method cache
							synchronized (mRestrictionCache) {
								for (Hook hook : PrivacyManager.getHooks(restriction.restrictionName)) {
									CRestriction key = new CRestriction(new PRestriction(restriction.uid,
											restriction.restrictionName, hook.getName()));
									if (mRestrictionCache.containsKey(key))
										mRestrictionCache.remove(key);
								}
							}
						}

						// Update cache
						CRestriction key = new CRestriction(restriction);
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
			enforcePermission();
			for (PRestriction restriction : listRestriction)
				setRestrictionInternal(restriction);
		}

		@Override
		public PRestriction getRestriction(final PRestriction restriction, boolean usage, String secret)
				throws RemoteException {
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
							&& "getString".equals(restriction.methodName))
						return mresult;
					if (PrivacyManager.cIPC.equals(restriction.restrictionName))
						return mresult;
					else if (PrivacyManager.cStorage.equals(restriction.restrictionName))
						return mresult;
					else if (PrivacyManager.cSystem.equals(restriction.restrictionName))
						return mresult;
					else if (PrivacyManager.cView.equals(restriction.restrictionName))
						return mresult;
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
					if (!getSettingBool(0, PrivacyManager.cSettingSystem, false))
						return mresult;

				// Check if restrictions enabled
				if (usage && !getSettingBool(restriction.uid, PrivacyManager.cSettingRestricted, true))
					return mresult;

				// Check cache
				boolean cached = false;
				if (mUseCache) {
					CRestriction key = new CRestriction(restriction);
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
					PRestriction cresult = new PRestriction(restriction);
					cresult.methodName = null;
					boolean methodFound = false;

					// No permissions required
					SQLiteDatabase db = getDatabase();

					// Precompile statement when needed
					if (stmtGetRestriction == null) {
						String sql = "SELECT restricted FROM " + cTableRestriction
								+ " WHERE uid=? AND restriction=? AND method=?";
						stmtGetRestriction = db.compileStatement(sql);
					}

					// Execute statement
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

					if (!methodFound && hook != null && hook.isDangerous())
						if (!getSettingBool(0, PrivacyManager.cSettingDangerous, false)) {
							mresult.restricted = false;
							mresult.asked = true;
						}

					// Fallback
					if (!mresult.restricted && usage && PrivacyManager.isApplication(restriction.uid)
							&& !getSettingBool(0, PrivacyManager.cSettingMigrated, false)) {
						if (hook != null && !hook.isDangerous()) {
							mresult.restricted = PrivacyProvider.getRestrictedFallback(null, restriction.uid,
									restriction.restrictionName, restriction.methodName);
							Util.log(null, Log.WARN, "Fallback " + mresult);
						}
					}

					// Update cache
					if (mUseCache) {
						CRestriction ckey = new CRestriction(cresult);
						CRestriction mkey = new CRestriction(mresult);
						synchronized (mRestrictionCache) {
							if (mRestrictionCache.containsKey(ckey))
								mRestrictionCache.remove(ckey);
							mRestrictionCache.put(ckey, ckey);
							if (mRestrictionCache.containsKey(mkey))
								mRestrictionCache.remove(mkey);
							mRestrictionCache.put(mkey, mkey);
						}
					}
				}

				// Media: notify user
				if (mresult.restricted && usage && hook != null && hook.shouldNotify())
					notifyRestricted(restriction);

				// Ask to restrict
				if (!mresult.asked && usage && PrivacyManager.isApplication(restriction.uid))
					onDemandDialog(hook, restriction, mresult);

				// Log usage
				if (usage && hook != null && hook.hasUsageData())
					if (getSettingBool(0, PrivacyManager.cSettingUsage, true)) {
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
											SQLiteDatabase db = getDatabase();

											mLock.writeLock().lock();
											db.beginTransaction();
											try {
												ContentValues values = new ContentValues();
												values.put("uid", restriction.uid);
												values.put("restriction", restriction.restrictionName);
												values.put("method", restriction.methodName);
												values.put("restricted", mresult.restricted);
												values.put("time", new Date().getTime());
												db.insertWithOnConflict(cTableUsage, null, values,
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
									} catch (Throwable ex) {
										Util.bug(null, ex);
									}
								}
							});
						}
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return mresult;
		}

		@Override
		public List<PRestriction> getRestrictionList(PRestriction selector) throws RemoteException {
			List<PRestriction> result = new ArrayList<PRestriction>();
			try {
				enforcePermission();

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
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				mLock.writeLock().lock();
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
					try {
						db.endTransaction();
					} finally {
						mLock.writeLock().unlock();
					}
				}

				// Clear cache
				if (mUseCache)
					synchronized (mRestrictionCache) {
						mRestrictionCache.clear();
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
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				// Precompile statement when needed
				if (stmtGetUsageRestriction == null) {
					String sql = "SELECT MAX(time) FROM " + cTableUsage + " WHERE uid=? AND restriction=?";
					stmtGetUsageRestriction = db.compileStatement(sql);
				}
				if (stmtGetUsageMethod == null) {
					String sql = "SELECT MAX(time) FROM " + cTableUsage + " WHERE uid=? AND restriction=? AND method=?";
					stmtGetUsageMethod = db.compileStatement(sql);
				}

				mLock.readLock().lock();
				db.beginTransaction();
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

					db.setTransactionSuccessful();
				} finally {
					try {
						db.endTransaction();
					} finally {
						mLock.readLock().unlock();
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
			return lastUsage;
		}

		@Override
		public List<PRestriction> getUsageList(int uid) throws RemoteException {
			List<PRestriction> result = new ArrayList<PRestriction>();
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				mLock.readLock().lock();
				db.beginTransaction();
				try {
					Cursor cursor;
					if (uid == 0)
						cursor = db.query(cTableUsage, new String[] { "uid", "restriction", "method", "restricted",
								"time" }, null, new String[] {}, null, null, "time DESC LIMIT " + cMaxUsageData);
					else
						cursor = db.query(cTableUsage, new String[] { "uid", "restriction", "method", "restricted",
								"time" }, "uid=?", new String[] { Integer.toString(uid) }, null, null,
								"time DESC LIMIT " + cMaxUsageData);
					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (usage data)");
					else
						try {
							while (cursor.moveToNext()) {
								PRestriction data = new PRestriction();
								data.uid = cursor.getInt(0);
								data.restrictionName = cursor.getString(1);
								data.methodName = cursor.getString(2);
								data.restricted = (cursor.getInt(3) > 0);
								data.time = cursor.getLong(4);
								result.add(data);
							}
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
			return result;
		}

		@Override
		public void deleteUsage(int uid) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				mLock.writeLock().lock();
				db.beginTransaction();
				try {
					if (uid == 0)
						db.delete(cTableUsage, null, new String[] {});
					else
						db.delete(cTableUsage, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Usage data deleted uid=" + uid);

					db.setTransactionSuccessful();
				} finally {
					try {
						db.endTransaction();
					} finally {
						mLock.writeLock().unlock();
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
				enforcePermission();
				setSettingInternal(setting);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		private void setSettingInternal(PSetting setting) throws RemoteException {
			try {
				SQLiteDatabase db = getDatabase();

				mLock.writeLock().lock();
				db.beginTransaction();
				try {
					if (setting.value == null)
						db.delete(cTableSetting, "uid=? AND name=?", new String[] { Integer.toString(setting.uid),
								setting.name });
					else {
						// Create record
						ContentValues values = new ContentValues();
						values.put("uid", setting.uid);
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

				// Update cache
				if (mUseCache) {
					CSetting key = new CSetting(setting.uid, setting.name);
					key.setValue(setting.value);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							mSettingCache.remove(key);
						if (setting.value != null)
							mSettingCache.put(key, key);
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		@Override
		public void setSettingList(List<PSetting> listSetting) throws RemoteException {
			enforcePermission();
			for (PSetting setting : listSetting)
				setSettingInternal(setting);
		}

		@Override
		@SuppressLint("DefaultLocale")
		public PSetting getSetting(PSetting setting) throws RemoteException {
			PSetting result = new PSetting(setting.uid, setting.name, setting.value);
			try {
				// No permissions enforced

				// Check cache
				if (mUseCache && setting.value != null) {
					CSetting key = new CSetting(setting.uid, setting.name);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key)) {
							result.value = mSettingCache.get(key).getValue();
							return result;
						}
					}
				}

				// No persmissions required
				SQLiteDatabase db = getDatabase();

				// Fallback
				if (!PrivacyManager.cSettingMigrated.equals(setting.name)
						&& !getSettingBool(0, PrivacyManager.cSettingMigrated, false)) {
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
					String sql = "SELECT value FROM " + cTableSetting + " WHERE uid=? AND name=?";
					stmtGetSetting = db.compileStatement(sql);
				}

				// Execute statement
				mLock.readLock().lock();
				db.beginTransaction();
				try {
					try {
						synchronized (stmtGetSetting) {
							stmtGetSetting.clearBindings();
							stmtGetSetting.bindLong(1, setting.uid);
							stmtGetSetting.bindString(2, setting.name);
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

				// Add to cache
				if (mUseCache && result.value != null) {
					CSetting key = new CSetting(setting.uid, setting.name);
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
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				mLock.readLock().lock();
				db.beginTransaction();
				try {
					Cursor cursor = db.query(cTableSetting, new String[] { "name", "value" }, "uid=?",
							new String[] { Integer.toString(uid) }, null, null, null);
					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (settings)");
					else
						try {
							while (cursor.moveToNext())
								listSetting.add(new PSetting(uid, cursor.getString(0), cursor.getString(1)));
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
			return listSetting;
		}

		@Override
		public void deleteSettings(int uid) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

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
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				mLock.writeLock().lock();
				db.beginTransaction();
				try {
					db.execSQL("DELETE FROM restriction");
					db.execSQL("DELETE FROM setting");
					db.execSQL("DELETE FROM usage");
					Util.log(null, Log.WARN, "Database cleared");

					// Reset migrated
					ContentValues values = new ContentValues();
					values.put("uid", 0);
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

				// Clear caches
				if (mUseCache) {
					synchronized (mRestrictionCache) {
						mRestrictionCache.clear();
					}
					synchronized (mSettingCache) {
						mSettingCache.clear();
					}
					Util.log(null, Log.WARN, "Cache cleared");
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		// Helper methods

		private void onDemandDialog(final Hook hook, final PRestriction restriction, final PRestriction result) {
			// Neither category nor method is restricted and asked for
			try {
				// Without handler nothing can be done
				if (mHandler == null)
					return;

				if (hook != null && !hook.canOnDemand())
					return;

				if (!XActivityManagerService.canOnDemand())
					return;

				// Check if enabled
				if (!getSettingBool(0, PrivacyManager.cSettingOnDemand, true))
					return;
				if (!getSettingBool(restriction.uid, PrivacyManager.cSettingOnDemand, false))
					return;

				// Skip dangerous methods
				final boolean dangerous = getSettingBool(0, PrivacyManager.cSettingDangerous, false);
				if (!dangerous && hook != null && hook.isDangerous())
					return;

				// Get am context
				final Context context = getContext();
				if (context == null)
					return;

				long token = 0;
				try {
					token = Binder.clearCallingIdentity();

					// Get application info
					final ApplicationInfoEx appInfo = new ApplicationInfoEx(context, restriction.uid);

					// Check if system application
					if (!dangerous && appInfo.isSystem())
						return;

					// Go ask
					Util.log(null, Log.WARN, "On demand " + restriction);
					mOndemandSemaphore.acquireUninterruptibly();
					try {
						Util.log(null, Log.WARN, "On demanding " + restriction);

						// Check if not asked before
						CRestriction key = new CRestriction(restriction);
						synchronized (mRestrictionCache) {
							if (mRestrictionCache.containsKey(key))
								if (mRestrictionCache.get(key).asked) {
									Util.log(null, Log.WARN, "Already asked " + restriction);
									return;
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
									alertDialog.getWindow().setType(WindowManager.LayoutParams.TYPE_SYSTEM_DIALOG);
									alertDialog.getWindow().setSoftInputMode(
											WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_HIDDEN);
									alertDialog.setCancelable(false);
									alertDialog.setCanceledOnTouchOutside(false);
									alertDialog.show();
									holder.dialog = alertDialog;

									// Progress bar
									final ProgressBar mProgress = (ProgressBar) alertDialog.findViewById(1966);
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
									// Deny once
									result.restricted = dangerous
											|| (!(hook != null && hook.isDangerous()) && !appInfo.isSystem());
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
			int hmargin = resources.getDimensionPixelSize(R.dimen.activity_horizontal_margin);
			int vmargin = resources.getDimensionPixelSize(R.dimen.activity_vertical_margin);
			float density = context.getResources().getDisplayMetrics().density;

			// Build view
			ScrollView scroll = new ScrollView(context);
			ViewGroup.LayoutParams scrollParam = new ViewGroup.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.WRAP_CONTENT);
			scroll.setLayoutParams(scrollParam);
			scroll.setPadding(hmargin, vmargin, hmargin, vmargin);
			if ((hook != null && hook.isDangerous()) || appInfo.isSystem())
				scroll.setBackgroundColor(resources.getColor(R.color.color_dangerous_dark));

			// Container
			LinearLayout llContainer = new LinearLayout(context);
			llContainer.setOrientation(LinearLayout.VERTICAL);
			LinearLayout.LayoutParams llContainerParams = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
			llContainer.setLayoutParams(llContainerParams);

			// Container for icon & message
			LinearLayout llApplication = new LinearLayout(context);
			llApplication.setOrientation(LinearLayout.HORIZONTAL);
			LinearLayout.LayoutParams llApplicationParams = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
			llApplication.setLayoutParams(llApplicationParams);
			{
				// Application icon
				ImageView ivApp = new ImageView(context);
				ivApp.setImageDrawable(appInfo.getIcon(context));
				LinearLayout.LayoutParams ivAppParams = new LinearLayout.LayoutParams(
						LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
				ivAppParams.height = (int) (density * 32);
				ivAppParams.width = (int) (density * 32);
				ivApp.setLayoutParams(ivAppParams);
				llApplication.addView(ivApp);

				// Application name
				TextView tvApp = new TextView(context);
				tvApp.setText(TextUtils.join(", ", appInfo.getApplicationName()));
				tvApp.setTextAppearance(context, android.R.attr.textAppearanceMedium);
				LinearLayout.LayoutParams tvAppParams = new LinearLayout.LayoutParams(
						LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
				tvAppParams.setMargins(hmargin / 2, 0, 0, 0);
				tvApp.setLayoutParams(tvAppParams);
				llApplication.addView(tvApp);
			}
			llContainer.addView(llApplication);

			// Attempt
			TextView titleAttempt = new TextView(context);
			titleAttempt.setText(resources.getString(R.string.title_attempt));
			LinearLayout.LayoutParams tvAttemptParams = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
			tvAttemptParams.setMargins(0, vmargin / 2, 0, 0);
			titleAttempt.setLayoutParams(tvAttemptParams);
			titleAttempt.setPaintFlags(titleAttempt.getPaintFlags() | Paint.UNDERLINE_TEXT_FLAG);
			llContainer.addView(titleAttempt);

			// Table for restriction
			TableLayout table = new TableLayout(context);
			LinearLayout.LayoutParams llTableParams = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
			table.setLayoutParams(llTableParams);
			table.setPadding(0, 0, 0, vmargin / 2);
			table.setShrinkAllColumns(true);
			{
				TableRow row1 = new TableRow(context);
				TableRow row2 = new TableRow(context);
				TableRow row3 = new TableRow(context);

				TableRow.LayoutParams cellParams0 = new TableRow.LayoutParams(TableRow.LayoutParams.WRAP_CONTENT,
						TableRow.LayoutParams.WRAP_CONTENT);
				TableRow.LayoutParams cellParams1 = new TableRow.LayoutParams(0, TableRow.LayoutParams.WRAP_CONTENT, 1);
				cellParams1.setMargins(hmargin / 2, 0, 0, 0);

				// Category
				TextView titleCategory = new TextView(context);
				titleCategory.setText(resources.getString(R.string.title_category));
				titleCategory.setSingleLine(true);
				row1.addView(titleCategory, cellParams0);

				TextView category = new TextView(context);
				int catId = resources.getIdentifier("restrict_" + restriction.restrictionName, "string", self);
				category.setText(resources.getString(catId));
				category.setTypeface(null, Typeface.BOLD);
				category.setSingleLine(true);
				category.setEllipsize(TextUtils.TruncateAt.END);
				row1.addView(category, cellParams1);

				// Method
				TextView titleMethod = new TextView(context);
				titleMethod.setText(resources.getString(R.string.title_function));
				titleMethod.setSingleLine(true);
				row2.addView(titleMethod, cellParams0);

				TextView method = new TextView(context);
				method.setText(restriction.methodName);
				method.setTypeface(null, Typeface.BOLD);
				method.setSingleLine(true);
				method.setEllipsize(TextUtils.TruncateAt.START);
				row2.addView(method, cellParams1);

				// Arguments
				if (restriction.extra != null) {
					TextView titleArguments = new TextView(context);
					titleArguments.setText(resources.getString(R.string.title_parameters));
					titleArguments.setSingleLine(true);
					row3.addView(titleArguments, cellParams0);

					TextView argument = new TextView(context);
					argument.setText(restriction.extra);
					argument.setTypeface(null, Typeface.BOLD);
					argument.setSingleLine(true);
					argument.setEllipsize(TextUtils.TruncateAt.MIDDLE);
					row3.addView(argument, cellParams1);
				}

				TableLayout.LayoutParams rowParams = new TableLayout.LayoutParams(
						TableLayout.LayoutParams.WRAP_CONTENT, TableLayout.LayoutParams.WRAP_CONTENT);

				table.addView(row1, rowParams);
				table.addView(row2, rowParams);
				if (restriction.extra != null)
					table.addView(row3, rowParams);
			}
			llContainer.addView(table);

			// Category check box
			final CheckBox cbCategory = new CheckBox(context);
			cbCategory.setText(resources.getString(R.string.title_applycat));
			cbCategory.setChecked(mSelectCategory);
			LinearLayout.LayoutParams llCategoryParams = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
			cbCategory.setLayoutParams(llCategoryParams);
			llContainer.addView(cbCategory);

			// Once check box
			final CheckBox cbOnce = new CheckBox(context);
			cbOnce.setText(String.format(resources.getString(R.string.title_once),
					PrivacyManager.cRestrictionCacheTimeoutMs / 1000));
			cbOnce.setChecked(mSelectOnce);
			LinearLayout.LayoutParams llOnceParams = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
			cbOnce.setLayoutParams(llOnceParams);
			llContainer.addView(cbOnce);

			// Message
			TextView tvPlease = new TextView(context);
			tvPlease.setText(resources.getString(R.string.title_pleasesubmit));
			tvPlease.setTypeface(null, Typeface.ITALIC);
			LinearLayout.LayoutParams tvPleaseParams = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
			tvPlease.setLayoutParams(tvPleaseParams);
			llContainer.addView(tvPlease);

			// Progress bar
			ProgressBar pbProgress = new ProgressBar(context, null, android.R.attr.progressBarStyleHorizontal);
			pbProgress.setId(1966);
			pbProgress.setIndeterminate(false);
			LinearLayout.LayoutParams llProgress = new LinearLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
			llProgress.setMargins(0, vmargin, 0, 0);
			pbProgress.setLayoutParams(llProgress);
			llContainer.addView(pbProgress);

			scroll.addView(llContainer);

			// Ask
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
			alertDialogBuilder.setTitle(resources.getString(R.string.app_name));
			alertDialogBuilder.setView(scroll);
			alertDialogBuilder.setIcon(resources.getDrawable(R.drawable.ic_launcher));
			alertDialogBuilder.setPositiveButton(resources.getString(R.string.title_deny),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							// Deny
							result.restricted = true;
							mSelectCategory = cbCategory.isChecked();
							mSelectOnce = cbOnce.isChecked();
							if (cbOnce.isChecked())
								Util.log(null, Log.WARN, "Deny once " + restriction);
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
							mSelectCategory = cbCategory.isChecked();
							mSelectOnce = cbOnce.isChecked();
							if (cbOnce.isChecked())
								Util.log(null, Log.WARN, "Allow once " + restriction);
							else
								onDemandChoice(restriction, cbCategory.isChecked(), false);
							latch.countDown();
						}
					});
			return alertDialogBuilder;
		}

		private void onDemandChoice(PRestriction restriction, boolean category, boolean restrict) {
			try {
				PRestriction result = new PRestriction(restriction);

				// Get current category restriction state
				boolean prevRestricted = false;
				CRestriction key = new CRestriction(restriction);
				key.methodName = null;
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
					boolean dangerous = getSettingBool(0, PrivacyManager.cSettingDangerous, false);
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
					setRestrictionInternal(result);
				}

				// Mark state as changed
				setSettingInternal(new PSetting(restriction.uid, PrivacyManager.cSettingState,
						Integer.toString(ActivityMain.STATE_CHANGED)));

				// Update modification time
				setSettingInternal(new PSetting(restriction.uid, PrivacyManager.cSettingModifyTime,
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
							text += " (" + restriction.restrictionName + "/" + restriction.methodName + ")";
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
			String value = getSetting(new PSetting(uid, name, Boolean.toString(defaultValue))).value;
			return Boolean.parseBoolean(value);
		}

		private void enforcePermission() {
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

		private SQLiteDatabase getDatabase() {
			synchronized (this) {
				// Check current reference
				if (mDatabase != null && !mDatabase.isOpen()) {
					mDatabase = null;
					Util.log(null, Log.ERROR, "Database not open");
				}

				if (mDatabase == null)
					try {
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
							mLock.writeLock().lock();
							db.beginTransaction();
							try {
								ContentValues values = new ContentValues();
								values.put("uid", 0);
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

						// Upgrade database if needed
						if (db.needUpgrade(1)) {
							mLock.writeLock().lock();
							db.beginTransaction();
							try {
								// http://www.sqlite.org/lang_createtable.html
								Util.log(null, Log.WARN, "Creating database");
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

						if (db.needUpgrade(2))
							// Old migrated indication
							db.setVersion(2);

						if (db.needUpgrade(3)) {
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

						if (db.needUpgrade(4)) {
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

						if (db.needUpgrade(5)) {
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

						if (db.needUpgrade(6)) {
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

						Util.log(null, Log.WARN, "Database version=" + db.getVersion());
						mDatabase = db;
					} catch (Throwable ex) {
						mDatabase = null; // retry
						Util.bug(null, ex);
					}

				return mDatabase;
			}
		}
	};
}

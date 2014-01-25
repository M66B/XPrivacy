package biz.bokhorst.xprivacy;

import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import android.annotation.SuppressLint;
import android.content.ContentValues;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteDoneException;
import android.database.sqlite.SQLiteStatement;
import android.os.Binder;
import android.os.Environment;
import android.os.IBinder;
import android.os.RemoteException;
import android.os.StrictMode;
import android.os.StrictMode.ThreadPolicy;
import android.text.TextUtils;
import android.util.Log;

public class PrivacyService {
	private static int mXUid = -1;
	private static String mSecret = null;
	private static List<String> mListError;
	private static IPrivacyService mClient = null;
	private static SQLiteDatabase mDatabase = null;
	private static boolean mSettings = false;
	private static boolean mUsage = true;
	private static boolean mSystem = false;

	private static SQLiteStatement stmtGetRestriction = null;
	private static SQLiteStatement stmtGetSetting = null;
	private static SQLiteStatement stmtGetUsageRestriction = null;
	private static SQLiteStatement stmtGetUsageMethod = null;

	private static int cCurrentVersion = 245;
	private static String cServiceName = "xprivacy" + cCurrentVersion;
	private static String cTableRestriction = "restriction";
	private static String cTableUsage = "usage";
	private static String cTableSetting = "setting";

	private static boolean mUseCache = false;
	private static Map<CSetting, CSetting> mSettingCache = new HashMap<CSetting, CSetting>();
	private static Map<CRestriction, CRestriction> mRestrictionCache = new HashMap<CRestriction, CRestriction>();

	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

	// TODO: define column names

	public static void register(List<String> listError, String secret) {
		try {
			mSecret = secret;
			mListError = listError;

			// public static void addService(String name, IBinder service)
			Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
			Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class);
			mAddService.invoke(null, cServiceName, mPrivacyService);
			Util.log(null, Log.WARN, "Service registered name=" + cServiceName);

			// http://stackoverflow.com/questions/2630158/detect-application-heap-size-in-android
			int memoryClass = (int) (Runtime.getRuntime().maxMemory() / 1024L / 1024L);
			mUseCache = (memoryClass >= 32);
			Util.log(null, Log.WARN, "Memory class=" + memoryClass + " cache=" + mUseCache);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	public static IPrivacyService getClient() {
		if (mClient == null)
			try {
				// public static IBinder getService(String name)
				Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
				Method mGetService = cServiceManager.getDeclaredMethod("getService", String.class);
				mClient = IPrivacyService.Stub.asInterface((IBinder) mGetService.invoke(null, cServiceName));
				if (mClient != null)
					if (PrivacyService.getClient().getVersion() != cCurrentVersion)
						mClient = null;
			} catch (Throwable ex) {
				mClient = null;
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

	private static final IPrivacyService.Stub mPrivacyService = new IPrivacyService.Stub() {

		// Management

		@Override
		public int getVersion() throws RemoteException {
			return cCurrentVersion;
		}

		@Override
		public void migrated() throws RemoteException {
			SQLiteDatabase db = getDatabase();
			if (db.getVersion() < 2)
				db.setVersion(2);
		}

		@Override
		public List<String> check() throws RemoteException {
			enforcePermission();
			return mListError;
		}

		// Restrictions

		@Override
		public void setRestriction(ParcelableRestriction restriction) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					// Create category record
					if (restriction.methodName == null || restriction.restricted) {
						ContentValues cvalues = new ContentValues();
						cvalues.put("uid", restriction.uid);
						cvalues.put("restriction", restriction.restrictionName);
						cvalues.put("method", "");
						cvalues.put("restricted", restriction.restricted);
						db.insertWithOnConflict(cTableRestriction, null, cvalues, SQLiteDatabase.CONFLICT_REPLACE);
					}

					// Create method record
					if (restriction.methodName != null) {
						ContentValues mvalues = new ContentValues();
						mvalues.put("uid", restriction.uid);
						mvalues.put("restriction", restriction.restrictionName);
						mvalues.put("method", restriction.methodName);
						mvalues.put("restricted", !restriction.restricted);
						db.insertWithOnConflict(cTableRestriction, null, mvalues, SQLiteDatabase.CONFLICT_REPLACE);
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Clear cache
				if (mUseCache)
					synchronized (mRestrictionCache) {
						CRestriction key = new CRestriction(restriction.uid, restriction.restrictionName, null);
						if (mRestrictionCache.containsKey(key))
							mRestrictionCache.remove(key);
						for (Hook hook : PrivacyManager.getHooks(restriction.restrictionName)) {
							key = new CRestriction(restriction.uid, restriction.restrictionName, hook.getName());
							if (mRestrictionCache.containsKey(key))
								mRestrictionCache.remove(key);
						}
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		@Override
		public void setRestrictionList(List<ParcelableRestriction> listRestriction) throws RemoteException {
			// Permission enforced by setRestriction
			for (ParcelableRestriction restriction : listRestriction)
				setRestriction(restriction);
		}

		@Override
		public boolean getRestriction(final ParcelableRestriction restriction, boolean usage, String secret)
				throws RemoteException {
			boolean restricted = false;
			try {
				// No permissions enforced, but usage data requires a secret

				// Cache settings
				if (!mSettings) {
					mSettings = true;
					mUsage = Boolean.parseBoolean(getSetting(new ParcelableSetting(0, PrivacyManager.cSettingUsage,
							Boolean.toString(true))).value);
					mSystem = Boolean.parseBoolean(getSetting(new ParcelableSetting(0, PrivacyManager.cSettingSystem,
							Boolean.toString(false))).value);
				}

				// Check for self
				if (Util.getAppId(restriction.uid) == getXUid()) {
					if (PrivacyManager.cIdentification.equals(restriction.restrictionName)
							&& "getString".equals(restriction.methodName))
						return false;
					if (PrivacyManager.cIPC.equals(restriction.restrictionName))
						return false;
					else if (PrivacyManager.cStorage.equals(restriction.restrictionName))
						return false;
					else if (PrivacyManager.cSystem.equals(restriction.restrictionName))
						return false;
					else if (PrivacyManager.cView.equals(restriction.restrictionName))
						return false;
				}

				if (usage) {
					// Check for system
					if (!mSystem && !PrivacyManager.isApplication(restriction.uid))
						return false;

					// Check if restrictions enabled
					boolean enabled = Boolean.parseBoolean(getSetting(new ParcelableSetting(restriction.uid,
							PrivacyManager.cSettingRestricted, Boolean.toString(true))).value);
					if (!enabled)
						return false;
				}

				// Check cache
				boolean cached = false;
				if (mUseCache) {
					CRestriction key = new CRestriction(restriction.uid, restriction.restrictionName,
							restriction.methodName);
					synchronized (mRestrictionCache) {
						if (mRestrictionCache.containsKey(key)) {
							cached = true;
							restricted = mRestrictionCache.get(key).isRestricted();
						}
					}
				}

				if (!cached) {
					// No permissions required
					SQLiteDatabase db = getDatabase();

					// Precompile statement when needed
					if (stmtGetRestriction == null) {
						String sql = "SELECT restricted FROM " + cTableRestriction
								+ " WHERE uid=? AND restriction=? AND method=?";
						stmtGetRestriction = db.compileStatement(sql);
					}

					// Execute statement
					db.beginTransaction();
					try {
						try {
							synchronized (stmtGetRestriction) {
								stmtGetRestriction.clearBindings();
								stmtGetRestriction.bindLong(1, restriction.uid);
								stmtGetRestriction.bindString(2, restriction.restrictionName);
								stmtGetRestriction.bindString(3, "");
								restricted = (stmtGetRestriction.simpleQueryForLong() > 0);
							}
						} catch (SQLiteDoneException ignored) {
							restricted = false;
						}

						if (restricted && restriction.methodName != null)
							try {
								boolean mallowed;
								synchronized (stmtGetRestriction) {
									stmtGetRestriction.clearBindings();
									stmtGetRestriction.bindLong(1, restriction.uid);
									stmtGetRestriction.bindString(2, restriction.restrictionName);
									stmtGetRestriction.bindString(3, restriction.methodName);
									mallowed = (stmtGetRestriction.simpleQueryForLong() > 0);
									if (mallowed)
										restricted = false;
								}
							} catch (SQLiteDoneException ignored) {
								// no change
							}

						db.setTransactionSuccessful();
					} finally {
						db.endTransaction();
					}

					// Fallback
					if (restricted == false && db.getVersion() == 1)
						restricted = PrivacyProvider.getRestrictedFallback(null, restriction.uid,
								restriction.restrictionName, restriction.methodName);

					// Update cache
					if (mUseCache) {
						CRestriction key = new CRestriction(restriction.uid, restriction.restrictionName,
								restriction.methodName);
						key.setRestricted(restricted);
						synchronized (mRestrictionCache) {
							if (mRestrictionCache.containsKey(key))
								mRestrictionCache.remove(key);
							mRestrictionCache.put(key, key);
						}
					}
				}

				// Log usage
				if (mUsage && usage && restriction.methodName != null) {
					// Check secret
					boolean allowed = true;
					if (Util.getAppId(Binder.getCallingUid()) != getXUid()) {
						if (mSecret == null || !mSecret.equals(secret)) {
							allowed = false;
							Util.log(null, Log.WARN, "Invalid secret");
						}
					}

					if (allowed) {
						final boolean sRestricted = restricted;
						mExecutor.execute(new Runnable() {
							public void run() {
								try {
									SQLiteDatabase db = getDatabase();

									db.beginTransaction();
									try {
										ContentValues values = new ContentValues();
										values.put("uid", restriction.uid);
										values.put("restriction", restriction.restrictionName);
										values.put("method", restriction.methodName);
										values.put("restricted", sRestricted);
										values.put("time", new Date().getTime());
										db.insertWithOnConflict(cTableUsage, null, values,
												SQLiteDatabase.CONFLICT_REPLACE);

										db.setTransactionSuccessful();
									} catch (Throwable ex) {
										Util.bug(null, ex);
									} finally {
										db.endTransaction();
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
				return false;
			}
			return restricted;
		}

		@Override
		public List<ParcelableRestriction> getRestrictionList(ParcelableRestriction selector) throws RemoteException {
			List<ParcelableRestriction> result = new ArrayList<ParcelableRestriction>();
			try {
				enforcePermission();

				if (selector.restrictionName == null)
					for (String sRestrictionName : PrivacyManager.getRestrictions()) {
						ParcelableRestriction restriction = new ParcelableRestriction(selector.uid, sRestrictionName,
								null, false);
						restriction.restricted = getRestriction(restriction, false, mSecret);
						result.add(restriction);
					}
				else
					for (Hook md : PrivacyManager.getHooks(selector.restrictionName)) {
						ParcelableRestriction restriction = new ParcelableRestriction(selector.uid,
								selector.restrictionName, md.getName(), false);
						restriction.restricted = getRestriction(restriction, false, mSecret);
						result.add(restriction);
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
			return result;
		}

		@Override
		public void deleteRestrictions(int uid) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					db.delete(cTableRestriction, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Restrictions deleted uid=" + uid);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
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
		public long getUsage(List<ParcelableRestriction> listRestriction) throws RemoteException {
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

				db.beginTransaction();
				try {
					for (ParcelableRestriction restriction : listRestriction) {
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
					db.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
			return lastUsage;
		}

		@Override
		public List<ParcelableRestriction> getUsageList(int uid) throws RemoteException {
			List<ParcelableRestriction> result = new ArrayList<ParcelableRestriction>();
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					Cursor cursor;
					if (uid == 0)
						cursor = db.query(cTableUsage, new String[] { "uid", "restriction", "method", "restricted",
								"time" }, null, new String[] {}, null, null, null);
					else
						cursor = db.query(cTableUsage, new String[] { "uid", "restriction", "method", "restricted",
								"time" }, "uid=?", new String[] { Integer.toString(uid) }, null, null, null);
					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (usage data)");
					else
						try {
							while (cursor.moveToNext()) {
								ParcelableRestriction data = new ParcelableRestriction();
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
					db.endTransaction();
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

				db.beginTransaction();
				try {
					if (uid == 0)
						db.delete(cTableUsage, null, new String[] {});
					else
						db.delete(cTableUsage, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Usage data deleted uid=" + uid);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		// Settings

		@Override
		public void setSetting(ParcelableSetting setting) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					// Create record
					ContentValues values = new ContentValues();
					values.put("uid", setting.uid);
					values.put("name", setting.name);
					values.put("value", setting.value);

					// Insert/update record
					db.insertWithOnConflict(cTableSetting, null, values, SQLiteDatabase.CONFLICT_REPLACE);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Update cache
				if (mUseCache) {
					CSetting key = new CSetting(setting.uid, setting.name);
					key.setValue(setting.value);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							mSettingCache.remove(key);
						mSettingCache.put(key, key);
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		@Override
		public void setSettingList(List<ParcelableSetting> listSetting) throws RemoteException {
			// Permission enforced by setSetting
			for (ParcelableSetting setting : listSetting)
				setSetting(setting);
		}

		@Override
		@SuppressLint("DefaultLocale")
		public ParcelableSetting getSetting(ParcelableSetting setting) throws RemoteException {
			try {
				// No permissions enforced

				// Check cache
				if (mUseCache) {
					CSetting key = new CSetting(setting.uid, setting.name);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key)) {
							setting.value = mSettingCache.get(key).getValue();
							return setting;
						}
					}
				}

				// No persmissions required
				SQLiteDatabase db = getDatabase();

				// Fallback
				if (db.getVersion() == 1) {
					if (setting.uid == 0)
						setting.value = PrivacyProvider.getSettingFallback(setting.name, null, false);
					if (setting.value == null) {
						setting.value = PrivacyProvider.getSettingFallback(
								String.format("%s.%d", setting.name, setting.uid), setting.value, false);
						return setting;
					}
				}

				// Precompile statement when needed
				if (stmtGetSetting == null) {
					String sql = "SELECT value FROM " + cTableSetting + " WHERE uid=? AND name=?";
					stmtGetSetting = db.compileStatement(sql);
				}

				// Execute statement
				db.beginTransaction();
				try {
					try {
						synchronized (stmtGetSetting) {
							stmtGetSetting.clearBindings();
							stmtGetSetting.bindLong(1, setting.uid);
							stmtGetSetting.bindString(2, setting.name);
							setting.value = stmtGetSetting.simpleQueryForString();
						}
					} catch (SQLiteDoneException ignored) {
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Add to cache
				if (mUseCache) {
					CSetting key = new CSetting(setting.uid, setting.name);
					key.setValue(setting.value);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							mSettingCache.remove(key);
						mSettingCache.put(key, key);
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return setting;
			}
			return setting;
		}

		@Override
		public List<ParcelableSetting> getSettingList(int uid) throws RemoteException {
			List<ParcelableSetting> listSetting = new ArrayList<ParcelableSetting>();
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					Cursor cursor = db.query(cTableSetting, new String[] { "name", "value" }, "uid=?",
							new String[] { Integer.toString(uid) }, null, null, null);
					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (settings)");
					else
						try {
							while (cursor.moveToNext())
								listSetting.add(new ParcelableSetting(uid, cursor.getString(0), cursor.getString(1)));
						} finally {
							cursor.close();
						}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
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

				db.beginTransaction();
				try {
					db.delete(cTableSetting, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Settings deleted uid=" + uid);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
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
	};

	private static void enforcePermission() {
		int callingUid = Util.getAppId(Binder.getCallingUid());
		if (callingUid != getXUid()) {
			String callingPkg = null;
			Context context = getContext();
			if (context != null) {
				PackageManager pm = context.getPackageManager();
				if (pm != null)
					callingPkg = TextUtils.join(", ", pm.getPackagesForUid(Binder.getCallingUid()));
			}
			throw new SecurityException("xuid=" + mXUid + " calling=" + Binder.getCallingUid() + "/" + callingPkg);
		}
	}

	private static Context getContext() {
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

	private static int getXUid() {
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		return mXUid;
	}

	private static File getDbFile() {
		return new File(Environment.getDataDirectory() + File.separator + "data" + File.separator
				+ PrivacyService.class.getPackage().getName() + File.separator + "xprivacy.db");
	}

	public static void setupDatebase() {
		// This is run from Zygote with root permissions
		try {
			// Move database from experimental location
			File folder = new File(Environment.getDataDirectory() + File.separator + "xprivacy");
			if (folder.exists()) {
				File[] files = folder.listFiles();
				if (files != null)
					for (File file : files) {
						File target = new File(getDbFile().getParentFile() + File.separator + file.getName());
						Util.log(null, Log.WARN, "Moving " + file + " to " + target);
						file.renameTo(target);
					}
				folder.delete();
			}

			// Set application folder permission
			// Owner: rwx (untouched)
			// Group: rwx (set to system)
			// World: --x
			File dbFile = getDbFile();
			Util.setPermission(dbFile.getParentFile().getAbsolutePath(), 0771, -1, PrivacyManager.cAndroidUid);

			// Set database file permissions
			// Owner: rwx (untouched)
			// Group: rwx (set to system)
			// World: ---
			if (dbFile.exists())
				Util.setPermission(dbFile.getAbsolutePath(), 0770, -1, PrivacyManager.cAndroidUid);
			File journal = new File(dbFile + "-journal");
			if (journal.exists())
				Util.setPermission(journal.getAbsolutePath(), 0770, -1, PrivacyManager.cAndroidUid);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private static SQLiteDatabase getDatabase() {
		if (mDatabase == null) {
			File dbFile = getDbFile();
			SQLiteDatabase db = SQLiteDatabase.openOrCreateDatabase(dbFile, null);
			if (db.needUpgrade(1)) {
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
					Util.log(null, Log.WARN, "Database created");
				} catch (Throwable ex) {
					Util.bug(null, ex);
				} finally {
					db.endTransaction();
				}

			} else if (db.needUpgrade(2)) {
				// Do nothing, done by migration

			} else if (db.needUpgrade(3)) {
				db.beginTransaction();
				try {
					db.execSQL("DELETE FROM usage WHERE method=''");
					db.setVersion(3);
					db.setTransactionSuccessful();
				} catch (Throwable ex) {
					Util.bug(null, ex);
				} finally {
					db.endTransaction();
				}
			}

			Util.log(null, Log.WARN, "Database version=" + db.getVersion());
			mDatabase = db;
		}
		return mDatabase;
	}
}

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
import android.util.Log;

public class PrivacyService {
	private static int mXUid = -1;
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

	private static int cCurrentVersion = 243;
	private static String cServiceName = "xprivacy" + cCurrentVersion;
	private static String cTableRestriction = "restriction";
	private static String cTableUsage = "usage";
	private static String cTableSetting = "setting";

	private static boolean mUseCache = false;
	private static Map<CSetting, CSetting> mSettingCache = new HashMap<CSetting, CSetting>();
	private static Map<CRestriction, CRestriction> mRestrictionCache = new HashMap<CRestriction, CRestriction>();

	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

	// TODO: define column names

	public static void register(List<String> listError) {
		try {
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
		public void setRestriction(int uid, String restrictionName, String methodName, boolean restricted)
				throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					// Create category record
					if (methodName == null || restricted) {
						ContentValues cvalues = new ContentValues();
						cvalues.put("uid", uid);
						cvalues.put("restriction", restrictionName);
						cvalues.put("method", "");
						cvalues.put("restricted", restricted);
						db.insertWithOnConflict(cTableRestriction, null, cvalues, SQLiteDatabase.CONFLICT_REPLACE);
					}

					// Create method record
					if (methodName != null) {
						ContentValues mvalues = new ContentValues();
						mvalues.put("uid", uid);
						mvalues.put("restriction", restrictionName);
						mvalues.put("method", methodName);
						mvalues.put("restricted", !restricted);
						db.insertWithOnConflict(cTableRestriction, null, mvalues, SQLiteDatabase.CONFLICT_REPLACE);
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Clear cache
				if (mUseCache)
					synchronized (mRestrictionCache) {
						CRestriction key = new CRestriction(uid, restrictionName, null);
						if (mRestrictionCache.containsKey(key))
							mRestrictionCache.remove(key);
						for (Hook hook : PrivacyManager.getHooks(restrictionName)) {
							key = new CRestriction(uid, restrictionName, hook.getName());
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
			for (ParcelableRestriction restriction : listRestriction)
				setRestriction(restriction.uid, restriction.restrictionName, restriction.methodName,
						restriction.restricted);
		}

		@Override
		public boolean getRestriction(final int uid, final String restrictionName, final String methodName,
				final boolean usage) throws RemoteException {
			boolean restricted = false;
			try {
				// Cache settings
				if (!mSettings) {
					mSettings = true;
					mUsage = Boolean.parseBoolean(getSetting(0, PrivacyManager.cSettingUsage, Boolean.toString(true)));
					mSystem = Boolean
							.parseBoolean(getSetting(0, PrivacyManager.cSettingSystem, Boolean.toString(false)));
				}

				// Check for self
				if (uid == getXUid()) {
					if (PrivacyManager.cIdentification.equals(restrictionName) && "getString".equals(methodName))
						return false;
					if (PrivacyManager.cIPC.equals(restrictionName))
						return false;
					else if (PrivacyManager.cStorage.equals(restrictionName))
						return false;
					else if (PrivacyManager.cSystem.equals(restrictionName))
						return false;
					else if (PrivacyManager.cView.equals(restrictionName))
						return false;
				}

				// Check for system
				if (!mSystem && !PrivacyManager.isApplication(uid))
					return false;

				// Check cache
				boolean cached = false;
				if (mUseCache) {
					CRestriction key = new CRestriction(uid, restrictionName, methodName);
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
								stmtGetRestriction.bindLong(1, uid);
								stmtGetRestriction.bindString(2, restrictionName);
								stmtGetRestriction.bindString(3, "");
								restricted = (stmtGetRestriction.simpleQueryForLong() > 0);
							}
						} catch (SQLiteDoneException ignored) {
							restricted = false;
						}

						if (restricted && methodName != null)
							try {
								boolean mallowed;
								synchronized (stmtGetRestriction) {
									stmtGetRestriction.clearBindings();
									stmtGetRestriction.bindLong(1, uid);
									stmtGetRestriction.bindString(2, restrictionName);
									stmtGetRestriction.bindString(3, methodName);
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
						restricted = PrivacyProvider.getRestrictedFallback(null, uid, restrictionName, methodName);

					// Update cache
					if (mUseCache) {
						CRestriction key = new CRestriction(uid, restrictionName, methodName);
						key.setRestricted(restricted);
						synchronized (mRestrictionCache) {
							if (mRestrictionCache.containsKey(key))
								mRestrictionCache.remove(key);
							mRestrictionCache.put(key, key);
						}
					}
				}

				// Log usage
				if (mUsage && usage && methodName != null) {
					final boolean sRestricted = restricted;
					mExecutor.execute(new Runnable() {
						public void run() {
							try {
								SQLiteDatabase db = getDatabase();

								db.beginTransaction();
								try {
									ContentValues values = new ContentValues();
									values.put("uid", uid);
									values.put("restriction", restrictionName);
									values.put("method", methodName);
									values.put("restricted", sRestricted);
									values.put("time", new Date().getTime());
									db.insertWithOnConflict(cTableUsage, null, values, SQLiteDatabase.CONFLICT_REPLACE);

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
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return false;
			}
			return restricted;
		}

		@Override
		public List<Boolean> getRestrictionList(int uid, String restrictionName) throws RemoteException {
			List<Boolean> result = new ArrayList<Boolean>();
			try {
				enforcePermission();

				if (restrictionName == null)
					for (String sRestrictionName : PrivacyManager.getRestrictions())
						result.add(getRestriction(uid, sRestrictionName, null, false));
				else
					for (Hook md : PrivacyManager.getHooks(restrictionName))
						result.add(getRestriction(uid, restrictionName, md.getName(), false));
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
		public long getUsage(int uid, List<String> listRestrictionName, String methodName) throws RemoteException {
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
					for (String restrictionName : listRestrictionName) {
						if (methodName == null)
							try {
								synchronized (stmtGetUsageRestriction) {
									stmtGetUsageRestriction.clearBindings();
									stmtGetUsageRestriction.bindLong(1, uid);
									stmtGetUsageRestriction.bindString(2, restrictionName);
									lastUsage = Math.max(lastUsage, stmtGetUsageRestriction.simpleQueryForLong());
								}
							} catch (SQLiteDoneException ignored) {
							}
						else
							try {
								synchronized (stmtGetUsageMethod) {
									stmtGetUsageMethod.clearBindings();
									stmtGetUsageMethod.bindLong(1, uid);
									stmtGetUsageMethod.bindString(2, restrictionName);
									stmtGetUsageMethod.bindString(3, methodName);
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
		public void setSetting(int uid, String name, String value) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					// Create record
					ContentValues values = new ContentValues();
					values.put("uid", uid);
					values.put("name", name);
					values.put("value", value);

					// Insert/update record
					db.insertWithOnConflict(cTableSetting, null, values, SQLiteDatabase.CONFLICT_REPLACE);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Update cache
				if (mUseCache) {
					CSetting key = new CSetting(uid, name);
					key.setValue(value);
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
		@SuppressLint("DefaultLocale")
		public String getSetting(int uid, String name, String defaultValue) throws RemoteException {
			String value = null;
			try {
				// Check cache
				if (mUseCache) {
					CSetting key = new CSetting(uid, name);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							return mSettingCache.get(key).getValue();
					}
				}

				// No persmissions required
				SQLiteDatabase db = getDatabase();

				// Fallback
				if (db.getVersion() == 1) {
					if (uid == 0)
						value = PrivacyProvider.getSettingFallback(name, null, false);
					if (value == null)
						return PrivacyProvider.getSettingFallback(String.format("%s.%d", name, uid), defaultValue,
								false);
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
							stmtGetSetting.bindLong(1, uid);
							stmtGetSetting.bindString(2, name);
							value = stmtGetSetting.simpleQueryForString();
						}
					} catch (SQLiteDoneException ignored) {
						value = defaultValue;
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Add to cache
				if (mUseCache) {
					CSetting key = new CSetting(uid, name);
					key.setValue(value);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							mSettingCache.remove(key);
						mSettingCache.put(key, key);
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return defaultValue;
			}
			return value;
		}

		@Override
		public void setSettingList(List<ParcelableSetting> listSetting) throws RemoteException {
			for (ParcelableSetting setting : listSetting)
				setSetting(setting.uid, setting.name, setting.value);
		}

		@Override
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public Map getSettingMap(int uid) throws RemoteException {
			Map mapName = new HashMap();
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
								mapName.put(cursor.getString(0), cursor.getString(1));
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
			return mapName;
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
		int uid = Util.getAppId(Binder.getCallingUid());
		if (uid != getXUid())
			throw new SecurityException("uid=" + mXUid + " calling=" + Binder.getCallingUid());
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

			// Set file permission
			Util.setPermission(getDbFile().getParentFile().getAbsolutePath(), 0771, -1, PrivacyManager.cAndroidUid);
			if (getDbFile().exists())
				Util.setPermission(getDbFile().getAbsolutePath(), 0770, -1, PrivacyManager.cAndroidUid);
			File journal = new File(getDbFile() + "-journal");
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

				if (dbFile.exists())
					Util.setPermission(dbFile.getAbsolutePath(), 0775, -1, PrivacyManager.cAndroidUid);
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

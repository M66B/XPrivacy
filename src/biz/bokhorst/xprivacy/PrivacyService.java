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
	private static IPrivacyService mClient = null;
	private static SQLiteDatabase mDatabase = null;
	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

	private static SQLiteStatement stmtGetRestriction = null;
	private static SQLiteStatement stmtGetSetting = null;

	private static int cCurrentVersion = 1;
	private static String cServiceName = "xprivacy";
	private static String cTableRestriction = "restriction";
	private static String cTableUsage = "usage";
	private static String cTableSetting = "setting";

	// TODO: define column names
	// TODO: use precompiled statements

	public static void register() {
		try {
			// public static void addService(String name, IBinder service)
			Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
			Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class);
			mAddService.invoke(null, cServiceName, mPrivacyService);
			Util.log(null, Log.WARN, "Privacy service registered name=" + cServiceName);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	public static IPrivacyService getClient() {
		if (mClient == null)
			try {
				// TODO: retries to get privacy client?
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
		ThreadPolicy oldPolicy = StrictMode.getThreadPolicy();
		ThreadPolicy newpolicy = new ThreadPolicy.Builder(oldPolicy).permitDiskReads().permitDiskWrites().build();
		StrictMode.setThreadPolicy(newpolicy);

		return mClient;
	}

	private static final IPrivacyService.Stub mPrivacyService = new IPrivacyService.Stub() {

		@Override
		public int getVersion() throws RemoteException {
			return cCurrentVersion;
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		@Override
		public boolean getRestriction(final int uid, final String restrictionName, final String methodName,
				final boolean usage) throws RemoteException {
			boolean restricted = false;
			try {
				// No persmissions required
				final SQLiteDatabase db = getDatabase();

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
							synchronized (stmtGetRestriction) {
								stmtGetRestriction.clearBindings();
								stmtGetRestriction.bindLong(1, uid);
								stmtGetRestriction.bindString(2, restrictionName);
								stmtGetRestriction.bindString(3, methodName);
								if (stmtGetRestriction.simpleQueryForLong() > 0)
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

				// Log usage
				if (usage) {
					final boolean sRestricted = restricted;
					mExecutor.execute(new Runnable() {
						public void run() {
							db.beginTransaction();
							try {
								// Category
								ContentValues cvalues = new ContentValues();
								cvalues.put("uid", uid);
								cvalues.put("restriction", restrictionName);
								cvalues.put("method", "");
								cvalues.put("restricted", sRestricted);
								cvalues.put("time", new Date().getTime());
								db.insertWithOnConflict("usage", null, cvalues, SQLiteDatabase.CONFLICT_REPLACE);

								// Method
								if (methodName != null) {
									ContentValues mvalues = new ContentValues();
									mvalues.put("uid", uid);
									mvalues.put("restriction", restrictionName);
									mvalues.put("method", methodName);
									mvalues.put("restricted", sRestricted);
									mvalues.put("time", new Date().getTime());
									db.insertWithOnConflict("usage", null, mvalues, SQLiteDatabase.CONFLICT_REPLACE);
								}
								db.setTransactionSuccessful();
							} catch (Throwable ex) {
								Util.bug(null, ex);
							} finally {
								db.endTransaction();
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
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public List getRestrictionList(int uid, String restrictionName) throws RemoteException {
			List result = new ArrayList();
			try {
				enforcePermission();

				if (restrictionName == null)
					for (String sRestrictionName : PrivacyManager.getRestrictions())
						result.add(getRestriction(uid, sRestrictionName, null, false));
				else
					for (PrivacyManager.Hook md : PrivacyManager.getHooks(restrictionName))
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		// Usage

		@Override
		public long getUsage(int uid, String restrictionName, String methodName) throws RemoteException {
			long lastUsage = 0;
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					Cursor cursor;
					if (methodName == null)
						cursor = db.query(cTableUsage, new String[] { "time" }, "uid=? AND restriction=?",
								new String[] { Integer.toString(uid), restrictionName }, null, null, null);
					else
						cursor = db.query(cTableUsage, new String[] { "time" }, "uid=? AND restriction=? AND method=?",
								new String[] { Integer.toString(uid), restrictionName, methodName }, null, null, null);
					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (usage)");
					else
						try {
							while (cursor.moveToNext()) {
								long usage = cursor.getLong(0);
								if (usage > lastUsage)
									lastUsage = usage;
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
			return lastUsage;
		}

		@Override
		public List<ParcelableUsageData> getUsageList(int uid) throws RemoteException {
			List<ParcelableUsageData> result = new ArrayList<ParcelableUsageData>();
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
								ParcelableUsageData data = new ParcelableUsageData();
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return defaultValue;
			}
			return value;
		}

		@Override
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public Map getSettings(int uid) throws RemoteException {
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
				throw new RemoteException(ex.toString());
			}
		}

		@Override
		public void migrated() throws RemoteException {
			SQLiteDatabase db = getDatabase();
			if (db.getVersion() < 2)
				db.setVersion(2);
		}
	};

	public static void enforcePermission() {
		String self = PrivacyService.class.getPackage().getName();
		String calling = Util.getPackageNameByPid(Binder.getCallingPid());
		if (!self.equals(calling))
			throw new SecurityException("self=" + self + " calling=" + calling);
	}

	private static SQLiteDatabase getDatabase() {
		if (mDatabase == null) {
			File dbFile = new File(Environment.getDataDirectory() + File.separator + "xprivacy" + File.separator
					+ "xprivacy.db");
			dbFile.getParentFile().mkdirs();
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
					Util.log(null, Log.WARN, "Privacy database created");
				} finally {
					db.endTransaction();
				}
			}
			Util.log(null, Log.WARN, "Privacy database version=" + db.getVersion());
			mDatabase = db;
		}
		return mDatabase;
	}
}

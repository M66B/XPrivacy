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

import android.content.ContentValues;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Binder;
import android.os.Environment;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

public class PrivacyService {
	private static IPrivacyService mClient = null;
	private static SQLiteDatabase mDatabase = null;
	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

	private static String cServiceName = "xprivacy";

	// TODO: define table/column names
	// TODO: transactions?
	// TODO: convert shared preferences
	// TODO: error handling (no client, remote exception)

	public static void register() {
		try {
			// public static void addService(String name, IBinder service)
			Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
			Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class);
			mAddService.invoke(null, cServiceName, mPrivacyService);
			Util.log(null, Log.WARN, "Privacy service registered");
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		return mClient;
	}

	private static final IPrivacyService.Stub mPrivacyService = new IPrivacyService.Stub() {

		// Restrictions

		@Override
		public void setRestriction(int uid, String restrictionName, String methodName, boolean restricted)
				throws RemoteException {
			try {
				enforcePermission();
				getDatabase();

				// Create category record
				if (methodName == null || restricted) {
					ContentValues cvalues = new ContentValues();
					cvalues.put("uid", uid);
					cvalues.put("restriction", restrictionName);
					cvalues.put("method", "");
					cvalues.put("restricted", restricted);
					mDatabase.insertWithOnConflict("restriction", null, cvalues, SQLiteDatabase.CONFLICT_REPLACE);
				}

				// Create method record
				if (methodName != null) {
					ContentValues mvalues = new ContentValues();
					mvalues.put("uid", uid);
					mvalues.put("restriction", restrictionName);
					mvalues.put("method", methodName);
					mvalues.put("restricted", !restricted);
					mDatabase.insertWithOnConflict("restriction", null, mvalues, SQLiteDatabase.CONFLICT_REPLACE);
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				// throw new RemoteException(ex.toString());
			}
		}

		@Override
		public boolean getRestriction(final int uid, final String restrictionName, final String methodName,
				final boolean usage) throws RemoteException {
			boolean restricted = false;
			try {
				getDatabase();

				Cursor ccursor = mDatabase.query("restriction", new String[] { "restricted" },
						"uid=? AND restriction=? AND method=?", new String[] { Integer.toString(uid), restrictionName,
								"" }, null, null, null);
				if (ccursor == null)
					Util.log(null, Log.WARN, "Database cursor null (restriction)");
				else
					try {
						if (ccursor.moveToNext())
							restricted = (ccursor.getInt(0) > 0);
					} finally {
						ccursor.close();
					}

				if (restricted && methodName != null) {
					Cursor mcursor = mDatabase.query("restriction", new String[] { "restricted" },
							"uid=? AND restriction=? AND method=?", new String[] { Integer.toString(uid),
									restrictionName, methodName }, null, null, null);
					try {
						// Check method exception
						if (mcursor.moveToNext())
							if (mcursor.getInt(0) > 0)
								restricted = false;
					} finally {
						mcursor.close();
					}
				}

				// Log usage
				if (usage) {
					final boolean sRestricted = restricted;
					mExecutor.execute(new Runnable() {
						public void run() {
							// Category
							ContentValues cvalues = new ContentValues();
							cvalues.put("uid", uid);
							cvalues.put("restriction", restrictionName);
							cvalues.put("method", "");
							cvalues.put("restricted", sRestricted);
							cvalues.put("time", new Date().getTime());
							mDatabase.insertWithOnConflict("usage", null, cvalues, SQLiteDatabase.CONFLICT_REPLACE);

							// Method
							if (methodName != null) {
								ContentValues mvalues = new ContentValues();
								mvalues.put("uid", uid);
								mvalues.put("restriction", restrictionName);
								mvalues.put("method", methodName);
								mvalues.put("restricted", sRestricted);
								mvalues.put("time", new Date().getTime());
								mDatabase.insertWithOnConflict("usage", null, mvalues, SQLiteDatabase.CONFLICT_REPLACE);
							}
						}
					});
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return restricted;
		}

		@Override
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public List getRestrictionList(int uid, String restrictionName) throws RemoteException {
			List result = new ArrayList();
			if (restrictionName == null)
				for (String sRestrictionName : PrivacyManager.getRestrictions())
					result.add(getRestriction(uid, sRestrictionName, null, false));
			else
				for (PrivacyManager.MethodDescription md : PrivacyManager.getMethods(restrictionName))
					result.add(getRestriction(uid, restrictionName, md.getName(), false));
			return result;
		}

		@Override
		public void deleteRestrictions(int uid) throws RemoteException {
			try {
				enforcePermission();
				getDatabase();
				mDatabase.beginTransaction();
				try {
					mDatabase.delete("restriction", "uid=?", new String[] { Integer.toString(uid) });
					mDatabase.setTransactionSuccessful();
					Util.log(null, Log.WARN, "Restrictions deleted uid=" + uid);
				} finally {
					mDatabase.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		// Usage

		@Override
		public long getUsage(int uid, String restrictionName, String methodName) throws RemoteException {
			long lastUsage = 0;
			boolean restricted = false;
			try {
				getDatabase();

				Cursor cursor;
				if (methodName == null)
					cursor = mDatabase.query("usage", new String[] { "restricted", "time" }, "uid=? AND restriction=?",
							new String[] { Integer.toString(uid), restrictionName }, null, null, null);
				else
					cursor = mDatabase.query("usage", new String[] { "restricted", "time" },
							"uid=? AND restriction=? AND method=?", new String[] { Integer.toString(uid),
									restrictionName, methodName }, null, null, null);
				if (cursor == null)
					Util.log(null, Log.WARN, "Database cursor null (usage)");
				else
					try {
						while (cursor.moveToNext()) {
							restricted = (restricted || cursor.getInt(0) > 0);
							long usage = cursor.getLong(1);
							if (usage > lastUsage)
								lastUsage = usage;
						}
					} finally {
						cursor.close();
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return lastUsage * (restricted ? 1 : -1);
		}

		@Override
		public List<ParcelableUsageData> getUsageList(int uid) throws RemoteException {
			List<ParcelableUsageData> result = new ArrayList<ParcelableUsageData>();
			try {
				getDatabase();

				Cursor cursor;
				if (uid == 0)
					cursor = mDatabase.query("usage", new String[] { "uid", "restriction", "method", "restricted",
							"time" }, null, new String[] {}, null, null, null);
				else
					cursor = mDatabase.query("usage", new String[] { "uid", "restriction", "method", "restricted",
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return result;
		}

		@Override
		public void deleteUsage(int uid) throws RemoteException {
			try {
				enforcePermission();
				getDatabase();
				mDatabase.beginTransaction();
				try {
					mDatabase.delete("usage", "uid=?", new String[] { Integer.toString(uid) });
					mDatabase.setTransactionSuccessful();
					Util.log(null, Log.WARN, "Usage data deleted uid=" + uid);
				} finally {
					mDatabase.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		// Settings

		@Override
		public void setSetting(int uid, String name, String value) throws RemoteException {
			try {
				enforcePermission();
				getDatabase();

				// Create record
				ContentValues values = new ContentValues();
				values.put("uid", uid);
				values.put("name", name);
				values.put("value", value);

				// Insert/update record
				mDatabase.insertWithOnConflict("setting", null, values, SQLiteDatabase.CONFLICT_REPLACE);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				// throw new RemoteException(ex.toString());
			}
		}

		@Override
		public String getSetting(int uid, String name, String defaultValue) throws RemoteException {
			String value = null;
			try {
				getDatabase();

				Cursor cursor = mDatabase.query("setting", new String[] { "value" }, "uid=? AND name=?", new String[] {
						Integer.toString(uid), name }, null, null, null);
				if (cursor == null)
					Util.log(null, Log.WARN, "Database cursor null (setting)");
				else
					try {
						if (cursor.moveToNext()) {
							value = cursor.getString(0);
							if (value.equals("") && defaultValue != null)
								value = defaultValue;
						} else
							value = defaultValue;
					} finally {
						cursor.close();
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return value;
		}

		@Override
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public Map getSettings(int uid) throws RemoteException {
			Map mapName = new HashMap();
			try {
				getDatabase();

				Cursor cursor = mDatabase.query("setting", new String[] { "name", "value" }, "uid=?",
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
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return mapName;
		}

		@Override
		public void deleteSettings(int uid) throws RemoteException {
			try {
				enforcePermission();
				getDatabase();
				mDatabase.beginTransaction();
				try {
					mDatabase.delete("setting", "uid=?", new String[] { Integer.toString(uid) });
					mDatabase.setTransactionSuccessful();
					Util.log(null, Log.WARN, "Settings deleted uid=" + uid);
				} finally {
					mDatabase.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}
	};

	public static void enforcePermission() {
		String self = PrivacyService.class.getPackage().getName();
		String calling = Util.getPackageNameByPid(Binder.getCallingPid());
		if (!self.equals(calling))
			throw new SecurityException("self=" + self + " calling=" + calling);
	}

	private static void getDatabase() {
		if (mDatabase == null) {
			File dbFile = new File(Environment.getDataDirectory() + File.separator + "xprivacy" + File.separator
					+ "xprivacy.db");
			dbFile.getParentFile().mkdirs();
			SQLiteDatabase db = SQLiteDatabase.openOrCreateDatabase(dbFile, null);
			// TODO: handle corrupt database
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
					Util.log(null, Log.WARN, "Privacy database created version=" + db.getVersion());
				} finally {
					db.endTransaction();
				}
			} else
				Util.log(null, Log.WARN, "Privacy database version=" + db.getVersion());
			mDatabase = db;
		}
	}
}

package biz.bokhorst.xprivacy;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Date;

import android.content.ContentValues;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Environment;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

public class PrivacyService {
	private static IPrivacyService mClient = null;
	private static SQLiteDatabase mDatabase = null;

	private static String cServiceName = "xprivacy";

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
		// TODO: transactions

		@Override
		public void setRestricted(String hookName, int uid, String restrictionName, String methodName,
				boolean restricted) throws RemoteException {
			try {
				getDatabase();

				// Create record
				ContentValues values = new ContentValues();
				values.put("uid", uid);
				values.put("restriction", restrictionName);
				values.put("method", methodName == null ? "" : methodName);
				values.put("restricted", restricted);

				// Insert/update record
				mDatabase.insertWithOnConflict("restriction", null, values, SQLiteDatabase.CONFLICT_REPLACE);

				Util.log(null, Log.WARN, "Service set hook=" + hookName + " uid=" + uid + " restriction="
						+ restrictionName + "/" + methodName + "=" + restricted);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				// throw new RemoteException(ex.toString());
			}
		}

		@Override
		public boolean getRestricted(String hookName, int uid, String restrictionName, String methodName, boolean usage)
				throws RemoteException {
			boolean restricted = false;
			try {
				getDatabase();

				Cursor cursor = mDatabase.query("restriction", new String[] { "restricted" },
						"uid=? AND restriction=? AND method=?", new String[] { Integer.toString(uid), restrictionName,
								methodName == null ? "" : methodName }, null, null, null);
				if (cursor == null)
					Util.log(null, Log.WARN, "Database cursor null (restriction)");
				else
					try {
						if (cursor.moveToNext()) {
							restricted = (cursor.getInt(0) > 0);
							Util.log(null, Log.WARN, "Service get hook=" + hookName + " uid=" + uid + " restriction="
									+ restrictionName + "/" + methodName + "=" + restricted);
						}
					} finally {
						cursor.close();
					}

				// Log usage
				if (usage) {
					ContentValues values = new ContentValues();
					values.put("uid", uid);
					values.put("restriction", restrictionName);
					values.put("method", methodName == null ? "" : methodName);
					values.put("restricted", restricted);
					values.put("time", new Date().getTime());
					mDatabase.insertWithOnConflict("usage", null, values, SQLiteDatabase.CONFLICT_REPLACE);
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return restricted;
		}

		@Override
		public void setSetting(String hookName, int uid, String name, String value) throws RemoteException {
			try {
				getDatabase();

				// Create record
				ContentValues values = new ContentValues();
				values.put("uid", uid);
				values.put("name", name);
				values.put("value", value);

				// Insert/update record
				mDatabase.insertWithOnConflict("setting", null, values, SQLiteDatabase.CONFLICT_REPLACE);

				Util.log(null, Log.WARN, "Service set hook=" + hookName + " uid=" + uid + " " + name + "=" + value);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				// throw new RemoteException(ex.toString());
			}
		}

		@Override
		public String getSetting(String hookName, int uid, String name, String defaultValue) throws RemoteException {
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
							Util.log(null, Log.WARN, "Service get hook=" + hookName + " uid=" + uid + " " + name + "="
									+ value);
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
	};

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
					Util.log(null, Log.WARN, "Privacy database created");
				} finally {
					db.endTransaction();
				}
			} else
				Util.log(null, Log.WARN, "Privacy database version=" + db.getVersion());
			mDatabase = db;
		}
	}
}

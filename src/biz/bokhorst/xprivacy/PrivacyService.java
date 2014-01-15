package biz.bokhorst.xprivacy;

import java.io.File;
import java.lang.reflect.Method;

import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.os.Environment;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

public class PrivacyService {
	private static String cServiceName = "xprivacy";
	private static SQLiteDatabase mDatabase = null;

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
		try {
			// public static IBinder getService(String name)
			Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
			Method mGetService = cServiceManager.getDeclaredMethod("getService", String.class);
			return IPrivacyService.Stub.asInterface((IBinder) mGetService.invoke(null, cServiceName));
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return null;
		}
	}

	private static final IPrivacyService.Stub mPrivacyService = new IPrivacyService.Stub() {
		@Override
		public boolean getRestricted(String hookName, int uid, String restrictionName, String methodName,
				boolean usage, boolean useCache) throws RemoteException {
			if (mDatabase == null)
				mDatabase = getDatabase();
			return PrivacyProvider.getRestrictedFallback(null, uid, restrictionName, methodName);
		}

		@Override
		public String getSetting(String hookName, int uid, String name, String defaultValue, boolean useCache)
				throws RemoteException {
			return PrivacyProvider.getSettingFallback(name, defaultValue, true);
		}

		@Override
		public boolean setRestricted(String hook, int uid, String restrictionName, String methodName, boolean restricted)
				throws RemoteException {
			return false;
		}

		@Override
		public void setSetting(String hookName, int uid, String settingName, String value) throws RemoteException {
		}

	};

	private static SQLiteDatabase getDatabase() {
		try {
			File dbFile = new File(Environment.getDataDirectory() + File.separator + "xprivacy" + File.separator
					+ "xprivacy.db");
			dbFile.getParentFile().mkdirs();
			SQLiteDatabase db = SQLiteDatabase.openOrCreateDatabase(dbFile, null);
			if (db.needUpgrade(1))
				try {
					db.beginTransaction();
					// http://www.sqlite.org/lang_createtable.html
					db.execSQL("CREATE TABLE restriction (uid INTEGER, restriction TEXT, method TEXT, restricted INTEGER)");
					db.execSQL("CREATE TABLE setting (uid INTEGER, name TEXT, value TEXT)");
					db.execSQL("CREATE TABLE usage (uid INTEGER, restriction TEXT, method TEXT, time INTEGER, restricted INTEGER)");
					db.setVersion(1);
					db.setTransactionSuccessful();
					Util.log(null, Log.WARN, "Database created");
				} finally {
					db.endTransaction();
				}
			return db;
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return null;
		}
	}
}

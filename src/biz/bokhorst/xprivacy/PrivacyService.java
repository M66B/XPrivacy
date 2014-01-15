package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;

import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

public class PrivacyService {
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
		public boolean getRestricted(int uid, String restrictionName, String methodName) throws RemoteException {
			return PrivacyProvider.getRestrictedFallback(null, uid, restrictionName, methodName);
		}

		@Override
		public String getSetting(String name, String defaultValue) throws RemoteException {
			return PrivacyProvider.getSettingFallback(name, defaultValue, true);
		}
	};
}

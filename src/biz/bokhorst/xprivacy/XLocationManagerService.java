package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.content.Context;
import android.location.Location;
import android.os.Binder;
import android.os.Build;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Process;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XLocationManagerService extends XHook {
	private Methods mMethod;
	private static boolean mHooked = false;
	private static Map<IBinder, Integer> mListenerUid = new WeakHashMap<IBinder, Integer>();

	private XLocationManagerService(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()));
		mMethod = method;
	}

	private XLocationManagerService(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()), sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.server.LocationManagerService";
	}

	// @formatter:off

	// public boolean addGpsStatusListener(IGpsStatusListener listener, String packageName)
	// public Location getLastLocation(LocationRequest request, String packageName)
	// public List<String> getProviders(Criteria criteria, boolean enabledOnly)
	// public List<String> getAllProviders()
	// public String getBestProvider(Criteria criteria, boolean enabledOnly)
	// public boolean isProviderEnabled(String provider)
	// public void removeUpdates(ILocationListener listener, PendingIntent intent, String packageName)
	// public void requestGeofence(LocationRequest request, Geofence geofence, PendingIntent intent, String packageName)
	// public void requestLocationUpdates(LocationRequest request, ILocationListener listener, PendingIntent intent, String packageName)
	// public boolean sendExtraCommand(String provider, String command, Bundle extras)
	// frameworks/base/services/java/com/android/server/LocationManagerService.java
	// http://developer.android.com/reference/android/location/LocationManager.html
	
	// @formatter:on

	private enum Methods {
		addGpsStatusListener, getLastLocation, getLastKnownLocation, getProviders, getAllProviders, getBestProvider, isProviderEnabled, removeUpdates, requestGeofence, requestLocationUpdates, sendExtraCommand
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods loc : Methods.values())
			if (loc != Methods.requestGeofence)
				listHook.add(new XLocationManagerService(loc, loc == Methods.removeUpdates ? null
						: PrivacyManager.cLocation));
		listHook.add(new XLocationManagerService(Methods.requestGeofence, PrivacyManager.cLocation,
				Build.VERSION_CODES.JELLY_BEAN_MR1));
		return listHook;
	}

	@Override
	protected void before(final MethodHookParam param) throws Throwable {
		if (mMethod == Methods.addGpsStatusListener) {
			if (isRestricted(param))
				param.setResult(false);
		} else if (mMethod == Methods.requestGeofence) {
			if (isRestricted(param))
				param.setResult(null);
		} else if (mMethod == Methods.removeUpdates) {
			if (param.args.length > 0 && param.args[0] != null)
				if (param.args[0].getClass().getName().startsWith("android.location.ILocationListener")) {
					// Remove listener
					IBinder binder = ((IInterface) param.args[0]).asBinder();
					synchronized (mListenerUid) {
						if (mListenerUid.containsKey(binder))
							mListenerUid.remove(binder);
						else
							Util.log(this, Log.WARN, "Remove: listener not found");
					}
				}
		} else if (mMethod == Methods.requestLocationUpdates) {
			if (param.args.length > 1 && param.args[1] != null) {
				if (param.args[1].getClass().getName().startsWith("android.location.ILocationListener")) {
					// Add listener
					int uid = Binder.getCallingUid();
					IBinder binder = ((IInterface) param.args[1]).asBinder();
					synchronized (mListenerUid) {
						mListenerUid.put(binder, uid);
					}

					// Hook
					if (!mHooked) {
						hookOnLocationChanged(param);
						mHooked = true;
					}
				} else
					Util.log(this, Log.WARN, "Not hooking class=" + param.args[1].getClass().getName());
			} else if (param.args.length > 2 && param.args[2] != null) {
				// TODO: hook intent
				if (isRestricted(param))
					param.setResult(null);
			}
		}
	}

	private void hookOnLocationChanged(final MethodHookParam param) {
		try {
			// void onLocationChanged(Location location)
			Method on = param.args[1].getClass().getDeclaredMethod("onLocationChanged", Location.class);
			XposedBridge.hookMethod(on, new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam onparam) throws Throwable {
					// Get uid
					int uid = 0;
					IBinder binder = ((IInterface) onparam.thisObject).asBinder();
					synchronized (mListenerUid) {
						if (mListenerUid.containsKey(binder))
							uid = mListenerUid.get(binder);
						else
							Util.log(XLocationManagerService.this, Log.WARN, "Get: listener not found");
					}

					// Restrict
					if (uid > 0)
						if (onparam.args.length > 0 && onparam.args[0] != null) {
							Location location = (Location) onparam.args[0];
							if (location != null && isRestricted(param, uid))
								onparam.args[0] = PrivacyManager.getDefacedLocation(uid, location);
						}
				}
			});
			Util.log(this, Log.WARN, "Hooked " + on + " uid=" + Process.myUid());
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod != Methods.addGpsStatusListener && mMethod != Methods.requestGeofence
				&& mMethod != Methods.removeUpdates && mMethod != Methods.requestLocationUpdates)
			if (mMethod == Methods.getLastLocation || mMethod == Methods.getLastKnownLocation) {
				Location location = (Location) param.getResult();
				if (location != null && isRestricted(param))
					param.setResult(PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location));
			} else if (mMethod == Methods.isProviderEnabled) {
				if (isRestricted(param))
					param.setResult(false);
			} else if (mMethod == Methods.getProviders || mMethod == Methods.getAllProviders) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<String>());
			} else if (mMethod == Methods.getBestProvider) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(null);
			} else if (mMethod == Methods.sendExtraCommand) {
				if (isRestricted(param))
					param.setResult(false);
			} else
				Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		int uid = Binder.getCallingUid();
		return isRestricted(param, uid);
	}

	private boolean isRestricted(MethodHookParam param, int uid) throws Throwable {
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
		return getRestricted(context, uid, true);
	}
}

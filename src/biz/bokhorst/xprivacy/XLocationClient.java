package biz.bokhorst.xprivacy;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;

import android.location.Location;
import android.os.Binder;
import android.util.Log;

public class XLocationClient extends XHook {
	private Methods mMethod;

	private XLocationClient(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("GMS.%s", method.name()));
		mMethod = method;
	}

	private XLocationClient(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), String.format("GMS.%s", method.name()), sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "com.google.android.gms.location.LocationClient";
	}

	// @formatter:off

	// void addGeofences(List<Geofence> geofences, PendingIntent pendingIntent, LocationClient.OnAddGeofencesResultListener listener)
	// Location getLastLocation()
	// void removeGeofences(List<String> geofenceRequestIds, LocationClient.OnRemoveGeofencesResultListener listener)
	// void removeGeofences(PendingIntent pendingIntent, LocationClient.OnRemoveGeofencesResultListener listener)
	// void requestLocationUpdates(LocationRequest request, PendingIntent callbackIntent)
	// void requestLocationUpdates(LocationRequest request, LocationListener listener)
	// void requestLocationUpdates(LocationRequest request, LocationListener listener, Looper looper)
	// https://developer.android.com/reference/com/google/android/gms/location/LocationClient.html

	// @formatter:on

	private enum Methods {
		addGeofences, getLastLocation, removeGeofences, requestLocationUpdates
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XLocationClient(Methods.addGeofences, PrivacyManager.cLocation).optional());
		listHook.add(new XLocationClient(Methods.getLastLocation, PrivacyManager.cLocation).optional());
		listHook.add(new XLocationClient(Methods.removeGeofences, null, 1).optional());
		listHook.add(new XLocationClient(Methods.requestLocationUpdates, PrivacyManager.cLocation).optional());
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.addGeofences) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.removeGeofences) {
			if (isRestricted(param, PrivacyManager.cLocation, "GMS.addGeofences"))
				param.setResult(null);

		} else if (mMethod == Methods.getLastLocation) {
			// Do nothing

		} else if (mMethod == Methods.requestLocationUpdates) {
			if (isRestricted(param)) {
				ClassLoader cl = param.thisObject.getClass().getClassLoader();
				Class<?> ll = Class.forName("com.google.android.gms.location.LocationListener", false, cl);
				InvocationHandler ih = new OnLocationChangedHandler(Binder.getCallingUid(), param.args[1]);
				Object proxy = Proxy.newProxyInstance(cl, new Class<?>[] { ll }, ih);
				param.args[1] = proxy;
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.addGeofences || mMethod == Methods.removeGeofences) {
			// Do nothing

		} else if (mMethod == Methods.getLastLocation) {
			Location location = (Location) param.getResult();
			if (location != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location));

		} else if (mMethod == Methods.requestLocationUpdates) {
			// Do nothing

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	private class OnLocationChangedHandler implements InvocationHandler {
		private int mUid;
		private Object mTarget;

		public OnLocationChangedHandler(int uid, Object target) {
			mUid = uid;
			mTarget = target;
		}

		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if ("onLocationChanged".equals(method.getName()))
				args[0] = PrivacyManager.getDefacedLocation(mUid, (Location) args[0]);
			return method.invoke(mTarget, args);
		}
	}
}

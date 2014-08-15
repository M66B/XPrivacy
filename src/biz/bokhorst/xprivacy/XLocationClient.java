package biz.bokhorst.xprivacy;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.app.PendingIntent;
import android.location.Location;
import android.os.Binder;
import android.util.Log;

public class XLocationClient extends XHook {
	private Methods mMethod;
	private static final Map<Object, Object> mMapProxy = new WeakHashMap<Object, Object>();

	private XLocationClient(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("GMS.%s", method.name()));
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
	// void removeLocationUpdates(LocationListener listener)
	// void removeLocationUpdates(PendingIntent callbackIntent)
	// void requestLocationUpdates(LocationRequest request, PendingIntent callbackIntent)
	// void requestLocationUpdates(LocationRequest request, LocationListener listener)
	// void requestLocationUpdates(LocationRequest request, LocationListener listener, Looper looper)
	// https://developer.android.com/reference/com/google/android/gms/location/LocationClient.html

	// @formatter:on

	private enum Methods {
		addGeofences, getLastLocation, removeGeofences, removeLocationUpdates, requestLocationUpdates
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XLocationClient(Methods.addGeofences, PrivacyManager.cLocation));
		listHook.add(new XLocationClient(Methods.getLastLocation, PrivacyManager.cLocation));
		listHook.add(new XLocationClient(Methods.removeGeofences, null));
		listHook.add(new XLocationClient(Methods.removeLocationUpdates, null));
		listHook.add(new XLocationClient(Methods.requestLocationUpdates, PrivacyManager.cLocation));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.addGeofences) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.getLastLocation) {
			// Do nothing

		} else if (mMethod == Methods.removeGeofences) {
			if (isRestricted(param, PrivacyManager.cLocation, "GMS.addGeofences"))
				param.setResult(null);

		} else if (mMethod == Methods.removeLocationUpdates) {
			if (param.args.length > 0)
				if (param.args[0] instanceof PendingIntent) {
					if (isRestricted(param, PrivacyManager.cLocation, "GMS.requestLocationUpdates"))
						param.setResult(null);
				} else
					synchronized (mMapProxy) {
						if (mMapProxy.containsKey(param.args[0]))
							param.args[0] = mMapProxy.get(param.args[0]);
					}

		} else if (mMethod == Methods.requestLocationUpdates) {
			if (param.args.length > 1)
				if (isRestricted(param))
					if (param.args[1] instanceof PendingIntent)
						param.setResult(null);
					else if (param.thisObject != null && param.args[1] != null) {
						// Create proxy
						ClassLoader cl = param.thisObject.getClass().getClassLoader();
						Class<?> ll = Class.forName("com.google.android.gms.location.LocationListener", false, cl);
						InvocationHandler ih = new OnLocationChangedHandler(Binder.getCallingUid(), param.args[1]);
						Object proxy = Proxy.newProxyInstance(cl, new Class<?>[] { ll }, ih);

						// Use proxy
						synchronized (mMapProxy) {
							mMapProxy.put(param.args[1], proxy);
						}
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

		} else if (mMethod == Methods.removeLocationUpdates) {
			// Do nothing

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

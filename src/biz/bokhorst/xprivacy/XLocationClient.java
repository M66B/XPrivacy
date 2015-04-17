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
		Util.log(null, Log.WARN, "Hooking LocationClient uid=" + Binder.getCallingUid());

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
		switch (mMethod) {
		case addGeofences:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case getLastLocation:
			// Do nothing
			break;

		case removeGeofences:
			if (isRestricted(param, PrivacyManager.cLocation, "GMS.addGeofences"))
				param.setResult(null);
			break;

		case removeLocationUpdates:
			if (param.args.length > 0)
				if (param.args[0] instanceof PendingIntent) {
					if (isRestricted(param, PrivacyManager.cLocation, "GMS.requestLocationUpdates"))
						param.setResult(null);
				} else if (param.args[0] != null)
					synchronized (mMapProxy) {
						if (mMapProxy.containsKey(param.args[0]))
							param.args[0] = mMapProxy.get(param.args[0]);
					}
			break;

		case requestLocationUpdates:
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
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case addGeofences:
		case removeGeofences:
			// Do nothing
			break;

		case getLastLocation:
			Location location = (Location) param.getResult();
			if (location != null)
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location));
			break;

		case removeLocationUpdates:
			// Do nothing
			break;

		case requestLocationUpdates:
			// Do nothing
			break;

		}
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

package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
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
import android.os.IInterface;
import android.location.GpsSatellite;
import android.location.GpsStatus;

public class XLocationManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.location.LocationManager";
	private static final Map<Object, Object> mMapProxy = new WeakHashMap<Object, Object>();

	private XLocationManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
		mClassName = className;
	}

	private XLocationManager(Methods method, String restrictionName, String className, int sdk) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name(), sdk);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public void addGeofence(LocationRequest request, Geofence fence, PendingIntent intent)
	// public boolean addGpsStatusListener(GpsStatus.Listener listener)
	// public boolean addNmeaListener(GpsStatus.NmeaListener listener)
	// public void addProximityAlert(double latitude, double longitude, float radius, long expiration, PendingIntent intent)
	// public List<String> getAllProviders()
	// public String getBestProvider(Criteria criteria, boolean enabledOnly)
	// public GpsStatus getGpsStatus(GpsStatus status)
	// public Location getLastKnownLocation(String provider)
	// public List<String> getProviders(boolean enabledOnly)
	// public List<String> getProviders(Criteria criteria, boolean enabledOnly)
	// public boolean isProviderEnabled(String provider)
	// public void removeUpdates(LocationListener listener)
	// public void removeUpdates(PendingIntent intent)
	// public void requestLocationUpdates(String provider, long minTime, float minDistance, LocationListener listener)
	// public void requestLocationUpdates(String provider, long minTime, float minDistance, LocationListener listener, Looper looper)
	// public void requestLocationUpdates(long minTime, float minDistance, Criteria criteria, LocationListener listener, Looper looper)
	// public void requestLocationUpdates(String provider, long minTime, float minDistance, PendingIntent intent)
	// public void requestLocationUpdates(long minTime, float minDistance, Criteria criteria, PendingIntent intent)
	// public void requestSingleUpdate(String provider, LocationListener listener, Looper looper)
	// public void requestSingleUpdate(Criteria criteria, LocationListener listener, Looper looper)
	// public void requestSingleUpdate(String provider, PendingIntent intent)
	// public void requestSingleUpdate(Criteria criteria, PendingIntent intent)
	// public boolean sendExtraCommand(String provider, String command, Bundle extras)
	// frameworks/base/location/java/android/location/LocationManager.java
	// http://developer.android.com/reference/android/location/LocationManager.html

	// public void requestLocationUpdates(LocationRequest request, ILocationListener listener, android.app.PendingIntent intent, java.lang.String packageName)
	// public void removeUpdates(ILocationListener listener, android.app.PendingIntent intent, java.lang.String packageName)
	// public void requestGeofence(LocationRequest request, Geofence geofence, android.app.PendingIntent intent, java.lang.String packageName)
	// public void removeGeofence(Geofence fence, android.app.PendingIntent intent, java.lang.String packageName)
	// public Location getLastLocation(LocationRequest request, java.lang.String packageName)
	// public boolean addGpsStatusListener(IGpsStatusListener listener, java.lang.String packageName)
	// public void removeGpsStatusListener(IGpsStatusListener listener)
	// public java.util.List<java.lang.String> getAllProviders()
	// public java.util.List<java.lang.String> getProviders(Criteria criteria, boolean enabledOnly)
	// public java.lang.String getBestProvider(Criteria criteria, boolean enabledOnly)
	// public boolean isProviderEnabled(java.lang.String provider)
	// public boolean sendExtraCommand(java.lang.String provider, java.lang.String command, android.os.Bundle extras)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.4_r1/com/android/server/LocationManagerService.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		addGeofence, addGpsStatusListener, addNmeaListener, addProximityAlert,
		getAllProviders, getBestProvider, getProviders, isProviderEnabled,
		getGpsStatus,
		getLastKnownLocation,
		removeUpdates,
		requestLocationUpdates, requestSingleUpdate,
		sendExtraCommand,

		Srv_requestLocationUpdates, Srv_removeUpdates,
		Srv_requestGeofence, Srv_removeGeofence,
		Srv_getLastLocation,
		Srv_addGpsStatusListener, Srv_removeGpsStatusListener,
		Srv_getAllProviders, Srv_getProviders, Srv_getBestProvider, Srv_isProviderEnabled,
		Srv_sendExtraCommand
		// TODO: addNmeaListener
		// TODO: addProximityAlert
		// TODO: getGpsStatus
	};
	// @formatter:on

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			for (Methods loc : Methods.values())
				if (loc == Methods.removeUpdates)
					listHook.add(new XLocationManager(loc, null, className, 3));
				else if (loc.name().startsWith("Srv_remove"))
					listHook.add(new XLocationManager(loc, null, "com.android.server.LocationManagerService", 19));
				else if (loc.name().startsWith("Srv_"))
					listHook.add(new XLocationManager(loc, PrivacyManager.cLocation,
							"com.android.server.LocationManagerService"));
				else
					listHook.add(new XLocationManager(loc, PrivacyManager.cLocation, className));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case addGeofence:
		case addProximityAlert:
		case Srv_requestGeofence:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case Srv_removeGeofence:
			if (isRestricted(param, PrivacyManager.cLocation, "Srv_requestGeofence"))
				param.setResult(null);
			break;

		case addGpsStatusListener:
		case addNmeaListener:
		case Srv_addGpsStatusListener:
			if (isRestricted(param))
				param.setResult(false);
			break;

		case Srv_removeGpsStatusListener:
			if (isRestricted(param, PrivacyManager.cLocation, "Srv_addGpsStatusListener"))
				param.setResult(null);
			break;

		case getAllProviders:
		case getBestProvider:
		case getGpsStatus:
		case getLastKnownLocation:
		case getProviders:
		case isProviderEnabled:
		case Srv_getAllProviders:
		case Srv_getProviders:
		case Srv_getBestProvider:
		case Srv_isProviderEnabled:
		case Srv_getLastLocation:
			// Do nothing
			break;

		case removeUpdates:
			if (isRestricted(param, PrivacyManager.cLocation, "requestLocationUpdates"))
				removeLocationListener(param, 0);
			break;

		case requestLocationUpdates:
			if (param.args.length > 0 && param.args[0] instanceof String) {
				if (isRestrictedExtra(param, (String) param.args[0]))
					replaceLocationListener(param, 3);
			} else {
				if (isRestricted(param))
					replaceLocationListener(param, 3);
			}
			break;

		case Srv_removeUpdates:
			if (isRestricted(param, PrivacyManager.cLocation, "Srv_requestLocationUpdates"))
				removeLocationListener(param, 0);
			break;

		case Srv_requestLocationUpdates:
			if (isRestricted(param))
				replaceLocationListener(param, 1);
			break;

		case requestSingleUpdate:
			if (param.args.length > 0 && param.args[0] instanceof String) {
				if (isRestrictedExtra(param, (String) param.args[0]))
					replaceLocationListener(param, 1);
			} else {
				if (isRestricted(param))
					replaceLocationListener(param, 1);
			}
			break;

		case sendExtraCommand:
		case Srv_sendExtraCommand:
			// Do nothing
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case addGeofence:
		case addNmeaListener:
		case addGpsStatusListener:
		case addProximityAlert:
		case Srv_requestGeofence:
		case Srv_addGpsStatusListener:
		case Srv_removeGeofence:
		case Srv_removeGpsStatusListener:
			// Do nothing
			break;

		case isProviderEnabled:
		case Srv_isProviderEnabled:
			if (param.args.length > 0) {
				String provider = (String) param.args[0];
				if (isRestrictedExtra(param, provider))
					param.setResult(false);
			}
			break;

		case getGpsStatus:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					GpsStatus status = (GpsStatus) param.getResult();
					// private GpsSatellite mSatellites[]
					try {
						Field mSatellites = status.getClass().getDeclaredField("mSatellites");
						mSatellites.setAccessible(true);
						mSatellites.set(status, new GpsSatellite[0]);
					} catch (Throwable ex) {
						Util.bug(null, ex);
					}
				}
			break;

		case getProviders:
		case getAllProviders:
		case Srv_getAllProviders:
		case Srv_getProviders:
			if (isRestricted(param))
				param.setResult(new ArrayList<String>());
			break;

		case getBestProvider:
		case Srv_getBestProvider:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(null);
			break;

		case getLastKnownLocation:
			if (param.args.length > 0) {
				String provider = (String) param.args[0];
				Location location = (Location) param.getResult();
				if (location != null && isRestrictedExtra(param, provider))
					param.setResult(PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location));
			}
			break;

		case Srv_getLastLocation:
			if (param.getResult() instanceof Location) {
				Location location = (Location) param.getResult();
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location));
			}
			break;

		case removeUpdates:
		case requestLocationUpdates:
		case requestSingleUpdate:
		case Srv_removeUpdates:
		case Srv_requestLocationUpdates:
			// Do nothing
			break;

		case sendExtraCommand:
		case Srv_sendExtraCommand:
			if (param.args.length > 0) {
				String provider = (String) param.args[0];
				if (isRestrictedExtra(param, provider))
					param.setResult(false);
			}
			break;
		}
	}

	private void replaceLocationListener(XParam param, int arg) throws Throwable {
		if (param.args.length > arg)
			if (param.args[arg] instanceof PendingIntent)
				param.setResult(null);
			else if (param.args[arg] != null && param.thisObject != null) {
				// Create proxy
				ClassLoader cl = param.thisObject.getClass().getClassLoader();
				InvocationHandler ih = new OnLocationChangedHandler(Binder.getCallingUid(), param.args[arg]);
				Object proxy = Proxy.newProxyInstance(cl, new Class<?>[] { param.args[arg].getClass() }, ih);

				Object key = param.args[arg];
				if (key instanceof IInterface)
					key = ((IInterface) key).asBinder();

				// Use proxy
				synchronized (mMapProxy) {
					mMapProxy.put(key, proxy);
				}
				param.args[arg] = proxy;
			}
	}

	private void removeLocationListener(XParam param, int arg) {
		if (param.args.length > arg)
			if (param.args[arg] instanceof PendingIntent)
				param.setResult(null);
			else if (param.args[arg] != null)
				synchronized (mMapProxy) {
					Object key = param.args[arg];
					if (key instanceof IInterface)
						key = ((IInterface) key).asBinder();
					if (mMapProxy.containsKey(key))
						param.args[arg] = mMapProxy.get(key);
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

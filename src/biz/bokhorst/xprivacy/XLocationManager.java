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
import android.os.Bundle;
import android.os.IInterface;
import android.util.Log;
import android.location.GpsSatellite;
import android.location.GpsStatus;
import android.location.LocationListener;

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
	// public boolean addGpsMeasurementsListener(IGpsMeasurementsListener listener, String packageName)
	// public boolean addGpsNavigationMessageListener(IGpsNavigationMessageListener listener, String packageName)
	// public boolean removeGpsMeasurementsListener(IGpsMeasurementsListener listener)
	// public boolean removeGpsNavigationMessageListener(IGpsNavigationMessageListener listener)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/com/android/server/LocationManagerService.java

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
		Srv_sendExtraCommand,

		Srv_addGpsMeasurementsListener, Srv_addGpsNavigationMessageListener, Srv_removeGpsMeasurementsListener, Srv_removeGpsNavigationMessageListener
	};
	// @formatter:on

	public static List<XHook> getInstances(String className, boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			for (Methods loc : Methods.values())
				if (loc == Methods.removeUpdates)
					listHook.add(new XLocationManager(loc, null, className));
				else if (loc.name().startsWith("Srv_remove")) {
					if (server)
						listHook.add(new XLocationManager(loc, null, "com.android.server.LocationManagerService"));
				} else if (loc.name().startsWith("Srv_")) {
					if (server)
						listHook.add(new XLocationManager(loc, PrivacyManager.cLocation,
								"com.android.server.LocationManagerService"));
				} else
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
		case Srv_addGpsMeasurementsListener:
		case Srv_addGpsNavigationMessageListener:
			if (isRestricted(param))
				param.setResult(false);
			break;

		case Srv_removeGpsStatusListener:
			if (isRestricted(param, PrivacyManager.cLocation, "Srv_addGpsStatusListener"))
				param.setResult(null);
			break;

		case Srv_removeGpsMeasurementsListener:
			if (isRestricted(param, PrivacyManager.cLocation, "Srv_addGpsMeasurementsListener"))
				param.setResult(null);
			break;

		case Srv_removeGpsNavigationMessageListener:
			if (isRestricted(param, PrivacyManager.cLocation, "Srv_addGpsNavigationMessageListener"))
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
				unproxyLocationListener(param, 0, true);
			break;

		case requestLocationUpdates:
			if (param.args.length > 0 && param.args[0] instanceof String) {
				if (isRestrictedExtra(param, (String) param.args[0]))
					proxyLocationListener(param, 3, LocationListener.class, true);
			} else {
				if (isRestricted(param))
					proxyLocationListener(param, 3, LocationListener.class, true);
			}
			break;

		case Srv_removeUpdates:
			if (isRestricted(param, PrivacyManager.cLocation, "Srv_requestLocationUpdates"))
				if (param.args.length > 1)
					if (param.args[0] != null) // ILocationListener
						unproxyLocationListener(param, 0, false);
					else if (param.args[1] != null) // PendingIntent
						param.setResult(null);
			break;

		case Srv_requestLocationUpdates:
			if (isRestricted(param))
				if (param.args.length > 2)
					if (param.args[1] != null) // ILocationListener
						proxyLocationListener(param, 1, Class.forName("android.location.ILocationListener"), false);
					else if (param.args[2] != null) // PendingIntent
						param.setResult(null);
			break;

		case requestSingleUpdate:
			if (param.args.length > 0 && param.args[0] instanceof String) {
				if (isRestrictedExtra(param, (String) param.args[0]))
					proxyLocationListener(param, 1, LocationListener.class, true);
			} else {
				if (isRestricted(param))
					proxyLocationListener(param, 1, LocationListener.class, true);
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
		case Srv_addGpsMeasurementsListener:
		case Srv_addGpsNavigationMessageListener:
		case Srv_removeGeofence:
		case Srv_removeGpsStatusListener:
		case Srv_removeGpsMeasurementsListener:
		case Srv_removeGpsNavigationMessageListener:
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
			if (param.getResult() instanceof GpsStatus)
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
			if (param.args.length > 0 && param.getResult() instanceof Location) {
				String provider = (String) param.args[0];
				Location location = (Location) param.getResult();
				if (isRestrictedExtra(param, provider))
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

	private void proxyLocationListener(XParam param, int arg, Class<?> interfaze, boolean client) throws Throwable {
		if (param.args.length > arg)
			if (param.args[arg] instanceof PendingIntent)
				param.setResult(null);

			else if (param.args[arg] != null && param.thisObject != null) {
				if (client) {
					Object key = param.args[arg];
					synchronized (mMapProxy) {
						// Reuse existing proxy
						if (mMapProxy.containsKey(key)) {
							Util.log(this, Log.INFO, "Reuse existing proxy uid=" + Binder.getCallingUid());
							param.args[arg] = mMapProxy.get(key);
							return;
						}

						// Already proxied
						if (mMapProxy.containsValue(key)) {
							Util.log(this, Log.INFO, "Already proxied uid=" + Binder.getCallingUid());
							return;
						}
					}

					// Create proxy
					Util.log(this, Log.INFO, "Creating proxy uid=" + Binder.getCallingUid());
					Object proxy = new ProxyLocationListener(Binder.getCallingUid(), (LocationListener) param.args[arg]);

					// Use proxy
					synchronized (mMapProxy) {
						mMapProxy.put(key, proxy);
					}
					param.args[arg] = proxy;
				} else {
					// Create proxy
					ClassLoader cl = param.thisObject.getClass().getClassLoader();
					InvocationHandler ih = new OnLocationChangedHandler(Binder.getCallingUid(), param.args[arg]);
					Object proxy = Proxy.newProxyInstance(cl, new Class<?>[] { interfaze }, ih);

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
	}

	private void unproxyLocationListener(XParam param, int arg, boolean client) {
		if (param.args.length > arg)
			if (param.args[arg] instanceof PendingIntent)
				param.setResult(null);

			else if (param.args[arg] != null) {
				if (client) {
					Object key = param.args[arg];
					synchronized (mMapProxy) {
						if (mMapProxy.containsKey(key)) {
							Util.log(this, Log.INFO, "Removing proxy uid=" + Binder.getCallingUid());
							param.args[arg] = mMapProxy.get(key);
						}
					}
				} else {
					Object key = param.args[arg];
					if (key instanceof IInterface)
						key = ((IInterface) key).asBinder();

					synchronized (mMapProxy) {
						if (mMapProxy.containsKey(key))
							param.args[arg] = mMapProxy.get(key);
					}
				}
			}
	}

	private static class ProxyLocationListener implements LocationListener {
		private int mUid;
		private LocationListener mListener;

		public ProxyLocationListener(int uid, LocationListener listener) {
			mUid = uid;
			mListener = listener;
		}

		@Override
		public void onLocationChanged(Location location) {
			Util.log(null, Log.INFO, "Location changed uid=" + Binder.getCallingUid());
			Location fakeLocation = PrivacyManager.getDefacedLocation(mUid, location);
			mListener.onLocationChanged(fakeLocation);
		}

		@Override
		public void onProviderDisabled(String provider) {
			mListener.onProviderDisabled(provider);
		}

		@Override
		public void onProviderEnabled(String provider) {
			mListener.onProviderEnabled(provider);
		}

		@Override
		public void onStatusChanged(String provider, int status, Bundle extras) {
			mListener.onStatusChanged(provider, status, extras);
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

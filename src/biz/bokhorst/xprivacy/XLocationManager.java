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
import android.util.Log;
import android.location.GpsSatellite;
import android.location.LocationListener;
import android.location.GpsStatus;

public class XLocationManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.location.LocationManager";
	private static final Map<Object, Object> mMapProxy = new WeakHashMap<Object, Object>();

	private XLocationManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	private XLocationManager(Methods method, String restrictionName, String className, int sdk) {
		super(restrictionName, method.name(), null, sdk);
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

	// @formatter:on

	// @formatter:off
	private enum Methods {
		addGeofence, addGpsStatusListener, addNmeaListener, addProximityAlert,
		getAllProviders, getBestProvider,
		getGpsStatus,
		getLastKnownLocation,
		getProviders, isProviderEnabled,
		removeUpdates,
		requestLocationUpdates, requestSingleUpdate,
		sendExtraCommand
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
				else
					listHook.add(new XLocationManager(loc, PrivacyManager.cLocation, className));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.addNmeaListener || mMethod == Methods.addGpsStatusListener) {
			if (isRestricted(param))
				param.setResult(false);

		} else if (mMethod == Methods.addGeofence || mMethod == Methods.addProximityAlert) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.removeUpdates) {
			if (isRestricted(param, PrivacyManager.cLocation, "requestLocationUpdates"))
				removeLocationListener(param);

		} else if (mMethod == Methods.requestLocationUpdates) {
			if (param.args.length > 0 && param.args[0] instanceof String) {
				if (isRestrictedExtra(param, (String) param.args[0]))
					replaceLocationListener(param, 3);
			} else {
				if (isRestricted(param))
					replaceLocationListener(param, 3);
			}

		} else if (mMethod == Methods.requestSingleUpdate) {
			if (param.args.length > 0 && param.args[0] instanceof String) {
				if (isRestrictedExtra(param, (String) param.args[0]))
					replaceLocationListener(param, 1);
			} else {
				if (isRestricted(param))
					replaceLocationListener(param, 1);
			}
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod != Methods.addGeofence && mMethod != Methods.addNmeaListener
				&& mMethod != Methods.addGpsStatusListener && mMethod != Methods.addProximityAlert
				&& mMethod != Methods.removeUpdates && mMethod != Methods.requestLocationUpdates
				&& mMethod != Methods.requestSingleUpdate)
			if (mMethod == Methods.isProviderEnabled) {
				if (param.args.length > 0) {
					String provider = (String) param.args[0];
					if (isRestrictedExtra(param, provider))
						param.setResult(false);
				}

			} else if (mMethod == Methods.getGpsStatus) {
				if (param.getResult() != null && isRestricted(param)) {
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

			} else if (mMethod == Methods.getAllProviders) {
				if (isRestricted(param))
					param.setResult(new ArrayList<String>());

			} else if (mMethod == Methods.getBestProvider) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(null);

			} else if (mMethod == Methods.getLastKnownLocation) {
				if (param.args.length > 0) {
					String provider = (String) param.args[0];
					Location location = (Location) param.getResult();
					if (location != null && isRestrictedExtra(param, provider))
						param.setResult(PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location));
				}

			} else if (mMethod == Methods.getProviders) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<String>());

			} else if (mMethod == Methods.sendExtraCommand) {
				if (param.args.length > 0) {
					String provider = (String) param.args[0];
					if (isRestrictedExtra(param, provider))
						param.setResult(false);
				}

			} else
				Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	private void replaceLocationListener(XParam param, int arg) throws Throwable {
		if (param.args.length > arg)
			if (param.args[arg] instanceof PendingIntent)
				param.setResult(null);
			else if (param.thisObject != null) {
				// Create proxy
				ClassLoader cl = param.thisObject.getClass().getClassLoader();
				InvocationHandler ih = new OnLocationChangedHandler(Binder.getCallingUid(), param.args[arg]);
				Object proxy = Proxy.newProxyInstance(cl, new Class<?>[] { LocationListener.class }, ih);

				// Use proxy
				synchronized (mMapProxy) {
					mMapProxy.put(param.args[arg], proxy);
				}
				param.args[arg] = proxy;
			}
	}

	private void removeLocationListener(XParam param) {
		if (param.args.length > 0)
			if (param.args[0] instanceof PendingIntent)
				param.setResult(null);
			else
				synchronized (mMapProxy) {
					if (mMapProxy.containsKey(param.args[0]))
						param.args[0] = mMapProxy.get(param.args[0]);
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

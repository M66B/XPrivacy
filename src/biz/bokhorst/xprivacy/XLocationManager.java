package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Map;
import java.util.WeakHashMap;

import android.content.Context;
import android.location.Location;
import android.os.Binder;
import android.os.Bundle;
import android.util.Log;
import android.location.LocationListener;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XLocationManager extends XHook {
	private static final Map<LocationListener, XLocationListener> mListener = new WeakHashMap<LocationListener, XLocationListener>();

	public XLocationManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// @formatter:off

	// public boolean addNmeaListener(GpsStatus.NmeaListener listener)
	// public void addProximityAlert(double latitude, double longitude, float radius, long expiration, PendingIntent intent)
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

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("addGeofence") || methodName.equals("addNmeaListener")
				|| methodName.equals("addProximityAlert")) {
			if (isRestricted(param))
				param.setResult(null);
		} else if (methodName.equals("removeUpdates")) {
			if (isRestricted(param))
				removeLocationListener(param);
		} else if (methodName.equals("requestLocationUpdates")) {
			if (isRestricted(param))
				replaceLocationListener(param, 3);
		} else if (methodName.equals("requestSingleUpdate")) {
			if (isRestricted(param))
				replaceLocationListener(param, 1);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("isProviderEnabled")) {
			if (isRestricted(param))
				param.setResult(false);
		} else if (methodName.equals("getLastLocation") || methodName.equals("getLastKnownLocation")) {
			Location location = (Location) param.getResult();
			if (location != null)
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedLocation(location));
		} else if (methodName.equals("getProviders")) {
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList<String>());
		} else if (methodName.equals("sendExtraCommand")) {
			if (isRestricted(param))
				param.setResult(false);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			// Not all location managers do have a context
			Util.bug(this, ex);
		}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	private void replaceLocationListener(MethodHookParam param, int arg) throws Throwable {
		if (param.args.length > arg && param.args[arg] != null
				&& LocationListener.class.isAssignableFrom(param.args[arg].getClass())) {
			if (!(param.args[arg] instanceof XLocationListener)) {
				LocationListener listener = (LocationListener) param.args[arg];
				if (listener != null) {
					XLocationListener xListener = new XLocationListener(listener);
					synchronized (mListener) {
						mListener.put(listener, xListener);
						Util.log(this, Log.INFO, "Added count=" + mListener.size());
					}
					param.args[arg] = xListener;
				}
			}
		} else
			param.setResult(null);
	}

	private void removeLocationListener(MethodHookParam param) {
		if (param.args.length > 0 && param.args[0] != null
				&& LocationListener.class.isAssignableFrom(param.args[0].getClass())) {
			LocationListener listener = (LocationListener) param.args[0];
			synchronized (mListener) {
				XLocationListener xlistener = mListener.get(listener);
				if (xlistener == null)
					Util.log(this, Log.WARN, "Not found count=" + mListener.size());
				else {
					param.args[0] = xlistener;
					mListener.remove(listener);
				}
			}
		} else
			param.setResult(null);
	}

	private class XLocationListener implements LocationListener {

		private LocationListener mLocationListener;

		public XLocationListener(LocationListener locationListener) {
			mLocationListener = locationListener;
		}

		@Override
		public void onLocationChanged(Location location) {
			mLocationListener.onLocationChanged(location == null ? location : PrivacyManager
					.getDefacedLocation(location));
		}

		@Override
		public void onProviderDisabled(String provider) {
			mLocationListener.onProviderDisabled(provider);
		}

		@Override
		public void onProviderEnabled(String provider) {
			mLocationListener.onProviderEnabled(provider);
		}

		@Override
		public void onStatusChanged(String provider, int status, Bundle extras) {
			mLocationListener.onStatusChanged(provider, status, extras);
		}
	}
}

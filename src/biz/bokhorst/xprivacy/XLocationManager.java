package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Date;
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
	// frameworks/base/location/java/android/location/LocationManager.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (!methodName.equals("getLastKnownLocation"))
			if (isRestricted(param))
				if (methodName.equals("addNmeaListener"))
					param.setResult(false);
				else if (methodName.equals("addProximityAlert"))
					param.setResult(null);
				else if (methodName.equals("removeUpdates"))
					removeLocationListener(param);
				else if (methodName.equals("requestLocationUpdates"))
					replaceLocationListener(param, 3);
				else if (methodName.equals("requestSingleUpdate"))
					replaceLocationListener(param, 1);
				else
					XUtil.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("getLastKnownLocation")) {
			Location referenceLocation = (Location) param.getResult();
			if (referenceLocation != null)
				if (isRestricted(param)) {
					String provider = (String) param.args[0];
					Context context = getContext(param);
					Location baseLocation = getBaseLocation(context);
					Location randomLocation = getRandomLocation(provider, baseLocation, referenceLocation);
					param.setResult(randomLocation);
				}
		}
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = getContext(param);
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	private Context getContext(MethodHookParam param) throws Throwable {
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			// Not all location managers do have a context
			XUtil.bug(this, ex);
		}
		return context;
	}

	private void removeLocationListener(MethodHookParam param) {
		if (param.args[0] != null && LocationListener.class.isAssignableFrom(param.args[0].getClass())) {
			LocationListener listener = (LocationListener) param.args[0];
			synchronized (mListener) {
				XLocationListener xlistener = mListener.get(listener);
				if (xlistener == null)
					XUtil.log(this, Log.WARN, "Not found count=" + mListener.size());
				else {
					param.args[0] = xlistener;
					mListener.remove(listener);
				}
			}
		} else
			param.setResult(null);
	}

	private void replaceLocationListener(MethodHookParam param, int arg) throws Throwable {
		if (param.args[arg] != null && LocationListener.class.isAssignableFrom(param.args[arg].getClass())) {
			LocationListener listener = (LocationListener) param.args[arg];
			if (listener != null) {
				Context context = getContext(param);
				Location baseLocation = getBaseLocation(context);
				XLocationListener xListener = new XLocationListener(listener, baseLocation);
				synchronized (mListener) {
					mListener.put(listener, xListener);
					XUtil.log(this, Log.INFO, "Added count=" + mListener.size());
				}
				param.args[arg] = xListener;
			}
		} else
			param.setResult(null);
	}

	private Location getBaseLocation(Context context) {
		String sLat = XRestriction.getSetting(this, context, XRestriction.cSettingLatitude, "", true);
		String sLon = XRestriction.getSetting(this, context, XRestriction.cSettingLongitude, "", true);
		if (sLat.equals("") || sLon.equals(""))
			return null;
		Location location = new Location("");
		location.setLatitude(Float.parseFloat(sLat));
		location.setLongitude(Float.parseFloat(sLon));
		return location;
	}

	private Location getRandomLocation(String provider, Location baseLocation, Location referenceLocation) {
		Location location = new Location(provider);
		if (baseLocation == null || referenceLocation == null) {
			location.setLatitude(getRandomLat());
			location.setLongitude(getRandomLon());
		} else {
			// 1 degree ~ 111111 m
			// 1 m ~ 0,000009 degrees = 9e-6
			float accuracy = referenceLocation.getAccuracy();
			location.setLatitude(baseLocation.getLatitude() + (Math.random() * 2.0 - 1.0) * accuracy * 9e-6);
			location.setLongitude(baseLocation.getLongitude() + (Math.random() * 2.0 - 1.0) * accuracy * 9e-6);
		}
		location.setTime(new Date().getTime());
		return location;
	}

	private double getRandomLat() {
		double lat = Math.random() * 180;
		BigDecimal latitude = new BigDecimal(lat > 90 ? lat - 90 : -lat);
		return latitude.setScale(6, BigDecimal.ROUND_HALF_UP).doubleValue();
	}

	private double getRandomLon() {
		double lon = Math.random() * 360;
		BigDecimal longitude = new BigDecimal(lon > 180 ? lon - 180 : -lon);
		return longitude.setScale(6, BigDecimal.ROUND_HALF_UP).doubleValue();
	}

	private class XLocationListener implements LocationListener {

		private LocationListener mLocationListener;
		private Location mBaseLocation;

		public XLocationListener(LocationListener locationListener, Location baseLocation) {
			mLocationListener = locationListener;
			mBaseLocation = baseLocation;
		}

		@Override
		public void onLocationChanged(Location location) {
			Location randomLocation = getRandomLocation(location.getProvider(), mBaseLocation, location);
			mLocationListener.onLocationChanged(randomLocation);
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

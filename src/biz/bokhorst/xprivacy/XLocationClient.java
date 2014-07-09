package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.location.Location;
import android.os.Binder;
import android.util.Log;

import com.google.android.gms.location.LocationListener;

public class XLocationClient extends XHook {
	private Methods mMethod;
	private static final Map<LocationListener, XLocationListener> mListener = new WeakHashMap<LocationListener, XLocationListener>();

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
		listHook.add(new XLocationClient(Methods.addGeofences, PrivacyManager.cLocation).optional());
		listHook.add(new XLocationClient(Methods.getLastLocation, PrivacyManager.cLocation).optional());
		listHook.add(new XLocationClient(Methods.removeGeofences, null, 1).optional());
		listHook.add(new XLocationClient(Methods.removeLocationUpdates, null, 1).optional());
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

		} else if (mMethod == Methods.removeLocationUpdates) {
			removeLocationListener(param);

		} else if (mMethod == Methods.requestLocationUpdates) {
			if (isRestricted(param))
				replaceLocationListener(param);

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

	private void replaceLocationListener(XParam param) throws Throwable {
		if (param.args.length >= 2 && param.args[1] != null
				&& LocationListener.class.isAssignableFrom(param.args[1].getClass())) {
			if (!(param.args[1] instanceof XLocationListener)) {
				LocationListener listener = (LocationListener) param.args[1];
				if (listener != null) {
					XLocationListener xListener;
					synchronized (mListener) {
						xListener = mListener.get(listener);
						if (xListener == null) {
							xListener = new XLocationListener(listener);
							mListener.put(listener, xListener);
							Util.log(this, Log.WARN,
									"Added count=" + mListener.size() + " uid=" + Binder.getCallingUid());
						}
					}
					param.args[1] = xListener;
				}
			}
		} else
			param.setResult(null);
	}

	private void removeLocationListener(XParam param) {
		if (param.args.length >= 1 && param.args[0] != null
				&& LocationListener.class.isAssignableFrom(param.args[0].getClass())) {
			LocationListener listener = (LocationListener) param.args[0];
			synchronized (mListener) {
				XLocationListener xlistener = mListener.get(listener);
				if (xlistener != null) {
					param.args[0] = xlistener;
					Util.log(this, Log.WARN, "Removed count=" + mListener.size() + " uid=" + Binder.getCallingUid());
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
			if (location != null)
				location = PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location);
			mLocationListener.onLocationChanged(location);
		}
	}
}

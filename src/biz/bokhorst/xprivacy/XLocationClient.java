package biz.bokhorst.xprivacy;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XLocationClient extends XHook {

	public XLocationClient(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// @formatter:off

	// void addGeofences(List<Geofence> geofences, PendingIntent pendingIntent, LocationClient.OnAddGeofencesResultListener listener)
	// Location getLastLocation()
	// void removeLocationUpdates(LocationListener listener)
	// void removeLocationUpdates(PendingIntent callbackIntent)
	// void requestLocationUpdates(LocationRequest request, PendingIntent callbackIntent)
	// void requestLocationUpdates(LocationRequest request, LocationListener listener)
	// void requestLocationUpdates(LocationRequest request, LocationListener listener, Looper looper)
	// https://developer.android.com/reference/com/google/android/gms/location/LocationClient.html

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param))
			param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

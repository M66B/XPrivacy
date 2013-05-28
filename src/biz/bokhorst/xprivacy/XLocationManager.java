package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.math.BigDecimal;

import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.location.GpsStatus;
import android.location.Location;
import android.location.LocationListener;
import android.os.Binder;
import android.os.Bundle;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XLocationManager extends XHook {
	public final static String cPermissionName = "location";

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (!param.method.getName().equals("getLastKnownLocation")) {
			Context context = getContext(param);
			if (!isAllowed(context, Binder.getCallingUid(), cPermissionName)) {
				info("deny " + param.method.getName());
				for (int arg = 0; arg < param.args.length; arg++)
					if (param.args[arg] != null) {
						if (param.args[arg].getClass().equals(GpsStatus.Listener.class)) {
							// GpsStatus.Listener
							info("deny " + param.args[arg].getClass().getName());
							param.args[arg] = new GpsStatus.Listener() {
								@Override
								public void onGpsStatusChanged(int arg0) {
								}
							};
						} else if (param.args[arg].getClass().equals(GpsStatus.NmeaListener.class)) {
							// GpsStatus.NmeaListener
							info("deny " + param.args[arg].getClass().getName());
							param.args[arg] = new GpsStatus.NmeaListener() {
								@Override
								public void onNmeaReceived(long arg0, String arg1) {
								}
							};
						} else if (param.args[arg].getClass().equals(PendingIntent.class)) {
							// PendingIntent
							info("deny " + param.args[arg].getClass().getName());
							param.args[arg] = PendingIntent.getBroadcast(context, 0, new Intent(), 0);
						} else if (param.args[arg].getClass().equals(LocationListener.class)) {
							// LocationListener
							info("deny " + param.args[arg].getClass().getName());
							param.args[arg] = new LocationListener() {
								@Override
								public void onLocationChanged(Location arg0) {
								}

								@Override
								public void onProviderDisabled(String arg0) {
								}

								@Override
								public void onProviderEnabled(String arg0) {
								}

								@Override
								public void onStatusChanged(String arg0, int arg1, Bundle arg2) {
								}
							};
						}
					}
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("getLastKnownLocation")) {
			if (!isAllowed(getContext(param), Binder.getCallingUid(), cPermissionName)) {
				info("deny " + param.method.getName());
				Location location = (Location) param.getResult();
				if (location != null) {
					String provider = (String) param.args[0];
					location = getRandomLocation(provider);
					param.setResult(location);
				}
			}
		}
	}

	protected Context getContext(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		return (Context) fieldContext.get(param.thisObject);
	}

	protected Location getRandomLocation(String provider) {
		Location location = new Location(provider);
		location.setLatitude(getRandomLat());
		location.setLongitude(getRandomLon());
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
}

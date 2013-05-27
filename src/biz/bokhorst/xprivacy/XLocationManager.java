package biz.bokhorst.xprivacy;

import java.math.BigDecimal;

import android.location.Location;

public abstract class XLocationManager extends XHook {
	public final static String cPermissionName = "Location";

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

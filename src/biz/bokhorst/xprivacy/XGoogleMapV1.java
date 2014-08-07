package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.location.Location;
import android.os.Binder;
import android.util.Log;

public class XGoogleMapV1 extends XHook {
	private Methods mMethod;

	private XGoogleMapV1(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("MapV1.%s", method.name()));
		mMethod = method;
	}

	public String getClassName() {
		if (mMethod == Methods.getLatitudeE6 || mMethod == Methods.getLongitudeE6)
			return "com.google.android.maps.GeoPoint";
		else
			return "com.google.android.maps.MyLocationOverlay";
	}

	// @formatter:off

	// int getLatitudeE6()
	// int getLongitudeE6()
	// boolean enableMyLocation()
	// GeoPoint getMyLocation() 
	// https://developers.google.com/maps/documentation/android/v1/reference/index

	// @formatter:on

	private enum Methods {
		getLatitudeE6, getLongitudeE6, enableMyLocation
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods method : Methods.values())
			listHook.add(new XGoogleMapV1(method, PrivacyManager.cLocation));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.getLatitudeE6) {
			// Do nothing

		} else if (mMethod == Methods.getLongitudeE6) {
			// Do nothing

		} else if (mMethod == Methods.enableMyLocation) {
			if (isRestricted(param))
				param.setResult(false);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getLatitudeE6) {
			if (isRestricted(param)) {
				Location fakeLocation = PrivacyManager.getDefacedLocation(Binder.getCallingUid(), null);
				param.setResult((int) Math.round(fakeLocation.getLatitude() * 1e6));
			}

		} else if (mMethod == Methods.getLongitudeE6) {
			if (isRestricted(param)) {
				Location fakeLocation = PrivacyManager.getDefacedLocation(Binder.getCallingUid(), null);
				param.setResult((int) Math.round(fakeLocation.getLongitude() * 1e6));
			}

		} else if (mMethod == Methods.enableMyLocation) {
			// Do nothing

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

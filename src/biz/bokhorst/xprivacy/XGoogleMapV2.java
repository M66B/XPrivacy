package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.location.Location;
import android.os.Binder;

public class XGoogleMapV2 extends XHook {
	private Methods mMethod;

	private XGoogleMapV2(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("MapV2.%s", method.name()));
		mMethod = method;
	}

	public String getClassName() {
		if (mMethod == Methods.getPosition)
			return "com.google.android.gms.maps.model.Marker";
		else
			return "com.google.android.gms.maps.GoogleMap";
	}

	// @formatter:off

	// final Location getMyLocation()
	// final void setLocationSource(LocationSource source)
	// final void setOnMapClickListener(GoogleMap.OnMapClickListener listener)
	// final void setOnMapLongClickListener(GoogleMap.OnMapLongClickListener listener)
	// final void setOnMyLocationChangeListener(GoogleMap.OnMyLocationChangeListener listener)
	// http://developer.android.com/reference/com/google/android/gms/maps/GoogleMap.html

	// public LatLng getPosition ()
	// http://developer.android.com/reference/com/google/android/gms/maps/model/Marker.html
	// http://developer.android.com/reference/com/google/android/gms/maps/model/LatLng.html

	// @formatter:on

	private enum Methods {
		getMyLocation, getPosition, setLocationSource, setOnMapClickListener, setOnMapLongClickListener, setOnMyLocationChangeListener
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods method : Methods.values())
			listHook.add(new XGoogleMapV2(method, PrivacyManager.cLocation));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case getMyLocation:
			// Do nothing
			break;

		case getPosition:
			// Do nothing
			break;

		case setLocationSource:
		case setOnMapClickListener:
		case setOnMapLongClickListener:
		case setOnMyLocationChangeListener:
			if (isRestricted(param))
				param.setResult(null);
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getMyLocation:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					Location originalLocation = (Location) param.getResult();
					Location fakeLocation = PrivacyManager.getDefacedLocation(Binder.getCallingUid(), originalLocation);
					param.setResult(fakeLocation);
				}
			break;

		case getPosition:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					Location fakeLocation = PrivacyManager.getDefacedLocation(Binder.getCallingUid(), null);
					Field fLat = param.getResult().getClass().getField("latitude");
					Field fLon = param.getResult().getClass().getField("longitude");
					fLat.setAccessible(true);
					fLon.setAccessible(true);
					fLat.set(param.getResult(), fakeLocation.getLatitude());
					fLon.set(param.getResult(), fakeLocation.getLongitude());
				}
			break;

		case setLocationSource:
		case setOnMapClickListener:
		case setOnMapLongClickListener:
		case setOnMyLocationChangeListener:
			// Do nothing
			break;
		}
	}
}

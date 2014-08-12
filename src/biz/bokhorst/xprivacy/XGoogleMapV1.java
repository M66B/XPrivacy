package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

public class XGoogleMapV1 extends XHook {
	private Methods mMethod;

	private XGoogleMapV1(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("MapV1.%s", method.name()));
		mMethod = method;
	}

	public String getClassName() {
		return "com.google.android.maps.MyLocationOverlay";
	}

	// boolean enableMyLocation()
	// void disableMyLocation()
	// https://developers.google.com/maps/documentation/android/v1/reference/index

	private enum Methods {
		enableMyLocation, disableMyLocation
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XGoogleMapV1(Methods.enableMyLocation, PrivacyManager.cLocation));
		listHook.add(new XGoogleMapV1(Methods.disableMyLocation, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case enableMyLocation:
			if (isRestricted(param))
				param.setResult(false);
			break;

		case disableMyLocation:
			if (isRestricted(param, PrivacyManager.cLocation, "MapV1.enableMyLocation"))
				param.setResult(null);
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

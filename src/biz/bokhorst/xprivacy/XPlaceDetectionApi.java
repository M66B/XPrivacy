package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XPlaceDetectionApi extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XPlaceDetectionApi(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), "GMS5." + method.name());
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// abstract PendingResult<PlaceLikelihoodBuffer> getCurrentPlace(GoogleApiClient client, PlaceFilter filter)
	// https://developer.android.com/reference/com/google/android/gms/location/places/PlaceDetectionApi.html
	
	// @formatter:on

	private enum Methods {
		getCurrentPlace
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		Util.log(null, Log.WARN, "Hooking PlaceDetectionApi class=" + className + " uid=" + Binder.getCallingUid());

		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XPlaceDetectionApi(Methods.getCurrentPlace, PrivacyManager.cLocation, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getCurrentPlace:
			if (isRestricted(param))
				param.setResult(XGoogleApiClient.getPendingResult(param.thisObject.getClass().getClassLoader()));
			break;
		}
	}
}

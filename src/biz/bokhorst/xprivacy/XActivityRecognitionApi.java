package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XActivityRecognitionApi extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XActivityRecognitionApi(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), "GMS5." + method.name());
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// Location getLastLocation(GoogleApiClient client)
	// abstract PendingResult<Status> removeActivityUpdates(GoogleApiClient client, PendingIntent callbackIntent)
	// abstract PendingResult<Status> requestActivityUpdates(GoogleApiClient client, long detectionIntervalMillis, PendingIntent callbackIntent)
	// https://developer.android.com/reference/com/google/android/gms/location/ActivityRecognitionApi.html
	
	// @formatter:on

	private enum Methods {
		removeActivityUpdates, requestActivityUpdates
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		Util.log(null, Log.INFO, "Hooking ActivityRecognitionApi class=" + className + " uid=" + Binder.getCallingUid());

		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XActivityRecognitionApi(Methods.removeActivityUpdates, null, className));
		listHook.add(new XActivityRecognitionApi(Methods.requestActivityUpdates, PrivacyManager.cLocation, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case removeActivityUpdates:
			if (isRestricted(param, PrivacyManager.cLocation, "GMS5.requestActivityUpdates"))
				param.setResult(XGoogleApiClient.getPendingResult(param.thisObject.getClass().getClassLoader()));
			break;

		case requestActivityUpdates:
			if (isRestricted(param))
				param.setResult(XGoogleApiClient.getPendingResult(param.thisObject.getClass().getClassLoader()));
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

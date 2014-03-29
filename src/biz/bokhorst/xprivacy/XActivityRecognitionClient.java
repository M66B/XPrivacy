package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XActivityRecognitionClient extends XHook {
	private Methods mMethod;

	private XActivityRecognitionClient(Methods method, String restrictionName) {
		super(restrictionName, "GMS." + method.name(), null);
		mMethod = method;
	}

	private XActivityRecognitionClient(Methods method, String restrictionName, int sdk) {
		super(restrictionName, "GMS." + method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "com.google.android.gms.location.ActivityRecognitionClient";
	}

	// @formatter:off

	// public void removeActivityUpdates(PendingIntent callbackIntent)
	// public void requestActivityUpdates(long detectionIntervalMillis, PendingIntent callbackIntent)
	// http://developer.android.com/reference/com/google/android/gms/location/ActivityRecognitionClient.html

	// @formatter:on

	private enum Methods {
		removeActivityUpdates, requestActivityUpdates
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XActivityRecognitionClient(Methods.removeActivityUpdates, null, 1));
		listHook.add(new XActivityRecognitionClient(Methods.requestActivityUpdates, PrivacyManager.cLocation));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.removeActivityUpdates) {
			if (isRestricted(param, "GMS.requestActivityUpdates"))
				param.setResult(null);

		} else if (mMethod == Methods.requestActivityUpdates) {
			if (isRestricted(param))
				param.setResult(null);

		} else

			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

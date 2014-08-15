package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

public class XActivityRecognitionClient extends XHook {
	private Methods mMethod;

	private XActivityRecognitionClient(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("GMS.%s", method.name()));
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
		listHook.add(new XActivityRecognitionClient(Methods.removeActivityUpdates, null));
		listHook.add(new XActivityRecognitionClient(Methods.requestActivityUpdates, PrivacyManager.cLocation));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case removeActivityUpdates:
			if (isRestricted(param, PrivacyManager.cLocation, "GMS.requestActivityUpdates"))
				param.setResult(null);
			break;

		case requestActivityUpdates:
			if (isRestricted(param))
				param.setResult(null);
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XAnalytics extends XHook {
	private Methods mMethod;

	private XAnalytics(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	private XAnalytics(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "com.google.analytics.tracking.android.EasyTracker";
	}

	// https://developers.google.com/analytics/devguides/collection/android/v3/

	private enum Methods {
		activityStart, activityStop
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XAnalytics(Methods.activityStart, PrivacyManager.cAnalytics).optional());
		listHook.add(new XAnalytics(Methods.activityStop, null, 1).optional());
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.activityStart) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.activityStop) {
			if (isRestricted(param, PrivacyManager.cAnalytics, "activityStart"))
				param.setResult(null);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;
import java.util.SimpleTimeZone;

import android.util.Log;

public class XTimeZone extends XHook {
	private Methods mMethod;

	private XTimeZone(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "TZ." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "java.util.TimeZone";
	}

	// public static synchronized TimeZone getDefault()
	// http://developer.android.com/reference/android/app/ActivityManager.html

	private enum Methods {
		getDefault
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XTimeZone(Methods.getDefault, PrivacyManager.cSystem));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getDefault) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new SimpleTimeZone(0, "UTC"));

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

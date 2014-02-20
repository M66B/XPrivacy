package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.appwidget.AppWidgetProviderInfo;
import android.util.Log;

public class XAppWidgetManager extends XHook {
	private Methods mMethod;

	private XAppWidgetManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.appwidget.AppWidgetManager";
	}

	// public List<AppWidgetProviderInfo> getInstalledProviders()
	// frameworks/base/core/java/android/appwidget/AppWidgetManager.java
	// http://developer.android.com/reference/android/appwidget/AppWidgetManager.html

	private enum Methods {
		getInstalledProviders
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XAppWidgetManager(Methods.getInstalledProviders, PrivacyManager.cSystem));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getInstalledProviders) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<AppWidgetProviderInfo>());

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

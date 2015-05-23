package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.appwidget.AppWidgetProviderInfo;
import android.os.Build;

public class XAppWidgetManager extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XAppWidgetManager(Methods method, String restrictionName) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
		if (method.name().startsWith("Srv_"))
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
				mClassName = "com.android.server.appwidget.AppWidgetServiceImpl";
			else
				mClassName = "com.android.server.AppWidgetService";
		else
			mClassName = "android.appwidget.AppWidgetManager";
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public List<AppWidgetProviderInfo> getInstalledProviders()
	// public List<AppWidgetProviderInfo> getInstalledProvidersForProfile(UserHandle profile)
	// frameworks/base/core/java/android/appwidget/AppWidgetManager.java
	// http://developer.android.com/reference/android/appwidget/AppWidgetManager.html

	// public List<AppWidgetProviderInfo> getInstalledProviders(int categoryFilter, int userId)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.4_r1/com/android/server/AppWidgetService.java

	// @formatter:on

	private enum Methods {
		getInstalledProviders, getInstalledProvidersForProfile, Srv_getInstalledProviders, Srv_getInstalledProvidersForProfile
	};

	public static List<XHook> getInstances(boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (server) {
			listHook.add(new XAppWidgetManager(Methods.Srv_getInstalledProviders, PrivacyManager.cSystem));
			listHook.add(new XAppWidgetManager(Methods.Srv_getInstalledProvidersForProfile, PrivacyManager.cSystem));
		} else {
			listHook.add(new XAppWidgetManager(Methods.getInstalledProviders, PrivacyManager.cSystem));
			listHook.add(new XAppWidgetManager(Methods.getInstalledProvidersForProfile, PrivacyManager.cSystem));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getInstalledProviders:
		case getInstalledProvidersForProfile:
		case Srv_getInstalledProviders:
		case Srv_getInstalledProvidersForProfile:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList<AppWidgetProviderInfo>());
			break;
		}
	}
}

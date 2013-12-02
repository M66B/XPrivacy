package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;
import android.webkit.WebView;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XWebView extends XHook {
	private Methods mMethod;
	private static final List<String> mWebSettings = new ArrayList<String>();

	private XWebView(Methods method, String restrictionName) {
		super(restrictionName, (method == Methods.constructor ? null : method.name()),
				(method == Methods.constructor ? "WebView.constructor" : null));
		mMethod = method;
	}

	public String getClassName() {
		return "android.webkit.WebView";
	}

	// public WebSettings getSettings()
	// frameworks/base/core/java/android/webkit/WebView.java
	// http://developer.android.com/reference/android/webkit/WebView.html

	private enum Methods {
		constructor, getSettings
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebView(Methods.constructor, PrivacyManager.cView));
		listHook.add(new XWebView(Methods.getSettings, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.constructor) {
			if (isRestricted(param)) {
				String ua = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
				WebView webView = (WebView) param.thisObject;
				webView.getSettings().setUserAgentString(ua);
			}
		} else if (mMethod == Methods.getSettings) {
			if (param.getResult() != null) {
				Class<?> clazz = param.getResult().getClass();
				if (!mWebSettings.contains(clazz.getName())) {
					mWebSettings.add(clazz.getName());
					XPrivacy.hookAll(XWebSettings.getInstances(param.getResult()));
				}
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

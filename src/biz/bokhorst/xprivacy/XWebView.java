package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;
import android.webkit.WebView;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;
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

	// public synchronized void setUserAgent(int ua)
	// public synchronized void setUserAgentString (String ua)
	// frameworks/base/core/java/android/webkit/WebSettings.java
	// http://developer.android.com/reference/android/webkit/WebSettings.html

	private enum Methods {
		constructor, getSettings
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebView(Methods.constructor, PrivacyManager.cView));
		listHook.add(new XWebView(Methods.getSettings, PrivacyManager.cView));
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
			if (param.getResultOrThrowable() != null) {
				// Check web settings type
				Class<?> clazzWebSettings = param.getResult().getClass();
				if (!mWebSettings.contains(clazzWebSettings.getName())) {
					mWebSettings.add(clazzWebSettings.getName());

					// Hook setUserAgent
					try {
						Util.log(this, Log.INFO, "Hooking " + clazzWebSettings.getName() + ".setUserAgent");
						Method setUserAgent = clazzWebSettings.getDeclaredMethod("setUserAgent", int.class);
						XposedBridge.hookMethod(setUserAgent, new XC_MethodHook() {
							@Override
							protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
								if (isRestricted(param, "setUserAgent"))
									param.setResult(null);
							}
						});
					} catch (NoSuchFieldError ex) {
						Util.bug(this, ex);
					}

					// Hook setUserAgentString
					try {
						Util.log(this, Log.INFO, "Hooking " + clazzWebSettings.getName() + ".setUserAgentString");
						Method setUserAgentString = clazzWebSettings.getDeclaredMethod("setUserAgentString",
								String.class);
						XposedBridge.hookMethod(setUserAgentString, new XC_MethodHook() {
							@Override
							protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
								if (isRestricted(param, "setUserAgentString"))
									param.args[0] = PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
							}
						});
					} catch (NoSuchFieldError ex) {
						Util.bug(this, ex);
					}
				}
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XWebView extends XHook {
	private Methods mMethod;
	private static final List<String> mWebSettings = new ArrayList<String>();

	private XWebView(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.webkit.WebView";
	}

	// public WebSettings getSettings()
	// frameworks/base/core/java/android/webkit/WebView.java
	// http://developer.android.com/reference/android/webkit/WebView.html

	// public synchronize String getUserAgentString()
	// public synchronized void setUserAgentString (String ua)
	// frameworks/base/core/java/android/webkit/WebSettings.java
	// http://developer.android.com/reference/android/webkit/WebSettings.html

	private enum Methods {
		getSettings
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebView(Methods.getSettings, PrivacyManager.cView));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getSettings) {
			if (param.getResult() != null) {
				// Check web settings type
				Class<?> clazzWebSettings = param.getResultOrThrowable().getClass();
				if (!mWebSettings.contains(clazzWebSettings.getName())) {
					mWebSettings.add(clazzWebSettings.getName());
					Util.log(this, Log.INFO, "Hooking " + clazzWebSettings.getName());

					// getUserAgentString
					try {
						Method getUserAgentString = clazzWebSettings.getDeclaredMethod("getUserAgentString");
						Util.log(this, Log.INFO, "Hooking " + getUserAgentString.getName());
						XposedBridge.hookMethod(getUserAgentString, new XC_MethodHook() {
							@Override
							protected void afterHookedMethod(MethodHookParam param) throws Throwable {
								if (isRestricted(param))
									param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA"));
							}
						});
					} catch (NoSuchFieldError ex) {
						Util.bug(this, ex);
					}

					// setUserAgentString
					try {
						Method setUserAgentString = clazzWebSettings.getDeclaredMethod("setUserAgentString",
								String.class);
						Util.log(this, Log.INFO, "Hooking " + setUserAgentString.getName());
						XposedBridge.hookMethod(setUserAgentString, new XC_MethodHook() {
							@Override
							protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
								if (isRestricted(param))
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

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		if (param.args.length > 0)
			context = (Context) param.args[0];
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

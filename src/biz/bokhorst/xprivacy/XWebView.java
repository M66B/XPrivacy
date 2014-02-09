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
		super(restrictionName, (method == Methods.WebView ? null : method.name()), (method == Methods.WebView ? method
				.name() : null));
		mMethod = method;
	}

	private XWebView(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "android.webkit.WebView";
	}

	// @formatter:off

	// public WebView(Context context)
	// public WebView(Context context, AttributeSet attrs)
	// public WebView(Context context, AttributeSet attrs, int defStyle)
	// public WebView(Context context, AttributeSet attrs, int defStyle, boolean privateBrowsing)
	// protected WebView(Context context, AttributeSet attrs, int defStyle, Map<String, Object> javaScriptInterfaces, boolean privateBrowsing)
	// public WebSettings getSettings()
	// public void loadUrl(String url)
	// public void loadUrl(String url, Map<String, String> additionalHttpHeaders)
	// frameworks/base/core/java/android/webkit/WebView.java
	// http://developer.android.com/reference/android/webkit/WebView.html

	// @formatter:on

	private enum Methods {
		WebView, loadUrl, getSettings
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebView(Methods.WebView, PrivacyManager.cView));
		listHook.add(new XWebView(Methods.loadUrl, PrivacyManager.cView));
		listHook.add(new XWebView(Methods.getSettings, null, 1));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.WebView || mMethod == Methods.getSettings) {
			// Do nothing

		} else if (mMethod == Methods.loadUrl) {
			if (param.args.length > 0) {
				String extra = (param.args[0] instanceof String ? (String) param.args[0] : null);
				if (isRestrictedExtra(param, extra)) {
					String ua = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
					WebView webView = (WebView) param.thisObject;
					webView.getSettings().setUserAgentString(ua);
				}
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.WebView) {
			if (param.args.length > 0) {
				int uid = Binder.getCallingUid();
				if (getRestricted(uid)) {
					String ua = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
					WebView webView = (WebView) param.thisObject;
					webView.getSettings().setUserAgentString(ua);
				}
			}

		} else if (mMethod == Methods.loadUrl) {
			// Do nothing

		} else if (mMethod == Methods.getSettings) {
			if (param.getResult() != null) {
				Class<?> clazz = param.getResult().getClass();
				if (!mWebSettings.contains(clazz.getName())) {
					mWebSettings.add(clazz.getName());
					XPrivacy.hookAll(XWebSettings.getInstances(param.getResult()), getSecret());
				}
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

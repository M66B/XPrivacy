package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.webkit.WebView;

public class XWebView extends XHook {
	private Methods mMethod;

	private XWebView(Methods method, String restrictionName) {
		super(restrictionName, (method == Methods.WebView ? null : method.name()), (method == Methods.WebView ? method
				.name() : null));
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
	// public WebView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes)
	// protected WebView(Context context, AttributeSet attrs, int defStyle, Map<String, Object> javaScriptInterfaces, boolean privateBrowsing)
	// public WebSettings getSettings()
	// public void loadUrl(String url)
	// public void loadUrl(String url, Map<String, String> additionalHttpHeaders)
	// public postUrl(String url, byte[] postData)
	// http://developer.android.com/reference/android/webkit/WebView.html
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.2_r1/android/webkit/WebView.java/

	// @formatter:on

	private enum Methods {
		WebView, loadUrl, postUrl, getSettings
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebView(Methods.WebView, null));
		listHook.add(new XWebView(Methods.loadUrl, PrivacyManager.cView));
		listHook.add(new XWebView(Methods.postUrl, PrivacyManager.cView));
		listHook.add(new XWebView(Methods.getSettings, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case WebView:
		case getSettings:
			// Do nothing
			break;

		case loadUrl:
		case postUrl:
			if (param.args.length > 0 && param.thisObject instanceof WebView) {
				String extra = (param.args[0] instanceof String ? (String) param.args[0] : null);
				if (isRestrictedExtra(param, extra))
					param.setResult(null);
			}
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case WebView:
			if (param.args.length > 0 && param.thisObject instanceof WebView) {
				if (isRestricted(param, PrivacyManager.cView, "initUserAgentString")) {
					String ua = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
					WebView webView = (WebView) param.thisObject;
					if (webView.getSettings() != null)
						webView.getSettings().setUserAgentString(ua);
				}
			}
			break;

		case loadUrl:
		case postUrl:
			// Do nothing
			break;

		case getSettings:
			if (param.getResult() != null) {
				Class<?> clazz = param.getResult().getClass();
				if (PrivacyManager.getTransient(clazz.getName(), null) == null) {
					PrivacyManager.setTransient(clazz.getName(), Boolean.toString(true));
					XPrivacy.hookAll(XWebSettings.getInstances(param.getResult()), clazz.getClassLoader(), getSecret(),
							true);
				}
			}
			break;

		}
	}
}

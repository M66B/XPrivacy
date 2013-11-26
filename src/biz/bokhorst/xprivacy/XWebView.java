package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.webkit.WebView;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XWebView extends XHook {

	private XWebView(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
	}

	public String getClassName() {
		return "android.webkit.WebView";
	}

	// @formatter:off
	
	// public void loadData (String data, String mimeType, String encoding)
	// public void loadDataWithBaseURL(String baseUrl, String data, String mimeType, String encoding, String historyUrl)
	// public void loadUrl(String url)
	// public void loadUrl(String url, Map<String, String> additionalHttpHeaders)
	// public WebSettings getSettings()
	// frameworks/base/core/java/android/webkit/WebView.java
	// http://developer.android.com/reference/android/webkit/WebView.html

	// @formatter:on

	private enum Methods {
		loadData, loadDataWithBaseURL, loadUrl
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebView(Methods.loadData, PrivacyManager.cView));
		listHook.add(new XWebView(Methods.loadDataWithBaseURL, PrivacyManager.cView));
		listHook.add(new XWebView(Methods.loadUrl, PrivacyManager.cView));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			String ua = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
			WebView webView = (WebView) param.thisObject;
			webView.getSettings().setUserAgentString(ua);
		}
	}
}

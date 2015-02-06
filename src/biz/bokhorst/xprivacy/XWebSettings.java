package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XWebSettings extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XWebSettings(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// public static String getDefaultUserAgent(Context context) [17]
	// public synchronized int getUserAgent()
	// public synchronized String getUserAgentString()
	// public synchronized void setUserAgent(int ua)
	// public synchronized void setUserAgentString (String ua)
	// http://developer.android.com/reference/android/webkit/WebSettings.html
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.2_r1/android/webkit/WebSettings.java

	private enum Methods {
		getDefaultUserAgent, getUserAgent, getUserAgentString, setUserAgent, setUserAgentString
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		Util.log(null, Log.INFO, "Hooking class=" + className + " uid=" + Binder.getCallingUid());

		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebSettings(Methods.getDefaultUserAgent, PrivacyManager.cView, className));
		listHook.add(new XWebSettings(Methods.getUserAgent, PrivacyManager.cView, className));
		listHook.add(new XWebSettings(Methods.getUserAgentString, PrivacyManager.cView, className));
		listHook.add(new XWebSettings(Methods.setUserAgent, PrivacyManager.cView, className));
		listHook.add(new XWebSettings(Methods.setUserAgentString, PrivacyManager.cView, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case getDefaultUserAgent:
			int uid = Binder.getCallingUid();
			if (getRestricted(uid)) {
				String ua = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
				param.setResult(ua);
			}
			break;

		case getUserAgent:
			if (isRestricted(param))
				param.setResult(-1); // User defined
			break;

		case getUserAgentString:
			if (isRestrictedExtra(param, (String) param.getResult())) {
				String ua = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
				param.setResult(ua);
			}
			break;

		case setUserAgent:
			if (param.args.length > 0)
				if (isRestricted(param))
					param.args[0] = -1; // User defined
			break;

		case setUserAgentString:
			if (param.args.length > 0)
				if (isRestricted(param)) {
					String ua = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA");
					param.args[0] = ua;
				}
			break;

		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

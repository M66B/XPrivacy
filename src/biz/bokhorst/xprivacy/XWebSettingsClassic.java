package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.os.Build;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XWebSettingsClassic extends XHook {
	private Methods mMethod;

	private XWebSettingsClassic(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "android.webkit.WebSettingsClassic";
	}

	// @formatter:off

	// public static String getDefaultUserAgentForLocale(Context context, Locale locale)
	// public synchronized String getUserAgentString()
	// public synchronized void setUserAgentString(String ua)
	// frameworks/base/core/java/android/webkit/WebSettingsClassic.java
	// http://developer.android.com/reference/android/webkit/WebSettings.html

	// @formatter:on

	private enum Methods {
		getDefaultUserAgentForLocale, getUserAgentString, setUserAgentString
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebSettingsClassic(Methods.getDefaultUserAgentForLocale, PrivacyManager.cView,
				Build.VERSION_CODES.JELLY_BEAN_MR1));
		listHook.add(new XWebSettingsClassic(Methods.getUserAgentString, PrivacyManager.cView,
				Build.VERSION_CODES.CUPCAKE));
		listHook.add(new XWebSettingsClassic(Methods.setUserAgentString, PrivacyManager.cView,
				Build.VERSION_CODES.CUPCAKE));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod.equals(Methods.setUserAgentString))
			if (isRestricted(param) && param.args.length > 0)
				param.args[0] = XWebSettings.cUserAgent;
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod.equals(Methods.getDefaultUserAgentForLocale) || mMethod.equals(Methods.getUserAgentString))
			if (isRestricted(param))
				param.setResult(XWebSettings.cUserAgent);
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		if (param.thisObject == null) {
			if (param.args.length > 0)
				context = (Context) param.args[0];
		} else
			try {
				Field fieldContext = findField(param.thisObject.getClass(), "mContext");
				context = (Context) fieldContext.get(param.thisObject);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

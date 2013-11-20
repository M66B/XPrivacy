package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XWebSettings extends XHook {
	private Methods mMethod;

	private XWebSettings(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "android.webkit.WebSettings";
	}

	// public static String getDefaultUserAgent(Context context)
	// frameworks/base/core/java/android/webkit/WebSettings.java
	// http://developer.android.com/reference/android/webkit/WebSettings.html

	private enum Methods {
		getDefaultUserAgent
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWebSettings(Methods.getDefaultUserAgent, PrivacyManager.cView,
				Build.VERSION_CODES.JELLY_BEAN_MR1));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getDefaultUserAgent) {
			if (isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "UA"));
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
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

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContextWrapper extends XHook {
	private Methods mMethod;
	private static boolean mClipboardManagerHooked = false;
	private static boolean mWindowManagerHooked = false;

	private XContextWrapper(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.content.ContextWrapper";
	}

	// public Context getApplicationContext()
	// public Context getBaseContext()
	// public Object getSystemService(String name)
	// frameworks/base/core/java/android/content/ContextWrapper.java

	private enum Methods {
		getApplicationContext, getBaseContext, getSystemService
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XContextWrapper(Methods.getApplicationContext, null));
		listHook.add(new XContextWrapper(Methods.getBaseContext, null));
		listHook.add(new XContextWrapper(Methods.getSystemService, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
		if (mMethod == Methods.getApplicationContext || mMethod == Methods.getBaseContext) {
			int uid = Binder.getCallingUid();
			Context context = (Context) param.getResult();
			if (context != null && PrivacyManager.isExtraUsageDataEnabled(uid))
				PrivacyManager.sendUsageData(this, context);
		} else if (mMethod == Methods.getSystemService) {
			String name = (String) param.args[0];
			Object result = param.getResult();
			Util.log(this, Log.INFO, "getSystemService " + name + "="
					+ (result == null ? "null" : result.getClass().getName()));
			if (name != null && result != null) {
				if (name.equals(Context.CLIPBOARD_SERVICE)) {
					// Clipboard service
					if (!mClipboardManagerHooked) {
						XPrivacy.hookAll(XClipboardManager.getInstances(result));
						mClipboardManagerHooked = true;
					}
				} else if (name.equals(Context.WINDOW_SERVICE)) {
					// Window service
					if (!mWindowManagerHooked) {
						XPrivacy.hookAll(XWindowManager.getInstances(result));
						mWindowManagerHooked = true;
					}
				}
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

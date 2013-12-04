package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XService extends XHook {
	private Methods mMethod;

	public XService(Methods method, String restrictionName, String actionName) {
		super(restrictionName, method.name(), actionName);
		mMethod = method;
	}

	@Override
	public String getClassName() {
		return "android.app.Service";
	}

	// public void onDestroy()
	// frameworks/base/core/java/android/app/Service.java
	// http://developer.android.com/reference/android/app/Service.html

	private enum Methods {
		onDestroy
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XService(Methods.onDestroy, null, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.onDestroy) {
			int uid = Binder.getCallingUid();
			Util.log(this, Log.INFO, "Service destroyed uid=" + uid);
			try {
				if (PrivacyManager.isUsageDataEnabled(uid))
					PrivacyManager.sendUsageData(this, (Context) param.thisObject);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

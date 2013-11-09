package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContextWrapper extends XHook {
	private Methods mMethod;

	private XContextWrapper(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.content.ContextWrapper";
	}

	// public Context getBaseContext()
	// frameworks/base/core/java/android/content/ContextWrapper.java

	private enum Methods {
		getBaseContext
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XContextWrapper(Methods.getBaseContext, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
		if (mMethod == Methods.getBaseContext) {
			Context context = (Context) param.getResult();
			if (context != null && PrivacyManager.isUsageDataEnabled(Binder.getCallingUid()))
				PrivacyManager.sendUsageData(this, context);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

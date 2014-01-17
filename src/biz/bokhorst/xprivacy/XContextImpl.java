package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContextImpl extends XHook {
	private Methods mMethod;

	private XContextImpl(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.app.ContextImpl";
	}

	// public Object getSystemService(String name)
	// frameworks/base/core/java/android/app/ContextImpl.java

	private enum Methods {
		getPackageManager, getSystemService
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XContextImpl(Methods.getPackageManager, null));
		listHook.add(new XContextImpl(Methods.getSystemService, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
		if (mMethod == Methods.getPackageManager) {
			Object instance = param.getResult();
			if (instance != null)
				XPrivacy.handleGetSystemService(this, "PackageManager", instance);
		} else if (mMethod == Methods.getSystemService) {
			if (param.args.length > 0 && param.args[0] != null) {
				String name = (String) param.args[0];
				Object instance = param.getResult();
				if (name != null && instance != null)
					XPrivacy.handleGetSystemService(this, name, instance);
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

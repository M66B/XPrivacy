package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.pm.PackageManager;
import android.util.Log;

public class XContextImpl extends XHook {
	private Methods mMethod;

	private XContextImpl(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	private XContextImpl(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "android.app.ContextImpl";
	}

	@Override
	public boolean isVisible() {
		return (mMethod != Methods.enforce);
	}

	// @formatter:off

	// private void enforce(String permission, int resultOfCheck, boolean selfToo, int uid, String message)
	// public PackageManager getPackageManager()
	// public Object getSystemService(String name)
	// frameworks/base/core/java/android/app/ContextImpl.java

	// @formatter:on

	private enum Methods {
		enforce, getPackageManager, getSystemService
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XContextImpl(Methods.enforce, PrivacyManager.cSystem));
		listHook.add(new XContextImpl(Methods.getPackageManager, null, 1));
		listHook.add(new XContextImpl(Methods.getSystemService, null, 1));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
		if (mMethod == Methods.enforce) {
			if (param.args.length > 3 && param.args[0] instanceof String && param.args[1] instanceof Integer
					&& param.args[3] instanceof Integer) {
				String permission = (String) param.args[0];
				int resultOfCheck = (Integer) param.args[1];
				int uid = (Integer) param.args[3];

				if (resultOfCheck == PackageManager.PERMISSION_GRANTED) {
					String extra = permission.replace("android.permission.", "");
					if (isRestrictedExtra(uid, getRestrictionName(), getMethodName(), extra))
						param.setResult(new SecurityException("XPrivacy"));
				}
			}

		} else if (mMethod == Methods.getPackageManager) {
			Object instance = param.getResult();
			if (instance != null)
				XPrivacy.handleGetSystemService("PackageManager", instance.getClass().getName(), getSecret());

		} else if (mMethod == Methods.getSystemService) {
			if (param.args.length > 0 && param.args[0] != null) {
				String name = (String) param.args[0];
				Object instance = param.getResult();
				if (name != null && instance != null)
					XPrivacy.handleGetSystemService(name, instance.getClass().getName(), getSecret());
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

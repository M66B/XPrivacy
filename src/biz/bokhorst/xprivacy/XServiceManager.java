package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XServiceManager extends XHook {
	private Methods mMethod;

	private XServiceManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	private XServiceManager(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "android.os.ServiceManager";
	}

	// @formatter:off

	// public static IBinder getService(String name)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/android/os/ServiceManager.java/

	// @formatter:on

	private enum Methods {
		getService
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XServiceManager(Methods.getService, PrivacyManager.cIPC));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.getService) {
			if (param.args.length > 0) {
				String name = (String) param.args[0];
				if (XBinder.cServiceName.contains(name) && isRestrictedExtra(param, name))
					param.setResult(null);
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

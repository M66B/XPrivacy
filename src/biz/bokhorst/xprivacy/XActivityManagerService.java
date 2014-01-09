package biz.bokhorst.xprivacy;

import android.os.Process;
import android.util.Log;

import java.util.ArrayList;
import java.util.List;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityManagerService extends XHook {
	private Methods mMethod;

	private XActivityManagerService(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.server.am.ActivityManagerService";
	}

	public boolean isVisible() {
		return false;
	}

	// void enforceNotIsolatedCaller(String caller)

	private enum Methods {
		enforceNotIsolatedCaller
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XActivityManagerService(Methods.enforceNotIsolatedCaller, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.enforceNotIsolatedCaller) {
			if (PrivacyManager.isIsolated(Process.myUid())) {
				String caller = (String) param.args[0];
				Util.log(this, Log.WARN, "enforceNotIsolatedCaller(" + caller + ") uid=" + Process.myUid());
				if ("getContentProvider".equals(caller))
					param.setResult(null);
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

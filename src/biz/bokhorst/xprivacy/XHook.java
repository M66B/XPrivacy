package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {
	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	public Boolean isAllowed(int uid, String propertyName) {
		// Get permissions
		// TODO: cache
		String[] permissions = System.getProperty(propertyName, "").split(",");
		boolean defaultAllowed = (permissions.length > 0 && permissions[0].equals("*"));
		List<Integer> lstExcept = new ArrayList<Integer>();
		for (int idx = 1; idx < permissions.length; idx++)
			lstExcept.add(Integer.parseInt(permissions[idx]));

		// Check if allowed
		boolean allowed = !lstExcept.contains(uid);
		if (!defaultAllowed)
			allowed = !allowed;

		// Result
		info("uid=" + uid + " allowed=" + allowed);
		return allowed;
	}

	public void debug(String message) {
		XUtil.log(this, XUtil.LOG_DEBUG, message);
	}

	public void info(String message) {
		XUtil.log(this, XUtil.LOG_INFO, message);
	}

	public void warning(String message) {
		XUtil.log(this, XUtil.LOG_WARNING, message);
	}

	public void error(String message) {
		XUtil.log(this, XUtil.LOG_ERROR, message);
	}
}

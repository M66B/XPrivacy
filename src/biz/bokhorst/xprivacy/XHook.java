package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.text.TextUtils;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {
	private static final String cPermissionPrefix = "XPrivacy.";

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	public boolean isAllowed(int uid, String permissionName) {
		// Get permissions
		String prop = System.getProperty(cPermissionPrefix + permissionName, "*");
		String[] permissions = prop.split(",");

		// Decode permissions
		List<String> listPermission = new ArrayList<String>();
		listPermission.addAll(Arrays.asList(permissions));
		boolean defaultAllowed = listPermission.get(0).equals("*");

		// Check if allowed
		String sUid = Integer.toString(uid);
		boolean allowed = !listPermission.contains(sUid);
		if (!defaultAllowed)
			allowed = !allowed;

		// Result
		info("uid=" + uid + " permission=" + permissionName + " allowed=" + allowed + " prop=" + prop);
		return allowed;
	}

	public void setAllowed(int uid, String permissionName, boolean allowed) {
		// Get permissions
		String prop = System.getProperty(cPermissionPrefix + permissionName, "*");
		String[] permissions = prop.split(",");

		// Decode permissions
		List<String> listPermission = new ArrayList<String>();
		listPermission.addAll(Arrays.asList(permissions));
		boolean defaultAllowed = listPermission.get(0).equals("*");

		// Allow or deny
		String sUid = Integer.toString(uid);
		if (defaultAllowed ? allowed : !allowed)
			listPermission.remove(sUid);
		if (defaultAllowed ? !allowed : allowed)
			if (!listPermission.contains(sUid))
				listPermission.add(sUid);

		// Set permissions
		prop = TextUtils.join(",", listPermission);
		System.setProperty(cPermissionPrefix + permissionName, prop);
		info("set uid=" + uid + " permission=" + permissionName + " allowed=" + allowed + " prop=" + prop);
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

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.content.Context;
import android.text.TextUtils;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {
	private static final String cPermissionPrefix = "XPrivacy.";
	public static final String[] cPermissionNames = new String[] { XContactProvider2query.cPermissionName,
			XLocationManager.cPermissionName };

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	protected void initialize(Context context) {
		info(context.getApplicationInfo().packageName);
		for (String permissionName : cPermissionNames)
			System.setProperty(cPermissionPrefix + permissionName, "*");
	}

	protected boolean isAllowed(int uid, String permissionName) {
		try {
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
		} catch (Exception ex) {
			XUtil.bug(this, ex);
			return true;
		}
	}

	protected void setAllowed(int uid, String permissionName, boolean allowed) {
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

	protected void debug(String message) {
		XUtil.log(this, XUtil.LOG_DEBUG, message);
	}

	protected void info(String message) {
		XUtil.log(this, XUtil.LOG_INFO, message);
	}

	protected void warning(String message) {
		XUtil.log(this, XUtil.LOG_WARNING, message);
	}

	protected void error(String message) {
		XUtil.log(this, XUtil.LOG_ERROR, message);
	}
}

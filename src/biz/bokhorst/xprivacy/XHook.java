package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.text.TextUtils;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	protected boolean isAllowed(Context context, int uid, String permissionName, boolean usage) {
		try {
			// Check context
			if (context == null) {
				warning("context is null");
				return true;
			}

			// Check uid
			if (uid == 0) {
				warning("uid=0");
				return true;
			}

			// Get permissions
			String[] permissions = getPermissions(context, uid, permissionName, usage);

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
			info(String.format("get package=%s permission=%s allowed=%b", XUtil.getPackageName(context, uid),
					permissionName, allowed));
			return allowed;
		} catch (Throwable ex) {
			XUtil.bug(this, ex);
			return true;
		}
	}

	protected void setAllowed(Context context, int uid, String permissionName, boolean allowed) {
		// Check context
		if (context == null) {
			warning("context is null");
			return;
		}

		// Get permissions
		String[] permissions = getPermissions(context, uid, permissionName, false);

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
		permissions = listPermission.toArray(new String[listPermission.size()]);
		setPermissions(context, uid, permissionName, permissions);
		info(String.format("set package=%s permission=%s allowed=%b", XUtil.getPackageName(context, uid),
				permissionName, allowed));
	}

	private String[] getPermissions(Context context, int uid, String permissionName, boolean usage) {
		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			warning("contentResolver is null");
			return null;
		}

		// Query permissions
		Cursor cursor = contentResolver.query(XPrivacyProvider.URI_PERMISSIONS, null, null, new String[] {
				permissionName, (usage ? Integer.toString(uid) : null) }, null);
		if (cursor == null) {
			warning("cursor is null");
			return null;
		}
		if (!cursor.moveToNext()) {
			warning("cursor is empty");
			return null;
		}

		// Get permissions
		String permission = cursor.getString(cursor.getColumnIndex(XPrivacyProvider.COL_PERMISSION));
		cursor.close();

		// Return permissions
		return (permission == null ? null : permission.split(","));
	}

	private void setPermissions(Context context, int uid, String permissionName, String[] permissions) {
		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			warning("contentResolver is null");
			return;
		}

		// Set permissions
		ContentValues values = new ContentValues();
		values.put(XPrivacyProvider.COL_NAME, permissionName);
		values.put(XPrivacyProvider.COL_PERMISSION, TextUtils.join(",", permissions));
		contentResolver.update(XPrivacyProvider.URI_PERMISSIONS, values, null, null);
	}

	protected void debug(String message) {
		XUtil.log(this, Log.DEBUG, message);
	}

	protected void info(String message) {
		XUtil.log(this, Log.INFO, message);
	}

	protected void warning(String message) {
		XUtil.log(this, Log.WARN, message);
	}

	protected void error(String message) {
		XUtil.log(this, Log.ERROR, message);
	}
}

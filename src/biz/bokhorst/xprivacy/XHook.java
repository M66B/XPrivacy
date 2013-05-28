package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.text.TextUtils;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {
	public static final String[] cPermissionNames = new String[] { XContactProvider2.cPermissionName, };

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	private String[] getPermissions(Context context, String permissionName) {
		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			warning("contentResolver is null");
			return null;
		}

		// Query permissions
		Cursor cursor = contentResolver.query(XContentProvider.CONTENT_URI, null, permissionName, null, null);
		if (cursor == null) {
			warning("cursor is null");
			return null;
		}
		if (!cursor.moveToNext()) {
			warning("cursor is empty");
			return null;
		}

		// Get permissions
		String permission = cursor.getString(cursor.getColumnIndex(XContentProvider.COL_PERMISSION));
		cursor.close();

		// Return permissions
		return (permission == null ? null : permission.split(","));
	}

	private void setPermissions(Context context, String permissionName, String[] permissions) {
		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			warning("contentResolver is null");
			return;
		}

		// Set permissions
		ContentValues values = new ContentValues();
		values.put(XContentProvider.COL_NAME, permissionName);
		values.put(XContentProvider.COL_PERMISSION, TextUtils.join(",", permissions));
		contentResolver.update(XContentProvider.CONTENT_URI, values, null, null);
	}

	protected boolean isAllowed(Context context, int uid, String permissionName) {
		try {
			// Check context
			if (context == null) {
				warning("context is null");
				return true;
			}

			// Get permissions
			String[] permissions = getPermissions(context, permissionName);

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
		} catch (Exception ex) {
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
		String[] permissions = getPermissions(context, permissionName);

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
		setPermissions(context, permissionName, permissions);
		info(String.format("set package=%s permission=%s allowed=%b", XUtil.getPackageName(context, uid),
				permissionName, allowed));
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

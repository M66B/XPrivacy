package biz.bokhorst.xprivacy;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {

	private String mPermissionName;

	public XHook(String permissionName) {
		mPermissionName = permissionName;
	}

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	protected boolean isAllowed(Context context, int uid, boolean usage) {
		return isAllowed(context, uid, mPermissionName, usage);
	}

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

			// Get content resolver
			ContentResolver contentResolver = context.getContentResolver();
			if (contentResolver == null) {
				warning("contentResolver is null");
				return true;
			}

			// Query permission
			Cursor cursor = contentResolver.query(XPrivacyProvider.URI_PERMISSIONS, null, permissionName, new String[] {
					Integer.toString(uid), Boolean.toString(usage) }, null);
			if (cursor == null) {
				warning("cursor is null");
				return true;
			}

			// Get permission
			boolean allowed = true;
			if (cursor.moveToNext())
				allowed = Boolean.parseBoolean(cursor.getString(cursor.getColumnIndex(XPrivacyProvider.COL_ALLOWED)));
			else
				warning("cursor is empty");
			cursor.close();

			// Result
			info(String.format("get package=%s permission=%s allowed=%b", XUtil.getPackageName(context, uid),
					permissionName, allowed));
			return allowed;
		} catch (Throwable ex) {
			XUtil.bug(this, ex);
			return true;
		}
	}

	protected void setAllowed(Context context, int uid, boolean allowed) {
		setAllowed(context, uid, mPermissionName, allowed);
	}

	protected void setAllowed(Context context, int uid, String permissionName, boolean allowed) {
		// Check context
		if (context == null) {
			warning("context is null");
			return;
		}

		// Check uid
		if (uid == 0) {
			warning("uid=0");
			return;
		}

		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			warning("contentResolver is null");
			return;
		}

		// Set permissions
		ContentValues values = new ContentValues();
		values.put(XPrivacyProvider.COL_ALLOWED, Boolean.toString(allowed));
		contentResolver.update(XPrivacyProvider.URI_PERMISSIONS, values, permissionName, null);

		info(String.format("set package=%s permission=%s allowed=%b", XUtil.getPackageName(context, uid),
				permissionName, allowed));
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

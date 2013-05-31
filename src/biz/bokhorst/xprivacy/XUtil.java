package biz.bokhorst.xprivacy;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.util.Log;

public class XUtil {

	public static boolean getAllowed(XHook hook, Context context, int uid, String permissionName, boolean usage) {
		try {
			// Check context
			if (context == null) {
				log(hook, Log.WARN, "context is null");
				return true;
			}

			// Check uid
			if (uid == 0) {
				log(hook, Log.WARN, "uid=0");
				return true;
			}

			// Get content resolver
			ContentResolver contentResolver = context.getContentResolver();
			if (contentResolver == null) {
				log(hook, Log.WARN, "contentResolver is null");
				return true;
			}

			// Query permission
			Cursor cursor = contentResolver.query(XPrivacyProvider.URI_PERMISSIONS, null, permissionName, new String[] {
					Integer.toString(uid), Boolean.toString(usage) }, null);
			if (cursor == null) {
				log(hook, Log.WARN, "cursor is null");
				return true;
			}

			// Get permission
			boolean allowed = true;
			if (cursor.moveToNext())
				allowed = Boolean.parseBoolean(cursor.getString(cursor.getColumnIndex(XPrivacyProvider.COL_ALLOWED)));
			else
				log(hook, Log.WARN, "cursor is empty");
			cursor.close();

			// Result
			log(hook, Log.INFO, String.format("get method=%s/%s permission=%s allowed=%b",
					XUtil.getPackageName(context, uid), (hook == null ? null : hook.getMethodName()), permissionName,
					allowed));
			return allowed;
		} catch (Throwable ex) {
			XUtil.bug(hook, ex);
			return true;
		}
	}

	public static void setAllowed(XHook hook, Context context, int uid, String permissionName, boolean allowed) {
		// Check context
		if (context == null) {
			log(hook, Log.WARN, "context is null");
			return;
		}

		// Check uid
		if (uid == 0) {
			log(hook, Log.WARN, "uid=0");
			return;
		}

		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			log(hook, Log.WARN, "contentResolver is null");
			return;
		}

		// Set permissions
		ContentValues values = new ContentValues();
		values.put(XPrivacyProvider.COL_UID, uid);
		values.put(XPrivacyProvider.COL_ALLOWED, Boolean.toString(allowed));
		contentResolver.update(XPrivacyProvider.URI_PERMISSIONS, values, permissionName, null);

		log(hook, Log.INFO, String.format("set method=%s.%s permission=%s allowed=%b",
				XUtil.getPackageName(context, uid), (hook == null ? null : hook.getMethodName()), permissionName,
				allowed));
	}

	public static String getPackageName(Context context, int uid) {
		String[] packages = context.getPackageManager().getPackagesForUid(uid);
		if (packages != null && packages.length == 1)
			return packages[0];
		return Integer.toString(uid);
	}

	public static void log(XHook hook, int priority, String msg) {
		if (priority != Log.DEBUG)
			if (hook == null)
				Log.println(priority, "XPrivacy", msg);
			else
				Log.println(priority, String.format("XPrivacy/%s", hook.getClass().getSimpleName()), msg);
	}

	public static void bug(XHook hook, Throwable ex) {
		log(hook, Log.ERROR, ex.toString());
		ex.printStackTrace();
	}
}

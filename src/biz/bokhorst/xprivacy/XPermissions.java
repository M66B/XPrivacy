package biz.bokhorst.xprivacy;

import java.util.LinkedHashMap;
import java.util.Map;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.util.Log;

public class XPermissions {

	// This should correspond with perm_<name> in strings.xml
	public static final String cBrowser = "browser";
	public static final String cCalendar = "calendar";
	public static final String cContacts = "contacts";
	public static final String cIdentification = "identification";
	public static final String cLocation = "location";
	public static final String cMessages = "messages";
	public static final String cPhone = "phone";
	public static final String cVoicemail = "voicemail";

	public static Map<String, String[]> cPermissions = new LinkedHashMap<String, String[]>();

	static {
		cPermissions.put(cBrowser, new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" });
		cPermissions.put(cCalendar, new String[] { "READ_CALENDAR" });
		cPermissions.put(cContacts, new String[] { "READ_CONTACTS" });
		cPermissions.put(cIdentification, new String[] {});
		cPermissions.put(cLocation, new String[] { "ACCESS_FINE_LOCATION", "ACCESS_COARSE_LOCATION" });
		cPermissions.put(cMessages, new String[] { "READ_SMS" });
		cPermissions.put(cPhone, new String[] { "READ_CALL_LOG", "READ_PHONE_STATE" });
		cPermissions.put(cVoicemail, new String[] { "READ_WRITE_ALL_VOICEMAIL" });
	}

	// android.intent.action.NEW_OUTGOING_CALL: PROCESS_OUTGOING_CALLS

	public static final String cDefaceString = "DEFACE";
	public static final long cDefaceHex = 0xDEFACEL;

	public static boolean hasInternet(Context context, String packageName) {
		PackageManager pm = context.getPackageManager();
		return (pm.checkPermission("android.permission.INTERNET", packageName) == PackageManager.PERMISSION_GRANTED);
	}

	public static boolean isGranted(Context context, String packageName, String permissionName) {
		String[] aPermissions = cPermissions.get(permissionName);
		if (aPermissions.length == 0)
			return true;
		PackageManager pm = context.getPackageManager();
		for (String aPermission : aPermissions)
			if (pm.checkPermission("android.permission." + aPermission, packageName) == PackageManager.PERMISSION_GRANTED)
				return true;
		return false;
	}

	public static boolean isUsed(Context context, int uid, String permissionName) {
		ContentResolver cr = context.getContentResolver();
		Cursor cursor = cr.query(XPrivacyProvider.URI_LASTUSED, null, permissionName,
				new String[] { Integer.toString(uid) }, null);
		if (cursor.moveToNext()) {
			long lastUsage = cursor.getLong(cursor.getColumnIndex(XPrivacyProvider.COL_LASTUSED));
			cursor.close();
			return (lastUsage != 0);
		}
		return false;
	}

	public static String getLocalizedName(Context context, String permissionName) {
		String packageName = XPermissions.class.getPackage().getName();
		int stringId = context.getResources().getIdentifier("perm_" + permissionName, "string", packageName);
		return (stringId == 0 ? null : context.getString(stringId));
	}

	public static boolean getAllowed(XHook hook, Context context, int uid, String permissionName, boolean usage) {
		try {
			// Check context
			if (context == null) {
				XUtil.log(hook, Log.WARN, "context is null");
				return true;
			}

			// Check uid
			if (uid == 0) {
				XUtil.log(hook, Log.WARN, "uid=0");
				return true;
			}

			// Get content resolver
			ContentResolver contentResolver = context.getContentResolver();
			if (contentResolver == null) {
				XUtil.log(hook, Log.WARN, "contentResolver is null");
				return true;
			}

			// Query permission
			Cursor cursor = contentResolver.query(XPrivacyProvider.URI_PERMISSIONS, null, permissionName, new String[] {
					Integer.toString(uid), Boolean.toString(usage) }, null);
			if (cursor == null) {
				XUtil.log(hook, Log.WARN, "cursor is null");
				return true;
			}

			// Get permission
			boolean allowed = true;
			if (cursor.moveToNext())
				allowed = Boolean.parseBoolean(cursor.getString(cursor.getColumnIndex(XPrivacyProvider.COL_ALLOWED)));
			else
				XUtil.log(hook, Log.WARN, "cursor is empty");
			cursor.close();

			// Result
			XUtil.log(hook, Log.INFO,
					String.format("get method=%s/%s permission=%s allowed=%b", getPackageName(context, uid),
							(hook == null ? null : hook.getMethodName()), permissionName, allowed));
			return allowed;
		} catch (Throwable ex) {
			XUtil.bug(hook, ex);
			return true;
		}
	}

	public static void setAllowed(XHook hook, Context context, int uid, String permissionName, boolean allowed) {
		// Check context
		if (context == null) {
			XUtil.log(hook, Log.WARN, "context is null");
			return;
		}

		// Check uid
		if (uid == 0) {
			XUtil.log(hook, Log.WARN, "uid=0");
			return;
		}

		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			XUtil.log(hook, Log.WARN, "contentResolver is null");
			return;
		}

		// Set permissions
		ContentValues values = new ContentValues();
		values.put(XPrivacyProvider.COL_UID, uid);
		values.put(XPrivacyProvider.COL_ALLOWED, Boolean.toString(allowed));
		contentResolver.update(XPrivacyProvider.URI_PERMISSIONS, values, permissionName, null);

		XUtil.log(hook, Log.INFO, String.format("set method=%s.%s permission=%s allowed=%b",
				getPackageName(context, uid), (hook == null ? null : hook.getMethodName()), permissionName, allowed));
	}

	private static String getPackageName(Context context, int uid) {
		String[] packages = context.getPackageManager().getPackagesForUid(uid);
		if (packages != null && packages.length == 1)
			return packages[0];
		return Integer.toString(uid);
	}
}

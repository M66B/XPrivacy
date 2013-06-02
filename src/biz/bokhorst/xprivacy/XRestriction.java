package biz.bokhorst.xprivacy;

import java.util.LinkedHashMap;
import java.util.Map;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.util.Log;

public class XRestriction {

	// This should correspond with restrict_<name> in strings.xml
	public static final String cBrowser = "browser";
	public static final String cCalendar = "calendar";
	public static final String cContacts = "contacts";
	public static final String cIdentification = "identification";
	public static final String cLocation = "location";
	public static final String cMedia = "media";
	public static final String cMessages = "messages";
	public static final String cPhone = "phone";
	public static final String cVoicemail = "voicemail";

	public static Map<String, String[]> cRestriction = new LinkedHashMap<String, String[]>();

	static {
		cRestriction.put(cBrowser, new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" });
		cRestriction.put(cCalendar, new String[] { "READ_CALENDAR" });
		cRestriction.put(cContacts, new String[] { "READ_CONTACTS" });
		cRestriction.put(cIdentification, new String[] {});
		cRestriction.put(cLocation, new String[] { "ACCESS_FINE_LOCATION", "ACCESS_COARSE_LOCATION" });
		cRestriction.put(cMedia, new String[] {}); // TODO: permissions
		cRestriction.put(cMessages, new String[] { "READ_SMS", "READ_WRITE_ALL_VOICEMAIL" });
		cRestriction.put(cPhone, new String[] { "READ_CALL_LOG", "READ_PHONE_STATE", "PROCESS_OUTGOING_CALLS" });
	}

	public static final String cDefaceString = "DEFACE";
	public static final long cDefaceHex = 0xDEFACEL;

	public static boolean hasInternet(Context context, String packageName) {
		PackageManager pm = context.getPackageManager();
		return (pm.checkPermission("android.permission.INTERNET", packageName) == PackageManager.PERMISSION_GRANTED);
	}

	public static boolean isGranted(Context context, String packageName, String restrictionName) {
		String[] aRestrictions = cRestriction.get(restrictionName);
		if (aRestrictions.length == 0)
			return true;
		PackageManager pm = context.getPackageManager();
		for (String aRestriction : aRestrictions)
			if (pm.checkPermission("android.permission." + aRestriction, packageName) == PackageManager.PERMISSION_GRANTED)
				return true;
		return false;
	}

	public static boolean isUsed(Context context, int uid, String restrictionName) {
		ContentResolver cr = context.getContentResolver();
		Cursor cursor = cr.query(XPrivacyProvider.URI_LASTUSED, null, restrictionName,
				new String[] { Integer.toString(uid) }, null);
		if (cursor.moveToNext()) {
			long lastUsage = cursor.getLong(cursor.getColumnIndex(XPrivacyProvider.COL_LASTUSED));
			cursor.close();
			return (lastUsage != 0);
		}
		return false;
	}

	public static String getLocalizedName(Context context, String restrictionName) {
		String packageName = XRestriction.class.getPackage().getName();
		int stringId = context.getResources().getIdentifier("restrict_" + restrictionName, "string", packageName);
		return (stringId == 0 ? null : context.getString(stringId));
	}

	public static boolean getRestricted(XHook hook, Context context, int uid, String restrictionName, boolean usage) {
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

			// Query restriction
			Cursor cursor = contentResolver.query(XPrivacyProvider.URI_RESTRICTIONS, null, restrictionName,
					new String[] { Integer.toString(uid), Boolean.toString(usage) }, null);
			if (cursor == null) {
				XUtil.log(hook, Log.WARN, "cursor is null");
				return true;
			}

			// Get restriction
			boolean restricted = true;
			if (cursor.moveToNext())
				restricted = Boolean.parseBoolean(cursor.getString(cursor
						.getColumnIndex(XPrivacyProvider.COL_RESTRICTED)));
			else
				XUtil.log(hook, Log.WARN, "cursor is empty");
			cursor.close();

			// Result
			XUtil.log(
					hook,
					Log.INFO,
					String.format("get %s/%s %s=%b", getPackageName(context, uid),
							(hook == null ? null : hook.getMethodName()), restrictionName, restricted));
			return restricted;
		} catch (Throwable ex) {
			XUtil.bug(hook, ex);
			return true;
		}
	}

	public static void setRestricted(XHook hook, Context context, int uid, String restrictionName, boolean restricted) {
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

		// Set restrictions
		ContentValues values = new ContentValues();
		values.put(XPrivacyProvider.COL_UID, uid);
		values.put(XPrivacyProvider.COL_RESTRICTED, Boolean.toString(restricted));
		contentResolver.update(XPrivacyProvider.URI_RESTRICTIONS, values, restrictionName, null);

		XUtil.log(
				hook,
				Log.INFO,
				String.format("set %s.%s %s=%b", getPackageName(context, uid),
						(hook == null ? null : hook.getMethodName()), restrictionName, restricted));
	}

	private static String getPackageName(Context context, int uid) {
		String[] packages = context.getPackageManager().getPackagesForUid(uid);
		if (packages != null && packages.length == 1)
			return packages[0];
		return Integer.toString(uid);
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.UriMatcher;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.Binder;
import android.text.TextUtils;
import android.util.Log;

public class XPrivacyProvider extends ContentProvider {

	public static final String AUTHORITY = "biz.bokhorst.xprivacy.provider";
	public static final String PATH_RESTRICTION = "restriction";
	public static final String PATH_USAGE = "usage";
	public static final String PATH_AUDIT = "audit";
	public static final String PATH_SETTING = "setting";
	public static final Uri URI_RESTRICTION = Uri.parse("content://" + AUTHORITY + "/" + PATH_RESTRICTION);
	public static final Uri URI_USAGE = Uri.parse("content://" + AUTHORITY + "/" + PATH_USAGE);
	public static final Uri URI_AUDIT = Uri.parse("content://" + AUTHORITY + "/" + PATH_AUDIT);
	public static final Uri URI_SETTING = Uri.parse("content://" + AUTHORITY + "/" + PATH_SETTING);

	public static final String COL_UID = "Uid";
	public static final String COL_RESTRICTION = "Restriction";
	public static final String COL_RESTRICTED = "Restricted";
	public static final String COL_METHOD = "Method";
	public static final String COL_USED = "Used";
	public static final String COL_SETTING = "Setting";
	public static final String COL_ENABLED = "Enabled";

	private static final UriMatcher sUriMatcher;
	private static final int TYPE_RESTRICTION = 1;
	private static final int TYPE_USAGE = 2;
	private static final int TYPE_AUDIT = 3;
	private static final int TYPE_SETTING = 4;

	private static final String cPackages = "Packages";

	static {
		sUriMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sUriMatcher.addURI(AUTHORITY, PATH_RESTRICTION, TYPE_RESTRICTION);
		sUriMatcher.addURI(AUTHORITY, PATH_USAGE, TYPE_USAGE);
		sUriMatcher.addURI(AUTHORITY, PATH_AUDIT, TYPE_AUDIT);
		sUriMatcher.addURI(AUTHORITY, PATH_SETTING, TYPE_SETTING);
	}

	@Override
	public boolean onCreate() {
		return true;
	}

	@Override
	public String getType(Uri uri) {
		if (sUriMatcher.match(uri) == TYPE_RESTRICTION)
			return String.format("vnd.android.cursor.dir/%s.%s", AUTHORITY, PATH_RESTRICTION);
		else if (sUriMatcher.match(uri) == TYPE_USAGE)
			return String.format("vnd.android.cursor.dir/%s.%s", AUTHORITY, PATH_USAGE);
		else if (sUriMatcher.match(uri) == TYPE_AUDIT)
			return String.format("vnd.android.cursor.dir/%s.%s", AUTHORITY, PATH_AUDIT);
		else if (sUriMatcher.match(uri) == TYPE_SETTING)
			return String.format("vnd.android.cursor.dir/%s.%s", AUTHORITY, PATH_SETTING);
		throw new IllegalArgumentException();
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
		// Get preferences
		SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);

		if (sUriMatcher.match(uri) == TYPE_RESTRICTION && selectionArgs != null && selectionArgs.length >= 2) {
			// Get arguments
			String restrictionName = selection;
			int uid = Integer.parseInt(selectionArgs[0]);
			boolean usage = Boolean.parseBoolean(selectionArgs[1]);
			String methodName = (selectionArgs.length >= 3 ? selectionArgs[2] : null);

			// Update usage count
			if (usage) {
				long timestamp = new Date().getTime();
				SharedPreferences.Editor editor = prefs.edit();
				editor.putLong(getUsagePref(uid, restrictionName), timestamp);
				if (methodName != null)
					editor.putLong(getMethodPref(uid, restrictionName, methodName), timestamp);
				editor.commit();
			}

			// Get restrictions
			String restrictions = prefs.getString(getRestrictionPref(restrictionName), "*");

			// Decode restrictions
			List<String> listRestriction = new ArrayList<String>(Arrays.asList(restrictions.split(",")));
			boolean defaultRestricted = listRestriction.get(0).equals("*");

			// Check if restricted
			boolean restricted = !listRestriction.contains(Integer.toString(uid));
			if (!defaultRestricted)
				restricted = !restricted;

			// Return restriction
			MatrixCursor cursor = new MatrixCursor(new String[] { COL_UID, COL_RESTRICTION, COL_RESTRICTED });
			cursor.addRow(new Object[] { uid, restrictionName, Boolean.toString(!restricted) });
			return cursor;
		} else if (sUriMatcher.match(uri) == TYPE_USAGE && selectionArgs != null && selectionArgs.length == 1) {
			// Return usage
			String restrictionName = selection;
			int uid = Integer.parseInt(selectionArgs[0]);
			MatrixCursor cursor = new MatrixCursor(new String[] { COL_UID, COL_RESTRICTION, COL_USED });
			cursor.addRow(new Object[] { uid, restrictionName, prefs.getLong(getUsagePref(uid, restrictionName), 0) });
			return cursor;
		} else if (sUriMatcher.match(uri) == TYPE_AUDIT && selectionArgs != null && selectionArgs.length == 1) {
			// Return audit
			String restrictionName = selection;
			int uid = Integer.parseInt(selectionArgs[0]);
			String prefix = getUsagePref(uid, restrictionName) + ".";
			MatrixCursor cursor = new MatrixCursor(new String[] { COL_UID, COL_RESTRICTION, COL_METHOD, COL_USED });
			for (String pref : prefs.getAll().keySet())
				if (pref.startsWith(prefix))
					cursor.addRow(new Object[] { uid, restrictionName, pref.substring(prefix.length()),
							prefs.getLong(pref, 0) });
			return cursor;
		} else if (sUriMatcher.match(uri) == TYPE_SETTING && selectionArgs == null) {
			// Return setting
			String settingName = selection;
			MatrixCursor cursor = new MatrixCursor(new String[] { COL_SETTING, COL_ENABLED });
			cursor.addRow(new Object[] { settingName, prefs.getBoolean(getSettingPref(settingName), false) });
			return cursor;
		}
		throw new IllegalArgumentException();
	}

	@Override
	public Uri insert(Uri uri, ContentValues values) {
		// Check access
		enforcePermission();

		throw new IllegalArgumentException();
	}

	@Override
	public int update(Uri uri, ContentValues values, String selection, String[] selectionArgs) {
		// Check access
		enforcePermission();

		// Get preferences
		SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);

		if (sUriMatcher.match(uri) == TYPE_RESTRICTION) {
			// Get arguments
			String restrictionName = selection;
			int uid = values.getAsInteger(COL_UID);
			boolean allowed = !Boolean.parseBoolean(values.getAsString(COL_RESTRICTED));

			// Get restrictions
			String restrictions = prefs.getString(getRestrictionPref(restrictionName), "*");

			// Decode restrictions
			List<String> listRestriction = new ArrayList<String>(Arrays.asList(restrictions.split(",")));
			boolean defaultAllowed = listRestriction.get(0).equals("*");

			// Allow or deny
			String sUid = Integer.toString(uid);
			if (defaultAllowed ? allowed : !allowed)
				listRestriction.remove(sUid);
			if (defaultAllowed ? !allowed : allowed)
				if (!listRestriction.contains(sUid))
					listRestriction.add(sUid);

			// Encode restrictions
			restrictions = TextUtils.join(",", listRestriction.toArray(new String[0]));

			// Update restriction
			SharedPreferences.Editor editor = prefs.edit();
			editor.putString(getRestrictionPref(restrictionName), restrictions);
			editor.commit();

			// Update package list
			if (XRestriction.cStorage.equals(restrictionName))
				updateStoragePackages(uid, XRestriction.cStorage, allowed);

			return 1; // rows
		} else if (sUriMatcher.match(uri) == TYPE_SETTING) {
			// Get arguments
			String settingName = selection;

			// Update setting
			boolean enabled = Boolean.parseBoolean(values.getAsString(COL_ENABLED));
			SharedPreferences.Editor editor = prefs.edit();
			editor.putBoolean(getSettingPref(settingName), enabled);
			editor.commit();

			return 1;
		}
		throw new IllegalArgumentException(uri.toString());
	}

	@Override
	public int delete(Uri uri, String where, String[] selectionArgs) {
		// Check access
		enforcePermission();

		if (sUriMatcher.match(uri) == TYPE_AUDIT && selectionArgs != null && selectionArgs.length == 1) {
			// Get arguments
			String restrictionName = where;
			int uid = Integer.parseInt(selectionArgs[0]);

			// Delete audit trail
			int rows = 0;
			String prefix = getUsagePref(uid, restrictionName);
			SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);
			SharedPreferences.Editor editor = prefs.edit();
			for (String pref : prefs.getAll().keySet())
				if (pref.startsWith(prefix)) {
					rows++;
					editor.remove(pref);
					XUtil.log(null, Log.INFO, "Removed audit=" + pref);
				}
			editor.commit();
			return rows;
		}
		throw new IllegalArgumentException();
	}

	private void enforcePermission() throws SecurityException {
		// Only XPrivacy can insert, update or delete
		int cuid = Binder.getCallingUid();
		String[] packages = getContext().getPackageManager().getPackagesForUid(cuid);
		List<String> listPackage = new ArrayList<String>(Arrays.asList(packages));
		String self = XPrivacyProvider.class.getPackage().getName();
		if (!listPackage.contains(self))
			throw new SecurityException();
	}

	// The following two methods represent an ugly, not very secure hack
	// Unfortunately the package manager service doesn't have a context
	// Please contact me if you know a better solution

	@SuppressWarnings("deprecation")
	@SuppressLint("WorldReadableFiles")
	private void updateStoragePackages(int uid, String restrictionName, boolean allowed) {
		// Get storage packages
		SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY + "." + XRestriction.cStorage,
				Context.MODE_WORLD_READABLE | Context.MODE_MULTI_PROCESS);
		String storagePackages = prefs.getString(getPackagesPref(restrictionName), "");

		// Current storage package list
		List<String> listStoragePackage = new ArrayList<String>();
		if (!storagePackages.equals(""))
			listStoragePackage.addAll(Arrays.asList(storagePackages.split(",")));

		// Update storage package list
		String[] packages = getContext().getPackageManager().getPackagesForUid(uid);
		if (packages != null)
			for (String storagePackage : packages)
				if (!allowed && !listStoragePackage.contains(storagePackage))
					listStoragePackage.add(storagePackage);
				else if (allowed && listStoragePackage.contains(storagePackage))
					listStoragePackage.remove(storagePackage);

		// Store storage package list
		storagePackages = TextUtils.join(",", listStoragePackage);
		SharedPreferences.Editor sEditor = prefs.edit();
		sEditor.putString(getPackagesPref(XRestriction.cStorage), storagePackages);
		sEditor.commit();
	}

	@SuppressWarnings("deprecation")
	@SuppressLint("WorldReadableFiles")
	public static boolean getRestricted(XHook hook, Context context, String packageName, String restrictionName,
			String methodName, boolean usage) throws Throwable {
		// Get storage packages
		Context xContext = XUtil.getXContext(context);
		SharedPreferences prefs = xContext.getSharedPreferences(AUTHORITY + "." + XRestriction.cStorage,
				Context.MODE_WORLD_READABLE | Context.MODE_MULTI_PROCESS);
		String storagePackages = prefs.getString(getPackagesPref(restrictionName), "");

		// Build storage package list
		List<String> listStoragePackage = new ArrayList<String>();
		if (!storagePackages.equals(""))
			listStoragePackage.addAll(Arrays.asList(storagePackages.split(",")));

		// Check if package restricted
		boolean restricted = listStoragePackage.contains(packageName);
		XUtil.log(hook, Log.INFO, "package=" + packageName + " restricted=" + restricted);
		return restricted;
	}

	private static String getRestrictionPref(String restrictionName) {
		return COL_RESTRICTED + "." + restrictionName;
	}

	private static String getUsagePref(int uid, String restrictionName) {
		return COL_USED + "." + uid + "." + restrictionName;
	}

	private static String getMethodPref(int uid, String restrictionName, String methodName) {
		return getUsagePref(uid, restrictionName) + "." + methodName;
	}

	private static String getSettingPref(String settingName) {
		return COL_SETTING + "." + settingName;
	}

	private static String getPackagesPref(String restrictionName) {
		return cPackages + "." + restrictionName;
	}
}

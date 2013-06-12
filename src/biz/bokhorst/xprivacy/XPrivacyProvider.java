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
	public static final Uri URI_RESTRICTION = Uri.parse("content://" + AUTHORITY + "/" + PATH_RESTRICTION);
	public static final Uri URI_USAGE = Uri.parse("content://" + AUTHORITY + "/" + PATH_USAGE);
	public static final Uri URI_AUDIT = Uri.parse("content://" + AUTHORITY + "/" + PATH_AUDIT);

	public static final String COL_UID = "Uid";
	public static final String COL_RESTRICTION = "Restriction";
	public static final String COL_RESTRICTED = "Restricted";
	public static final String COL_METHOD = "Method";
	public static final String COL_TIME = "Time";

	private static final UriMatcher sUriMatcher;
	private static final int TYPE_RESTRICTION = 1;
	private static final int TYPE_USAGE = 2;
	private static final int TYPE_AUDIT = 3;

	private static final String cPackages = "Packages";

	static {
		sUriMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sUriMatcher.addURI(AUTHORITY, PATH_RESTRICTION, TYPE_RESTRICTION);
		sUriMatcher.addURI(AUTHORITY, PATH_USAGE, TYPE_USAGE);
		sUriMatcher.addURI(AUTHORITY, PATH_AUDIT, TYPE_AUDIT);
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
		throw new IllegalArgumentException();
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String restrictionName, String[] selectionArgs, String sortOrder) {
		if (selectionArgs != null) {
			// Get preferences
			SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);

			if (sUriMatcher.match(uri) == TYPE_RESTRICTION && selectionArgs.length >= 2) {
				// Get arguments
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
			} else if (sUriMatcher.match(uri) == TYPE_USAGE && selectionArgs.length == 1) {
				// Return usage
				int uid = Integer.parseInt(selectionArgs[0]);
				MatrixCursor cursor = new MatrixCursor(new String[] { COL_UID, COL_RESTRICTION, COL_TIME });
				cursor.addRow(new Object[] { uid, restrictionName, prefs.getLong(getUsagePref(uid, restrictionName), 0) });
				return cursor;
			} else if (sUriMatcher.match(uri) == TYPE_AUDIT && selectionArgs.length == 1) {
				// Return audit
				int uid = Integer.parseInt(selectionArgs[0]);
				String prefix = getUsagePref(uid, restrictionName) + ".";
				MatrixCursor cursor = new MatrixCursor(new String[] { COL_UID, COL_RESTRICTION, COL_METHOD, COL_TIME });
				for (String pref : prefs.getAll().keySet())
					if (pref.startsWith(prefix))
						cursor.addRow(new Object[] { uid, restrictionName, pref.substring(prefix.length()),
								prefs.getLong(pref, 0) });
				return cursor;
			}
		}
		throw new IllegalArgumentException();
	}

	@Override
	public Uri insert(Uri uri, ContentValues values) {
		throw new IllegalArgumentException();
	}

	@Override
	public int update(Uri uri, ContentValues values, String restrictionName, String[] selectionArgs) {
		// TODO: register update time?
		if (sUriMatcher.match(uri) == TYPE_RESTRICTION) {
			// Check update restriction
			int cuid = Binder.getCallingUid();
			String[] packages = getContext().getPackageManager().getPackagesForUid(cuid);
			List<String> listPackage = new ArrayList<String>(Arrays.asList(packages));
			String packageName = XPrivacyProvider.class.getPackage().getName();
			if (listPackage.contains(packageName)) {
				// Get argument
				int uid = values.getAsInteger(COL_UID);
				boolean allowed = !Boolean.parseBoolean(values.getAsString(COL_RESTRICTED));

				// Get restrictions
				SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);
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
			} else
				throw new SecurityException();
		}
		throw new IllegalArgumentException();
	}

	@Override
	public int delete(Uri url, String where, String[] selectionArgs) {
		throw new IllegalArgumentException();
	}

	@SuppressWarnings("deprecation")
	@SuppressLint("WorldReadableFiles")
	private void updateStoragePackages(int uid, String restrictionName, boolean allowed) {
		// Get storage packages
		SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY + "." + XRestriction.cStorage,
				Context.MODE_WORLD_READABLE | Context.MODE_MULTI_PROCESS);
		String storagePackages = prefs.getString(getPackagesPref(restrictionName), "");

		// Build package list
		List<String> listStoragePackage = new ArrayList<String>();
		if (!storagePackages.equals(""))
			listStoragePackage.addAll(Arrays.asList(storagePackages.split(",")));

		// Update package list
		for (String storagePackage : getContext().getPackageManager().getPackagesForUid(uid))
			if (!allowed && !listStoragePackage.contains(storagePackage))
				listStoragePackage.add(storagePackage);
			else if (allowed && listStoragePackage.contains(storagePackage))
				listStoragePackage.remove(storagePackage);

		// Store storage packages
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

		// Build package list
		List<String> listStoragePackage = new ArrayList<String>();
		if (!storagePackages.equals(""))
			listStoragePackage.addAll(Arrays.asList(storagePackages.split(",")));

		// Check if restricted
		boolean restricted = listStoragePackage.contains(packageName);
		XUtil.log(hook, Log.INFO, "package=" + packageName + " restricted=" + restricted);
		return restricted;
	}

	private static String getRestrictionPref(String restrictionName) {
		return COL_RESTRICTED + "." + restrictionName;
	}

	private static String getUsagePref(int uid, String restrictionName) {
		return COL_TIME + "." + uid + "." + restrictionName;
	}

	private static String getMethodPref(int uid, String restrictionName, String methodName) {
		return getUsagePref(uid, restrictionName) + "." + methodName;
	}

	private static String getPackagesPref(String restrictionName) {
		return cPackages + "." + restrictionName;
	}
}

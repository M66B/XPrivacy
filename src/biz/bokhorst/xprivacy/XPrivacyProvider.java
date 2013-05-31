package biz.bokhorst.xprivacy;

import java.util.Arrays;
import java.util.Date;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.UriMatcher;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.Binder;

public class XPrivacyProvider extends ContentProvider {

	public static final String AUTHORITY = "biz.bokhorst.xprivacy.provider";
	public static final String PATH_PERMISSIONS = "permissions";
	public static final String PATH_LASTUSE = "lastuse";
	public static final Uri URI_PERMISSIONS = Uri.parse("content://" + AUTHORITY + "/" + PATH_PERMISSIONS);
	public static final Uri URI_LASTUSE = Uri.parse("content://" + AUTHORITY + "/" + PATH_LASTUSE);

	public static final String COL_NAME = "Name";
	public static final String COL_PERMISSION = "Permission";
	public static final String COL_UID = "Uid";
	public static final String COL_LASTUSE = "LastUse";

	private static final UriMatcher sUriMatcher;
	private static final int TYPE_PERMISSIONS = 1;
	private static final int TYPE_LASTUSE = 2;

	static {
		sUriMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sUriMatcher.addURI(AUTHORITY, PATH_PERMISSIONS, TYPE_PERMISSIONS);
		sUriMatcher.addURI(AUTHORITY, PATH_LASTUSE, TYPE_LASTUSE);
	}

	@Override
	public boolean onCreate() {
		return true;
	}

	@Override
	public String getType(Uri uri) {
		if (sUriMatcher.match(uri) == TYPE_PERMISSIONS)
			return String.format("vnd.android.cursor.dir/%s.%s", AUTHORITY, PATH_PERMISSIONS);
		else if (sUriMatcher.match(uri) == TYPE_LASTUSE)
			return String.format("vnd.android.cursor.dir/%s.%s", AUTHORITY, PATH_LASTUSE);
		throw new IllegalArgumentException();
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
		if (selectionArgs != null && selectionArgs.length == 2) {
			// Get arguments
			String permissionName = selection;
			SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);

			if (sUriMatcher.match(uri) == TYPE_PERMISSIONS) {
				int uid = Integer.parseInt(selectionArgs[0]);
				boolean usage = Boolean.parseBoolean(selectionArgs[1]);

				// Update usage count
				if (usage) {
					SharedPreferences.Editor editor = prefs.edit();
					editor.putLong(getUsagePref(uid, permissionName), new Date().getTime());
					editor.commit();
				}

				// Return permission
				MatrixCursor mc = new MatrixCursor(new String[] { COL_NAME, COL_PERMISSION });
				mc.addRow(new Object[] { permissionName, prefs.getString(getPermissionPref(permissionName), "*") });
				return mc;
			} else if (sUriMatcher.match(uri) == TYPE_LASTUSE) {
				// Return usage
				int uid = Integer.parseInt(selectionArgs[0]);
				MatrixCursor mc = new MatrixCursor(new String[] { COL_UID, COL_NAME, COL_LASTUSE });
				mc.addRow(new Object[] { uid, permissionName, prefs.getLong(getUsagePref(uid, permissionName), 0) });
				return mc;
			}
		}
		throw new IllegalArgumentException();
	}

	@Override
	public Uri insert(Uri uri, ContentValues values) {
		throw new IllegalArgumentException();
	}

	@Override
	public int update(Uri uri, ContentValues values, String where, String[] selectionArgs) {
		// TODO: register update time?
		if (sUriMatcher.match(uri) == TYPE_PERMISSIONS) {
			// Check update permission
			int uid = Binder.getCallingUid();
			String[] packages = getContext().getPackageManager().getPackagesForUid(uid);
			if (Arrays.asList(packages).contains("com.android.settings")) {
				// Update permission
				SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);
				SharedPreferences.Editor editor = prefs.edit();
				editor.putString(getPermissionPref(values.getAsString(COL_NAME)), values.getAsString(COL_PERMISSION));
				editor.commit();
			} else
				throw new SecurityException();
			return 1;
		}
		throw new IllegalArgumentException();
	}

	private String getPermissionPref(String permissionName) {
		return COL_PERMISSION + "." + permissionName;
	}

	private String getUsagePref(int uid, String permissionName) {
		return COL_LASTUSE + "." + uid + "." + permissionName;
	}

	@Override
	public int delete(Uri url, String where, String[] selectionArgs) {
		throw new IllegalArgumentException();
	}
}

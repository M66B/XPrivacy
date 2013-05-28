package biz.bokhorst.xprivacy;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.UriMatcher;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.util.Log;

public class XContentProvider extends ContentProvider {

	private static final int CONTENT_TYPE = 1;
	private static final UriMatcher sUriMatcher;

	public static final String AUTHORITY = "biz.bokhorst.xprivacy.provider";
	public static final String CONTENT_PATH = "permissions";
	public static final Uri CONTENT_URI = Uri.parse("content://" + AUTHORITY + "/" + CONTENT_PATH);
	public static final String COL_NAME = "Name";
	public static final String COL_PERMISSION = "Permission";

	static {
		sUriMatcher = new UriMatcher(UriMatcher.NO_MATCH);
		sUriMatcher.addURI(AUTHORITY, CONTENT_PATH, CONTENT_TYPE);
	}

	@Override
	public boolean onCreate() {
		return true;
	}

	@Override
	public String getType(Uri uri) {
		Log.i(AUTHORITY, "getType");
		if (sUriMatcher.match(uri) == CONTENT_TYPE)
			return String.format("vnd.android.cursor.dir/%s.%s", AUTHORITY, CONTENT_PATH);
		throw new IllegalArgumentException();
	}

	@Override
	public Cursor query(Uri uri, String[] arg1, String arg2, String[] arg3, String arg4) {
		Log.i(AUTHORITY, "query");
		if (sUriMatcher.match(uri) == CONTENT_TYPE) {
			MatrixCursor mc = new MatrixCursor(new String[] { COL_NAME, COL_PERMISSION });
			for (String permissionName : XHook.cPermissionNames)
				mc.addRow(new Object[] { permissionName,
						System.getProperty(XHook.cPermissionPrefix + permissionName, "*") });
			return mc;
		}
		throw new IllegalArgumentException();
	}

	@Override
	public Uri insert(Uri uri, ContentValues values) {
		Log.i(AUTHORITY, "insert");
		if (sUriMatcher.match(uri) == CONTENT_TYPE)
			System.setProperty(XHook.cPermissionPrefix + values.getAsString(COL_NAME),
					values.getAsString(COL_PERMISSION));
		throw new IllegalArgumentException();
	}

	@Override
	public int update(Uri uri, ContentValues values, String arg2, String[] arg3) {
		return 0;
	}

	@Override
	public int delete(Uri uri, String arg1, String[] arg2) {
		return 0;
	}
}

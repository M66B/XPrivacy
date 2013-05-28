package biz.bokhorst.xprivacy;

import java.util.Arrays;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.UriMatcher;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.Binder;

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
		if (sUriMatcher.match(uri) == CONTENT_TYPE)
			return String.format("vnd.android.cursor.dir/%s.%s", AUTHORITY, CONTENT_PATH);
		throw new IllegalArgumentException();
	}

	@Override
	public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
		if (sUriMatcher.match(uri) == CONTENT_TYPE) {
			SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);
			MatrixCursor mc = new MatrixCursor(new String[] { COL_NAME, COL_PERMISSION });
			mc.addRow(new Object[] { selection, prefs.getString(selection, "*") });
			return mc;
		}
		throw new IllegalArgumentException();
	}

	@Override
	public Uri insert(Uri uri, ContentValues values) {
		throw new IllegalArgumentException();
	}

	@Override
	public int update(Uri uri, ContentValues values, String where, String[] selectionArgs) {
		if (sUriMatcher.match(uri) == CONTENT_TYPE) {
			int uid = Binder.getCallingUid();
			String[] packages = getContext().getPackageManager().getPackagesForUid(uid);
			if (Arrays.asList(packages).contains("com.android.settings")) {
				SharedPreferences prefs = getContext().getSharedPreferences(AUTHORITY, Context.MODE_PRIVATE);
				SharedPreferences.Editor editor = prefs.edit();
				editor.putString(values.getAsString(COL_NAME), values.getAsString(COL_PERMISSION));
				editor.commit();
			} else
				throw new SecurityException();
			return 1;
		}
		throw new IllegalArgumentException();
	}

	@Override
	public int delete(Uri url, String where, String[] selectionArgs) {
		throw new IllegalArgumentException();
	}
}

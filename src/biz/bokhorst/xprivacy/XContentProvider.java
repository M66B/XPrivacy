package biz.bokhorst.xprivacy;

import java.util.Arrays;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.ContentProvider;
import android.content.Context;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.Binder;
import android.provider.ContactsContract;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContentProvider extends XHook {

	private String mProviderName;
	private String mUriStart;

	public XContentProvider(String restrictionName, String[] permissions, String providerName) {
		super("query", restrictionName, permissions, providerName);
		mProviderName = providerName;
		mUriStart = null;
	}

	public XContentProvider(String restrictionName, String[] permissions, String providerName, String uriStart) {
		super("query", restrictionName, permissions, providerName);
		mProviderName = providerName;
		mUriStart = uriStart;
	}

	// @formatter:off

	// public abstract Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	// public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder, CancellationSignal cancellationSignal)
	// frameworks/base/core/java/android/content/ContentProvider.java
	
	// frameworks/base/core/java/android/provider/Telephony.java ?
	
	// packages/apps/Browser/src/com/android/browser/provider/BrowserProvider2.java
	// packages/providers/CalendarProvider/src/com/android/providers/calendar/CalendarProvider2.java
	// packages/providers/ContactsProvider/src/com/android/providers/contacts/ContactsProvider2.java
	// packages/providers/ContactsProvider/src/com/android/providers/contacts/CallLogProvider.java
	// packages/providers/ContactsProvider/src/com/android/providers/contacts/VoicemailContentProvider.java
	// packages/providers/TelephonyProvider/src/com/android/providers/telephony/SmsProvider.java
	// packages/providers/TelephonyProvider/src/com/android/providers/telephony/MmsProvider.java
	// packages/providers/TelephonyProvider/src/com/android/providers/telephony/MmsSmsProvider.java
	// packages/providers/TelephonyProvider/src/com/android/providers/telephony/TelephonyProvider.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	@SuppressLint("DefaultLocale")
	protected void after(MethodHookParam param) throws Throwable {
		// Check uri
		Uri uri = (Uri) param.args[0];
		Util.log(this, Log.INFO, "uri=" + uri);
		String sUri = (uri == null ? null : uri.toString().toLowerCase());
		if (sUri != null && (mUriStart == null || sUri.startsWith(mUriStart))) {

			Cursor cursor = (Cursor) param.getResult();
			if (cursor != null && isRestricted(param, mProviderName))
				try {

					if (sUri.startsWith("content://com.google.android.gsf.gservices")) {
						// Google services provider: block only android_id
						List<String> selectionArgs = Arrays.asList((String[]) param.args[3]);
						if (Util.containsIgnoreCase(selectionArgs, "android_id")) {
							MatrixCursor gsfCursor = new MatrixCursor(cursor.getColumnNames());
							gsfCursor.addRow(new Object[] { "android_id", PrivacyManager.getDefacedProp("GSF_ID") });
							gsfCursor.respond(cursor.getExtras());
							param.setResult(gsfCursor);
						}

					} else if (sUri.startsWith(ContactsContract.Contacts.CONTENT_URI.toString())) {
						// Contacts provider: allow selected contacts
						MatrixCursor result = new MatrixCursor(cursor.getColumnNames());
						while (cursor.moveToNext()) {
							// Find contact ID column
							int iId = -1;
							if (sUri.startsWith("content://com.android.contacts/contacts"))
								iId = cursor.getColumnIndex("_id");
							else
								iId = cursor.getColumnIndex("contact_id");

							// Copy row
							if (iId >= 0)
								try {
									int id = Integer.parseInt(cursor.getString(iId));
									if (PrivacyManager.getSettingBool(this, null,
											String.format("Contact.%d.%d", Binder.getCallingUid(), id), false, true)) {
										Object[] columns = new Object[cursor.getColumnCount()];
										for (int i = 0; i < cursor.getColumnCount(); i++)
											switch (cursor.getType(i)) {
											case Cursor.FIELD_TYPE_NULL:
												columns[i] = null;
												break;
											case Cursor.FIELD_TYPE_INTEGER:
												columns[i] = cursor.getInt(i);
												break;
											case Cursor.FIELD_TYPE_FLOAT:
												columns[i] = cursor.getFloat(i);
												break;
											case Cursor.FIELD_TYPE_STRING:
												columns[i] = cursor.getString(i);
												break;
											case Cursor.FIELD_TYPE_BLOB:
												columns[i] = cursor.getBlob(i);
												break;
											default:
												Util.log(this, Log.WARN,
														"Unknown cursor data type=" + cursor.getType(i));
											}
										result.addRow(columns);
									}
								} catch (Throwable ex) {
									Util.bug(this, ex);
								}
							else
								Util.log(this, Log.WARN, "Contact ID not found uri=" + sUri);
						}
						result.respond(cursor.getExtras());
						param.setResult(result);

					} else {
						// Return empty cursor
						MatrixCursor result = new MatrixCursor(cursor.getColumnNames());
						result.respond(cursor.getExtras());
						param.setResult(result);
					}

				} finally {
					cursor.close();
				}
		}
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		ContentProvider contentProvider = (ContentProvider) param.thisObject;
		Context context = contentProvider.getContext();
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

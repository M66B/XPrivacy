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
		if (mUriStart == null || uri.toString().startsWith(mUriStart)) {

			Cursor cursor = (Cursor) param.getResult();
			if (cursor != null) {
				if (isRestricted(param, mProviderName)) {
					if (uri.toString().toLowerCase().startsWith("content://com.google.android.gsf.gservices")) {
						// Google services provider: block only android_id
						List<String> selectionArgs = Arrays.asList((String[]) param.args[3]);
						if (Util.containsIgnoreCase(selectionArgs, "android_id")) {
							MatrixCursor gsfCursor = new MatrixCursor(cursor.getColumnNames());
							gsfCursor.addRow(new Object[] { "android_id", PrivacyManager.getDefacedProp("GSF_ID") });
							param.setResult(gsfCursor);
						}
					} else if (uri.toString().toLowerCase()
							.startsWith(ContactsContract.Contacts.CONTENT_URI.toString())) {
						// Contacts provider: allow selected contacts
						ContentProvider contentProvider = (ContentProvider) param.thisObject;
						Context context = contentProvider.getContext();
						MatrixCursor result = new MatrixCursor(cursor.getColumnNames());
						while (cursor.moveToNext()) {
							int iId = cursor.getColumnIndex(ContactsContract.Contacts._ID);
							if (iId >= 0)
								try {
									int id = Integer.parseInt(cursor.getString(iId));
									if (PrivacyManager.getSettingBool(this, context,
											String.format("Contact.%s.%d", context.getPackageName(), id), false, true)) {
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
						}
						param.setResult(result);
					} else
						// Return empty cursor
						param.setResult(new MatrixCursor(cursor.getColumnNames()));

					cursor.close();
				}
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

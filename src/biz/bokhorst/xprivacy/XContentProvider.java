package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.annotation.SuppressLint;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContentProvider extends XHook {

	private String mClassName;
	private String mUriStart;

	private XContentProvider(String restrictionName, String providerName, String className) {
		super(restrictionName, "query", providerName);
		mClassName = className;
		mUriStart = null;
	}

	private XContentProvider(String restrictionName, String providerName, String className, String uriStart) {
		super(restrictionName, "query", uriStart.replace("content://com.android.", ""));
		mClassName = className;
		mUriStart = uriStart;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public abstract Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	// public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder, CancellationSignal cancellationSignal)
	// frameworks/base/core/java/android/content/ContentProvider.java
	// http://developer.android.com/reference/android/content/ContentProvider.html
	// https://developers.google.com/gmail/android/

	// packages/apps/Browser/src/com/android/browser/provider/BrowserProvider2.java
	// packages/providers/CalendarProvider/src/com/android/providers/calendar/CalendarProvider2.java
	// packages/providers/ContactsProvider/src/com/android/providers/contacts/ContactsProvider2.java
	// packages/providers/ContactsProvider/src/com/android/providers/contacts/CallLogProvider.java
	// packages/providers/ContactsProvider/src/com/android/providers/contacts/VoicemailContentProvider.java
	// packages/providers/TelephonyProvider/src/com/android/providers/telephony/SmsProvider.java
	// packages/providers/TelephonyProvider/src/com/android/providers/telephony/MmsProvider.java
	// packages/providers/TelephonyProvider/src/com/android/providers/telephony/MmsSmsProvider.java
	// packages/providers/TelephonyProvider/src/com/android/providers/telephony/TelephonyProvider.java

	// frameworks/base/core/java/android/provider/ContactsContract.java

	// @formatter:on

	public static List<XHook> getInstances(String packageName) {
		List<XHook> listHook = new ArrayList<XHook>();

		// Applications provider
		if (packageName.equals("com.android.providers.applications"))
			listHook.add(new XContentProvider(PrivacyManager.cSystem, "ApplicationsProvider",
					"com.android.providers.applications.ApplicationsProvider"));

		// Browser provider
		else if (packageName.equals("com.android.browser")) {
			listHook.add(new XContentProvider(PrivacyManager.cBrowser, "BrowserProvider",
					"com.android.browser.provider.BrowserProvider").optional());
			listHook.add(new XContentProvider(PrivacyManager.cBrowser, "BrowserProvider2",
					"com.android.browser.provider.BrowserProvider2").optional());
		}

		// Calendar provider
		else if (packageName.equals("com.android.providers.calendar"))
			listHook.add(new XContentProvider(PrivacyManager.cCalendar, "CalendarProvider2",
					"com.android.providers.calendar.CalendarProvider2"));

		// Contacts provider
		else if (packageName.equals("com.android.providers.contacts")) {
			String[] uris = new String[] { "contacts/contacts", "contacts/data", "contacts/raw_contacts",
					"contacts/phone_lookup", "contacts/profile" };
			for (String uri : uris)
				listHook.add(new XContentProvider(PrivacyManager.cContacts, "ContactsProvider2",
						"com.android.providers.contacts.ContactsProvider2", "content://com.android." + uri).optional());

			listHook.add(new XContentProvider(PrivacyManager.cPhone, "CallLogProvider",
					"com.android.providers.contacts.CallLogProvider").optional());
			listHook.add(new XContentProvider(PrivacyManager.cMessages, "VoicemailContentProvider",
					"com.android.providers.contacts.VoicemailContentProvider").optional());
		}

		// Contacts provider of Motorola's Blur
		else if (packageName.equals("com.motorola.blur.providers.contacts")) {
			String[] uris = new String[] { "contacts/contacts", "contacts/data", "contacts/raw_contacts",
					"contacts/phone_lookup", "contacts/profile" };
			for (String uri : uris)
				listHook.add(new XContentProvider(PrivacyManager.cContacts, "ContactsProvider2",
						"com.android.providers.contacts.ContactsProvider2", "content://com.android." + uri).optional());

			listHook.add(new XContentProvider(PrivacyManager.cPhone, "BlurCallLogProvider",
					"com.motorola.blur.providers.contacts.BlurCallLogProvider").optional());
		}

		// E-mail provider
		else if (packageName.equals("com.android.email"))
			listHook.add(new XContentProvider(PrivacyManager.cEMail, "EMailProvider",
					"com.android.email.provider.EmailProvider"));

		// Gmail provider
		else if (packageName.equals("com.google.android.gm"))
			listHook.add(new XContentProvider(PrivacyManager.cEMail, "GMailProvider",
					"com.google.android.gm.provider.PublicContentProvider").optional());

		// Google services provider
		else if (packageName.equals("com.google.android.gsf"))
			listHook.add(new XContentProvider(PrivacyManager.cIdentification, "GservicesProvider",
					"com.google.android.gsf.gservices.GservicesProvider"));

		// Telephony providers
		else if (packageName.equals("com.android.providers.telephony")) {
			listHook.add(new XContentProvider(PrivacyManager.cContacts, "IccProvider",
					"com.android.internal.telephony.IccProvider"));

			listHook.add(new XContentProvider(PrivacyManager.cMessages, "SmsProvider",
					"com.android.providers.telephony.SmsProvider"));
			listHook.add(new XContentProvider(PrivacyManager.cMessages, "MmsProvider",
					"com.android.providers.telephony.MmsProvider"));
			listHook.add(new XContentProvider(PrivacyManager.cMessages, "MmsSmsProvider",
					"com.android.providers.telephony.MmsSmsProvider"));
			listHook.add(new XContentProvider(PrivacyManager.cPhone, "TelephonyProvider",
					"com.android.providers.telephony.TelephonyProvider"));
		}

		// User dictionary
		else if (packageName.equals("com.android.providers.userdictionary"))
			listHook.add(new XContentProvider(PrivacyManager.cDictionary, "UserDictionary",
					"com.android.providers.userdictionary.UserDictionaryProvider"));

		return listHook;
	}

	@Override
	@SuppressLint("DefaultLocale")
	protected void before(MethodHookParam param) throws Throwable {
		// Check URI
		if (param.args.length > 1 && param.args[0] instanceof Uri && param.args[1] instanceof String[]) {
			String uri = ((Uri) param.args[0]).toString().toLowerCase();
			String[] projection = (String[]) param.args[1];
			if (mUriStart == null || uri.startsWith(mUriStart))
				if (uri.startsWith("content://com.android.contacts")
						&& !uri.startsWith("content://com.android.contacts/profile"))
					if (isRestrictedExtra(param, uri))
						if (projection != null) {
							String id = null;
							if (uri.startsWith("content://com.android.contacts/contacts"))
								id = "name_raw_contact_id";
							else if (uri.startsWith("content://com.android.contacts/data"))
								id = "raw_contact_id";
							else if (uri.startsWith("content://com.android.contacts/phone_lookup"))
								id = "name_raw_contact_id";
							else if (uri.startsWith("content://com.android.contacts/raw_contacts"))
								id = "_id";
							else
								Util.log(this, Log.ERROR, "Unexpected uri=" + uri);

							if (id != null) {
								List<String> listProjection = new ArrayList<String>();
								listProjection.addAll(Arrays.asList(projection));
								listProjection.add(id);
							}
						}
		}
	}

	@Override
	@SuppressLint("DefaultLocale")
	protected void after(MethodHookParam param) throws Throwable {
		// Check URI
		if (param.args.length > 1 && param.args[0] instanceof Uri && param.args[1] instanceof String[]) {
			String uri = ((Uri) param.args[0]).toString().toLowerCase();
			String[] projection = (String[]) param.args[1];
			if (mUriStart == null || uri.startsWith(mUriStart)) {
				Cursor cursor = (Cursor) param.getResult();
				if (cursor != null)
					if (uri.startsWith("content://com.google.android.gsf.gservices")) {
						// Google services provider: block only android_id
						if (param.args.length > 3 && param.args[3] != null) {
							List<String> selectionArgs = Arrays.asList((String[]) param.args[3]);
							if (Util.containsIgnoreCase(selectionArgs, "android_id"))
								if (isRestrictedExtra(param, uri)) {
									MatrixCursor gsfCursor = new MatrixCursor(cursor.getColumnNames());
									gsfCursor.addRow(new Object[] { "android_id",
											PrivacyManager.getDefacedProp(Binder.getCallingUid(), "GSF_ID") });
									gsfCursor.respond(cursor.getExtras());
									param.setResult(gsfCursor);
									cursor.close();
								}
						}

					} else if (uri.startsWith("content://com.android.contacts")
							&& !uri.startsWith("content://com.android.contacts/profile")) {
						// Contacts provider: allow selected contacts
						if (isRestrictedExtra(param, uri)) {
							// Get raw contact ID index
							int irawid = -1;
							if (uri.startsWith("content://com.android.contacts/contacts"))
								irawid = cursor.getColumnCount() - 1;
							else if (uri.startsWith("content://com.android.contacts/data"))
								irawid = cursor.getColumnCount() - 1;
							else if (uri.startsWith("content://com.android.contacts/phone_lookup"))
								irawid = cursor.getColumnCount() - 1;
							else if (uri.startsWith("content://com.android.contacts/raw_contacts"))
								irawid = cursor.getColumnCount() - 1;
							else
								Util.log(this, Log.ERROR, "Unexpected uri=" + uri);

							List<String> listColumn = new ArrayList<String>();
							listColumn.addAll(Arrays.asList(cursor.getColumnNames()));
							if (projection != null)
								listColumn.remove(listColumn.size() - 1);

							MatrixCursor result = new MatrixCursor(listColumn.toArray(new String[0]));
							while (cursor.moveToNext()) {
								// Get contact ID
								long rawid = (irawid < 0 ? -1 : cursor.getLong(irawid));

								// Check if can be copied
								boolean copy = false;
								if (rawid >= 0)
									copy = PrivacyManager.getSettingBool(Binder.getCallingUid(),
											PrivacyManager.cSettingRawContact + rawid, false, true);

								// Conditionally copy row
								if (copy)
									copyColumns(cursor, result, listColumn.size());
							}
							if (result.getCount() == 0)
								Util.log(this, Log.WARN, "Empty uri=" + uri);
							result.respond(cursor.getExtras());
							param.setResult(result);
							cursor.close();
						}

					} else if (uri.startsWith("content://applications")) {
						// Applications provider: allow selected applications
						if (isRestrictedExtra(param, uri)) {
							MatrixCursor result = new MatrixCursor(cursor.getColumnNames());
							while (cursor.moveToNext()) {
								int colPackage = cursor.getColumnIndex("package");
								String packageName = (colPackage < 0 ? null : cursor.getString(colPackage));
								if (packageName != null && XPackageManager.isPackageAllowed(packageName))
									copyColumns(cursor, result);
							}
							result.respond(cursor.getExtras());
							param.setResult(result);
							cursor.close();
						}

					} else {
						if (isRestrictedExtra(param, uri)) {
							// Return empty cursor
							MatrixCursor result = new MatrixCursor(cursor.getColumnNames());
							result.respond(cursor.getExtras());
							param.setResult(result);
							cursor.close();
						}
					}
			}
		}
	}

	private void copyColumns(Cursor cursor, MatrixCursor result) {
		copyColumns(cursor, result, cursor.getColumnCount());
	}

	private void copyColumns(Cursor cursor, MatrixCursor result, int count) {
		try {
			Object[] columns = new Object[count];
			for (int i = 0; i < count; i++)
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
					Util.log(this, Log.WARN, "Unknown cursor data type=" + cursor.getType(i));
				}
			result.addRow(columns);
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
	}
}

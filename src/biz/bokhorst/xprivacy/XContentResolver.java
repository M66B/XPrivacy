package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.SyncAdapterType;
import android.content.SyncInfo;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.Binder;
import android.text.TextUtils;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContentResolver extends XHook {
	private Methods mMethod;

	private XContentResolver(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	private XContentResolver(Methods method, String restrictionName, int sdk) {
		super(restrictionName, "query", null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return (mMethod == Methods.cquery ? "android.content.ContentProviderClient" : "android.content.ContentResolver");
	}

	// @formatter:off

	// public static SyncInfo getCurrentSync()
	// static List<SyncInfo> getCurrentSyncs()
	// static SyncAdapterType[] getSyncAdapterTypes()

	// public Cursor query(Uri url, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	// public Cursor query(Uri url, String[] projection, String selection, String[] selectionArgs, String sortOrder, CancellationSignal cancellationSignal)

	// https://developers.google.com/gmail/android/
	// http://developer.android.com/reference/android/content/ContentResolver.html
	// http://developer.android.com/reference/android/content/ContentProviderClient.html

	// http://developer.android.com/reference/android/provider/ContactsContract.Contacts.html
	// http://developer.android.com/reference/android/provider/ContactsContract.Data.html
	// http://developer.android.com/reference/android/provider/ContactsContract.PhoneLookup.html
	// http://developer.android.com/reference/android/provider/ContactsContract.Profile.html

	// @formatter:on

	private enum Methods {
		getCurrentSync, getCurrentSyncs, getSyncAdapterTypes, query, cquery
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XContentResolver(Methods.getCurrentSync, PrivacyManager.cAccounts));
		listHook.add(new XContentResolver(Methods.getCurrentSyncs, PrivacyManager.cAccounts));
		listHook.add(new XContentResolver(Methods.getSyncAdapterTypes, PrivacyManager.cAccounts));
		listHook.add(new XContentResolver(Methods.query, null, 1));
		listHook.add(new XContentResolver(Methods.cquery, null, 1));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.query || mMethod == Methods.cquery)
			handleUriBefore(param);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getCurrentSync) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.getCurrentSyncs) {
			if (isRestricted(param))
				param.setResult(new ArrayList<SyncInfo>());

		} else if (mMethod == Methods.getSyncAdapterTypes) {
			if (isRestricted(param))
				param.setResult(new SyncAdapterType[0]);

		} else if (mMethod == Methods.query || mMethod == Methods.cquery) {
			handleUriAfter(param);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@SuppressLint("DefaultLocale")
	private void handleUriBefore(MethodHookParam param) throws Throwable {
		// Check URI
		if (param.args.length > 1 && param.args[0] instanceof Uri) {
			String uri = ((Uri) param.args[0]).toString().toLowerCase();
			String[] projection = (param.args[1] instanceof String[] ? (String[]) param.args[1] : null);
			if (uri.startsWith("content://com.android.contacts/contacts")
					|| uri.startsWith("content://com.android.contacts/data")
					|| uri.startsWith("content://com.android.contacts/phone_lookup")) {
				String[] components = uri.replace("content://com.android.", "").split("/");
				if (components.length >= 2) {
					String methodName = components[0] + "/" + components[1].split("\\?")[0];
					if (isRestrictedExtra(param, PrivacyManager.cContacts, methodName, uri)) {
						// Modify projection
						boolean added = false;
						if (projection != null) {
							List<String> listProjection = new ArrayList<String>();
							listProjection.addAll(Arrays.asList(projection));
							ContactID id = getIdForUri(uri);
							if (id != null && !listProjection.contains(id.name)) {
								added = true;
								listProjection.add(id.name);
							}
							param.args[1] = listProjection.toArray(new String[0]);
						}
						param.setObjectExtra("column_added", added);
					}
				}
			}
		}
	}

	@SuppressLint("DefaultLocale")
	private void handleUriAfter(MethodHookParam param) throws Throwable {
		// Check URI
		if (param.args.length > 1 && param.args[0] instanceof Uri) {
			String uri = ((Uri) param.args[0]).toString().toLowerCase();
			Cursor cursor = (Cursor) param.getResult();
			if (cursor != null)
				if (uri.startsWith("content://applications")) {
					// Applications provider: allow selected applications
					if (isRestrictedExtra(param, PrivacyManager.cSystem, "ApplicationsProvider", uri)) {
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

				} else if (uri.startsWith("content://com.google.android.gsf.gservices")) {
					// Google services provider: block only android_id
					if (param.args.length > 3 && param.args[3] != null) {
						List<String> selectionArgs = Arrays.asList((String[]) param.args[3]);
						if (Util.containsIgnoreCase(selectionArgs, "android_id"))
							if (isRestrictedExtra(param, PrivacyManager.cIdentification, "GservicesProvider", uri)) {
								MatrixCursor gsfCursor = new MatrixCursor(cursor.getColumnNames());
								gsfCursor.addRow(new Object[] { "android_id",
										PrivacyManager.getDefacedProp(Binder.getCallingUid(), "GSF_ID") });
								gsfCursor.respond(cursor.getExtras());
								param.setResult(gsfCursor);
								cursor.close();
							}
					}

				} else if (uri.startsWith("content://com.android.contacts/contacts")
						|| uri.startsWith("content://com.android.contacts/data")
						|| uri.startsWith("content://com.android.contacts/phone_lookup")) {
					// Contacts provider: allow selected contacts
					String methodName = "contacts/"
							+ uri.replace("content://com.android.contacts/", "").split("/")[0].split("\\?")[0];
					if (isRestrictedExtra(param, PrivacyManager.cContacts, methodName, uri)) {
						// Modify column names back
						boolean added = (Boolean) param.getObjectExtra("column_added");
						List<String> listColumn = new ArrayList<String>();
						listColumn.addAll(Arrays.asList(cursor.getColumnNames()));
						if (added)
							listColumn.remove(listColumn.size() - 1);

						MatrixCursor result = new MatrixCursor(listColumn.toArray(new String[0]));

						// Filter rows
						ContactID cid = getIdForUri(uri);
						int iid = (cid == null ? -1 : cursor.getColumnIndex(cid.name));
						if (iid >= 0) {
							while (cursor.moveToNext()) {
								// Check if allowed
								long id = cursor.getLong(iid);
								String settingName = (cid.raw ? PrivacyManager.cSettingRawContact
										: PrivacyManager.cSettingContact);
								boolean allowed = PrivacyManager.getSettingBool(Binder.getCallingUid(), settingName
										+ id, false, true);
								if (allowed)
									copyColumns(cursor, result, listColumn.size());
							}
						} else
							Util.log(this, Log.ERROR, "ID missing uri=" + uri);

						result.respond(cursor.getExtras());
						param.setResult(result);
						cursor.close();
					}

				} else {
					// Other uri restrictions
					String restrictionName = null;
					String methodName = null;
					if (uri.startsWith("content://browser")) {
						restrictionName = PrivacyManager.cBrowser;
						methodName = "BrowserProvider2";
					}

					else if (uri.startsWith("content://com.android.calendar")) {
						restrictionName = PrivacyManager.cCalendar;
						methodName = "CalendarProvider2";
					}

					else if (uri.startsWith("content://call_log")) {
						restrictionName = PrivacyManager.cPhone;
						methodName = "CallLogProvider";
					}

					else if (uri.startsWith("content://com.android.contacts/profile")) {
						restrictionName = PrivacyManager.cContacts;
						methodName = "contacts/profile";
					}

					else if (uri.startsWith("content://com.android.contacts")) {
						restrictionName = PrivacyManager.cContacts;
						methodName = "ContactsProvider2"; // fall-back
					}

					else if (uri.startsWith("content://downloads")) {
						restrictionName = PrivacyManager.cBrowser;
						methodName = "Downloads";
					}

					else if (uri.startsWith("content://com.android.email.provider")) {
						restrictionName = PrivacyManager.cEMail;
						methodName = "EMailProvider";
					}

					else if (uri.startsWith("content://com.google.android.gm")) {
						restrictionName = PrivacyManager.cEMail;
						methodName = "GMailProvider";
					}

					else if (uri.startsWith("content://icc")) {
						restrictionName = PrivacyManager.cContacts;
						methodName = "IccProvider";
					}

					else if (uri.startsWith("content://mms")) {
						restrictionName = PrivacyManager.cMessages;
						methodName = "MmsProvider";
					}

					else if (uri.startsWith("content://mms-sms")) {
						restrictionName = PrivacyManager.cMessages;
						methodName = "MmsSmsProvider";
					}

					else if (uri.startsWith("content://sms")) {
						restrictionName = PrivacyManager.cMessages;
						methodName = "SmsProvider";
					}

					else if (uri.startsWith("content://telephony")) {
						restrictionName = PrivacyManager.cPhone;
						methodName = "TelephonyProvider";
					}

					else if (uri.startsWith("content://user_dictionary")) {
						restrictionName = PrivacyManager.cDictionary;
						methodName = "UserDictionary";
					}

					else if (uri.startsWith("content://com.android.voicemail")) {
						restrictionName = PrivacyManager.cMessages;
						methodName = "VoicemailContentProvider";
					}

					// Check if know / restricted
					if (methodName != null && isRestrictedExtra(param, restrictionName, methodName, uri)) {
						// Return empty cursor
						MatrixCursor result = new MatrixCursor(cursor.getColumnNames());
						result.respond(cursor.getExtras());
						param.setResult(result);
						cursor.close();
					}
				}
		}
	}

	private static class ContactID {
		public String name;
		public boolean raw;

		public ContactID(String _name, boolean _raw) {
			name = _name;
			raw = _raw;
		}
	}

	private ContactID getIdForUri(String uri) {
		if (uri.startsWith("content://com.android.contacts/contacts"))
			return new ContactID("name_raw_contact_id", true);
		else if (uri.startsWith("content://com.android.contacts/data"))
			return new ContactID("raw_contact_id", true);
		else if (uri.startsWith("content://com.android.contacts/phone_lookup"))
			return new ContactID("_id", false);
		else
			Util.log(this, Log.ERROR, "Unexpected uri=" + uri);
		return null;
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

	@SuppressWarnings("unused")
	private void _dumpCursor(String uri, Cursor cursor) {
		Util.log(this, Log.WARN, TextUtils.join(", ", cursor.getColumnNames()) + " uri=" + uri);
		int i = 0;
		while (cursor.moveToNext() && i++ < 10)
			_dumpColumns(cursor, "");
		cursor.moveToFirst();
	}

	private void _dumpColumns(Cursor cursor, String msg) {
		String[] columns = new String[cursor.getColumnCount()];
		for (int i = 0; i < cursor.getColumnCount(); i++)
			switch (cursor.getType(i)) {
			case Cursor.FIELD_TYPE_NULL:
				columns[i] = null;
				break;
			case Cursor.FIELD_TYPE_INTEGER:
				columns[i] = Integer.toString(cursor.getInt(i));
				break;
			case Cursor.FIELD_TYPE_FLOAT:
				columns[i] = Float.toString(cursor.getFloat(i));
				break;
			case Cursor.FIELD_TYPE_STRING:
				columns[i] = cursor.getString(i);
				break;
			case Cursor.FIELD_TYPE_BLOB:
				columns[i] = "[blob]";
				break;
			default:
				Util.log(this, Log.WARN, "Unknown cursor data type=" + cursor.getType(i));
			}
		Util.log(this, Log.WARN, TextUtils.join(", ", columns) + " " + msg);
	}
}

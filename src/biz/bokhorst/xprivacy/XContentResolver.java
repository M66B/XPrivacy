package biz.bokhorst.xprivacy;

import java.io.FileNotFoundException;
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
import android.os.DeadObjectException;
import android.text.TextUtils;
import android.util.Log;

public class XContentResolver extends XHook {
	private Methods mMethod;
	private boolean mClient;
	private String mClassName = null;

	private XContentResolver(Methods method, String restrictionName, boolean client) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClient = client;
	}

	private XContentResolver(Methods method, String restrictionName, int sdk, boolean client) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
		mClient = client;
	}

	private XContentResolver(Methods method, String restrictionName, int sdk, String className) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		if (mClassName == null)
			return (mClient ? "android.content.ContentProviderClient" : "android.content.ContentResolver");
		else
			return mClassName;
	}

	// @formatter:off

	// public static SyncInfo getCurrentSync()
	// static List<SyncInfo> getCurrentSyncs()
	// static SyncAdapterType[] getSyncAdapterTypes()

	// final AssetFileDescriptor openAssetFileDescriptor(Uri uri, String mode)
	// final AssetFileDescriptor openAssetFileDescriptor(Uri uri, String mode, CancellationSignal cancellationSignal)
	// final ParcelFileDescriptor openFileDescriptor(Uri uri, String mode, CancellationSignal cancellationSignal)
	// final ParcelFileDescriptor openFileDescriptor(Uri uri, String mode)
	// final InputStream openInputStream(Uri uri)
	// final OutputStream openOutputStream(Uri uri)
	// final OutputStream openOutputStream(Uri uri, String mode)
	// final AssetFileDescriptor openTypedAssetFileDescriptor(Uri uri, String mimeType, Bundle opts, CancellationSignal cancellationSignal)
	// final AssetFileDescriptor openTypedAssetFileDescriptor(Uri uri, String mimeType, Bundle opts)

	// AssetFileDescriptor openAssetFile(Uri url, String mode, CancellationSignal signal)
	// AssetFileDescriptor openAssetFile(Uri url, String mode)
	// ParcelFileDescriptor openFile(Uri url, String mode)
	// ParcelFileDescriptor openFile(Uri url, String mode, CancellationSignal signal)

	// public Cursor query(Uri url, String[] projection, String selection, String[] selectionArgs, String sortOrder)
	// public Cursor query(Uri url, String[] projection, String selection, String[] selectionArgs, String sortOrder, CancellationSignal cancellationSignal)

	// https://developers.google.com/gmail/android/
	// http://developer.android.com/reference/android/content/ContentResolver.html
	// http://developer.android.com/reference/android/content/ContentProviderClient.html

	// http://developer.android.com/reference/android/provider/Contacts.People.html
	// http://developer.android.com/reference/android/provider/ContactsContract.Contacts.html
	// http://developer.android.com/reference/android/provider/ContactsContract.Data.html
	// http://developer.android.com/reference/android/provider/ContactsContract.PhoneLookup.html
	// http://developer.android.com/reference/android/provider/ContactsContract.Profile.html
	// http://developer.android.com/reference/android/provider/ContactsContract.RawContacts.html

	// frameworks/base/core/java/android/content/ContentResolver.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		getCurrentSync, getCurrentSyncs, getSyncAdapterTypes,
		openAssetFile, openFile, openAssetFileDescriptor, openFileDescriptor, openInputStream, openOutputStream, openTypedAssetFileDescriptor,
		query
	};
	// @formatter:on

	// @formatter:off
	public static List<String> cProviderClassName = Arrays.asList(new String[] {
		"com.android.providers.applications.ApplicationsProvider",
		"com.android.browser.provider.BrowserProvider2",
		"com.android.browser.provider.BrowserProviderProxy",
		"com.android.providers.downloads.DownloadProvider",
		"com.android.providers.calendar.CalendarProvider2",
		"com.android.providers.contacts.CallLogProvider",
		"com.android.providers.contacts.ContactsProvider2",
		"com.android.email.provider.EmailProvider",
		"com.google.android.gm.provider.MailProvider",
		"com.google.android.gsf.gservices.GservicesProvider",
		"com.android.providers.telephony.MmsProvider",
		"com.android.providers.telephony.MmsSmsProvider",
		"com.android.providers.telephony.SmsProvider",
		"com.android.providers.telephony.TelephonyProvider",
		"com.android.providers.userdictionary.UserDictionaryProvider",
		"com.android.providers.contacts.VoicemailContentProvider",
	});
	// @formatter:on

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();

		if (className == null) {
			listHook.add(new XContentResolver(Methods.getCurrentSync, PrivacyManager.cAccounts, false));
			listHook.add(new XContentResolver(Methods.getCurrentSyncs, PrivacyManager.cAccounts, false));
			listHook.add(new XContentResolver(Methods.getSyncAdapterTypes, PrivacyManager.cAccounts, false));

			listHook.add(new XContentResolver(Methods.openAssetFileDescriptor, PrivacyManager.cStorage, false));
			listHook.add(new XContentResolver(Methods.openFileDescriptor, PrivacyManager.cStorage, false));
			listHook.add(new XContentResolver(Methods.openInputStream, PrivacyManager.cStorage, false));
			listHook.add(new XContentResolver(Methods.openOutputStream, PrivacyManager.cStorage, false));
			listHook.add(new XContentResolver(Methods.openTypedAssetFileDescriptor, PrivacyManager.cStorage, false));

			listHook.add(new XContentResolver(Methods.openAssetFile, PrivacyManager.cStorage, true));
			listHook.add(new XContentResolver(Methods.openFile, PrivacyManager.cStorage, true));
			listHook.add(new XContentResolver(Methods.openTypedAssetFileDescriptor, PrivacyManager.cStorage, true));

			if (PrivacyManager.isAOSP(19))
				listHook.add(new XContentResolver(Methods.query, null, 1, "com.android.internal.telephony.IccProvider"));
			else {
				listHook.add(new XContentResolver(Methods.query, null, 1, false));
				listHook.add(new XContentResolver(Methods.query, null, 1, true));
			}
		} else {
			XHook hook = new XContentResolver(Methods.query, null, 1, className);
			if (className.startsWith("com.android.browser.provider."))
				hook = hook.optional();
			listHook.add(hook);
		}

		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.query)
			try {
				handleUriBefore(param);
			} catch (DeadObjectException ignored) {
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getCurrentSync) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.getCurrentSyncs) {
			if (isRestricted(param))
				param.setResult(new ArrayList<SyncInfo>());

		} else if (mMethod == Methods.getSyncAdapterTypes) {
			if (isRestricted(param))
				param.setResult(new SyncAdapterType[0]);

		} else if (mMethod == Methods.openAssetFileDescriptor || mMethod == Methods.openFileDescriptor
				|| mMethod == Methods.openInputStream || mMethod == Methods.openOutputStream
				|| mMethod == Methods.openTypedAssetFileDescriptor || mMethod == Methods.openAssetFile
				|| mMethod == Methods.openFile) {
			if (param.args.length > 0 && param.args[0] instanceof Uri) {
				String uri = ((Uri) param.args[0]).toString();
				if (isRestrictedExtra(param, uri))
					param.setThrowable(new FileNotFoundException("XPrivacy"));
			}

		} else if (mMethod == Methods.query) {
			try {
				handleUriAfter(param);
			} catch (DeadObjectException ignored) {
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@SuppressLint("DefaultLocale")
	private void handleUriBefore(XParam param) throws Throwable {
		// Check URI
		if (param.args.length > 1 && param.args[0] instanceof Uri) {
			String uri = ((Uri) param.args[0]).toString().toLowerCase();
			String[] projection = (param.args[1] instanceof String[] ? (String[]) param.args[1] : null);

			if (uri.startsWith("content://com.android.contacts/contacts/name_phone_or_email")) {
				// Do nothing

			} else if (uri.startsWith("content://com.android.contacts/")
					&& !uri.equals("content://com.android.contacts/")) {
				String[] components = uri.replace("content://com.android.", "").split("/");
				String methodName = components[0] + "/" + components[1].split("\\?")[0];
				if (methodName.equals("contacts/contacts") || methodName.equals("contacts/data")
						|| methodName.equals("contacts/phone_lookup") || methodName.equals("contacts/raw_contacts"))
					if (isRestrictedExtra(param, PrivacyManager.cContacts, methodName, uri)) {
						// Get ID from URL if any
						int urlid = -1;
						if ((methodName.equals("contacts/contacts") || methodName.equals("contacts/phone_lookup"))
								&& components.length > 2 && TextUtils.isDigitsOnly(components[2]))
							urlid = Integer.parseInt(components[2]);

						// Modify projection
						boolean added = false;
						if (projection != null && urlid < 0) {
							List<String> listProjection = new ArrayList<String>();
							listProjection.addAll(Arrays.asList(projection));
							String cid = getIdForUri(uri);
							if (cid != null && !listProjection.contains(cid)) {
								added = true;
								listProjection.add(cid);
							}
							param.args[1] = listProjection.toArray(new String[0]);
						}
						if (added)
							param.setObjectExtra("column_added", added);
					}
			}
		}
	}

	@SuppressLint("DefaultLocale")
	private void handleUriAfter(XParam param) throws Throwable {
		// Check URI
		if (param.args.length > 1 && param.args[0] instanceof Uri && param.getResult() != null) {
			String uri = ((Uri) param.args[0]).toString().toLowerCase();
			String[] projection = (param.args[1] instanceof String[] ? (String[]) param.args[1] : null);
			String selection = (param.args[2] instanceof String ? (String) param.args[2] : null);
			Cursor cursor = (Cursor) param.getResult();

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
					List<String> listSelection = Arrays.asList((String[]) param.args[3]);
					if (listSelection.contains("android_id"))
						if (isRestrictedExtra(param, PrivacyManager.cIdentification, "GservicesProvider", uri)) {
							int ikey = cursor.getColumnIndex("key");
							int ivalue = cursor.getColumnIndex("value");
							if (ikey == 0 && ivalue == 1 && cursor.getColumnCount() == 2) {
								MatrixCursor result = new MatrixCursor(cursor.getColumnNames());
								while (cursor.moveToNext()) {
									if ("android_id".equals(cursor.getString(ikey)))
										result.addRow(new Object[] { "android_id",
												PrivacyManager.getDefacedProp(Binder.getCallingUid(), "GSF_ID") });
									else
										copyColumns(cursor, result);
								}
								result.respond(cursor.getExtras());
								param.setResult(result);
								cursor.close();
							} else
								Util.log(this, Log.ERROR,
										"Unexpected result uri=" + uri + " columns=" + cursor.getColumnNames());
						}
				}

			} else if (uri.startsWith("content://com.android.contacts/contacts/name_phone_or_email")) {

				// Do nothing

			} else if (uri.startsWith("content://com.android.contacts/")
					&& !uri.equals("content://com.android.contacts/")) {
				// Contacts provider: allow selected contacts
				String[] components = uri.replace("content://com.android.", "").split("/");
				String methodName = components[0] + "/" + components[1].split("\\?")[0];
				if (methodName.equals("contacts/contacts") || methodName.equals("contacts/data")
						|| methodName.equals("contacts/phone_lookup") || methodName.equals("contacts/raw_contacts")) {
					if (isRestrictedExtra(param, PrivacyManager.cContacts, methodName, uri)) {
						// Get ID from URL if any
						int urlid = -1;
						if ((methodName.equals("contacts/contacts") || methodName.equals("contacts/phone_lookup"))
								&& components.length > 2 && TextUtils.isDigitsOnly(components[2]))
							urlid = Integer.parseInt(components[2]);

						// Modify column names back
						Object column_added = param.getObjectExtra("column_added");
						boolean added = (column_added == null ? false : (Boolean) param.getObjectExtra("column_added"));

						List<String> listColumn = new ArrayList<String>();
						listColumn.addAll(Arrays.asList(cursor.getColumnNames()));
						if (added)
							listColumn.remove(listColumn.size() - 1);

						// Get blacklist setting
						int uid = Binder.getCallingUid();
						boolean blacklist = PrivacyManager
								.getSettingBool(-uid, PrivacyManager.cSettingBlacklist, false);

						MatrixCursor result = new MatrixCursor(listColumn.toArray(new String[0]));

						// Filter rows
						String cid = getIdForUri(uri);
						int iid = (cid == null ? -1 : cursor.getColumnIndex(cid));
						if (iid >= 0 || urlid >= 0)
							while (cursor.moveToNext()) {
								// Check if allowed
								long id = (urlid >= 0 ? urlid : cursor.getLong(iid));
								boolean allowed = PrivacyManager.getSettingBool(-uid, Meta.cTypeContact,
										Long.toString(id), false);
								if (blacklist)
									allowed = !allowed;
								if (allowed)
									copyColumns(cursor, result, listColumn.size());
							}
						else
							Util.log(this, Log.WARN, "ID missing URI=" + uri + " added=" + added + "/" + cid
									+ " columns=" + TextUtils.join(",", cursor.getColumnNames()) + " projection="
									+ (projection == null ? "null" : TextUtils.join(",", projection)) + " selection="
									+ selection);

						result.respond(cursor.getExtras());
						param.setResult(result);
						cursor.close();
					}
				} else {
					methodName = null;
					if (uri.startsWith("content://com.android.contacts/profile"))
						methodName = "contacts/profile";
					else
						methodName = "ContactsProvider2"; // fall-back

					if (methodName != null)
						if (isRestrictedExtra(param, PrivacyManager.cContacts, methodName, uri)) {
							// Return empty cursor
							MatrixCursor result = new MatrixCursor(cursor.getColumnNames());
							result.respond(cursor.getExtras());
							param.setResult(result);
							cursor.close();
						}
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
					restrictionName = PrivacyManager.cCalling;
					methodName = "CallLogProvider";
				}

				else if (uri.startsWith("content://contacts/people")) {
					restrictionName = PrivacyManager.cContacts;
					methodName = "contacts/people";
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
				if (restrictionName != null && methodName != null) {
					if (isRestrictedExtra(param, restrictionName, methodName, uri)) {
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

	private String getIdForUri(String uri) {
		if (uri.startsWith("content://com.android.contacts/contacts"))
			return "_id";
		else if (uri.startsWith("content://com.android.contacts/data"))
			return "contact_id";
		else if (uri.startsWith("content://com.android.contacts/phone_lookup"))
			return "_id";
		else if (uri.startsWith("content://com.android.contacts/raw_contacts"))
			return "contact_id";
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
		_dumpHeader(uri, cursor);
		int i = 0;
		while (cursor.moveToNext() && i++ < 10)
			_dumpColumns(cursor, "");
		cursor.moveToFirst();
	}

	private void _dumpHeader(String uri, Cursor cursor) {
		Util.log(this, Log.WARN, TextUtils.join(", ", cursor.getColumnNames()) + " uri=" + uri);
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

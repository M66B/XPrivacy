package biz.bokhorst.xprivacy;

import java.io.FileNotFoundException;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import android.annotation.SuppressLint;
import android.content.SyncAdapterType;
import android.content.SyncInfo;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.Binder;
import android.os.Bundle;
import android.provider.Settings;
import android.text.TextUtils;
import android.util.Log;

public class XContentResolver extends XHook {
	private Methods mMethod;
	private boolean mClient;
	private String mClassName;

	private XContentResolver(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
		if (className == null)
			mClassName = "com.android.server.content.ContentService";
		else
			mClassName = className;
	}

	private XContentResolver(Methods method, String restrictionName, boolean client) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClient = client;
		mClassName = null;
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

	// public List<SyncInfo> getCurrentSyncs()
	// public void registerContentObserver(android.net.Uri uri, boolean notifyForDescendants, android.database.IContentObserver observer, int userHandle)
	// public void unregisterContentObserver(android.database.IContentObserver observer)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.2.2_r1/android/content/ContentService.java
	// public List<android.content.SyncInfo> getCurrentSyncsAsUser(int userId)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/android/content/IContentService.java

	// public Bundle call(String method, String request, Bundle args)
	// http://developer.android.com/reference/android/provider/Settings.html
	// http://developer.android.com/reference/android/provider/Settings.Global.html
	// http://developer.android.com/reference/android/provider/Settings.Secure.html
	// http://developer.android.com/reference/android/provider/Settings.System.html
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/providers/settings/SettingsProvider.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		getCurrentSync, getCurrentSyncs, getSyncAdapterTypes,
		openAssetFile, openFile, openAssetFileDescriptor, openFileDescriptor, openInputStream, openOutputStream, openTypedAssetFileDescriptor,
		query, Srv_call, Srv_query,
		Srv_getCurrentSyncs, Srv_getCurrentSyncsAsUser
	};
	// @formatter:on

	// @formatter:off
	public static List<String> cProviderClassName = Arrays.asList(new String[] {
		"com.android.providers.downloads.DownloadProvider",
		"com.android.providers.calendar.CalendarProvider2",
		"com.android.providers.contacts.CallLogProvider",
		"com.android.providers.contacts.ContactsProvider2",
		"com.google.android.gm.provider.PublicContentProvider",
		"com.google.android.gsf.gservices.GservicesProvider",
		"com.android.providers.telephony.MmsProvider",
		"com.android.providers.telephony.MmsSmsProvider",
		"com.android.providers.telephony.SmsProvider",
		"com.android.providers.telephony.TelephonyProvider",
		"com.android.providers.userdictionary.UserDictionaryProvider",
		"com.android.providers.settings.SettingsProvider",
	});
	// @formatter:on

	public static List<XHook> getPackageInstances(String packageName, ClassLoader loader) {
		if (packageName.startsWith("com.android.browser.provider"))
			try {
				Class.forName("com.android.browser.provider.BrowserProviderProxy", false, loader);
				return getInstances("com.android.browser.provider.BrowserProviderProxy");
			} catch (ClassNotFoundException ignored) {
				try {
					Class.forName("com.android.browser.provider.BrowserProvider2", false, loader);
					return getInstances("com.android.browser.provider.BrowserProvider2");
				} catch (ClassNotFoundException ignored2) {
					Util.log(null, Log.ERROR, "Browser provider not found, package=" + packageName);
					return new ArrayList<XHook>();
				}
			}

		else if (packageName.startsWith("com.android.email.provider"))
			try {
				Class.forName("com.android.email.provider.EmailProvider", false, loader);
				return getInstances("com.android.email.provider.EmailProvider");
			} catch (ClassNotFoundException ignored) {
				Util.log(null, Log.WARN, "E-mail provider not found, package=" + packageName);
				return new ArrayList<XHook>();
			}

		else if (packageName.startsWith("com.google.android.gm.provider"))
			try {
				Class.forName("com.google.android.gm.provider.PublicContentProvider", false, loader);
				return getInstances("com.google.android.gm.provider.PublicContentProvider");
			} catch (ClassNotFoundException ignored) {
				Util.log(null, Log.WARN, "G-mail provider not found, package=" + packageName);
				return new ArrayList<XHook>();
			}

		else {
			List<XHook> listHook = new ArrayList<XHook>();
			for (String className : cProviderClassName)
				if (className.startsWith(packageName))
					listHook.addAll(getInstances(className));
			return listHook;
		}
	}

	private static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();

		if ("com.android.providers.settings.SettingsProvider".equals(className))
			listHook.add(new XContentResolver(Methods.Srv_call, null, className));
		else
			listHook.add(new XContentResolver(Methods.Srv_query, null, className));

		return listHook;
	}

	public static List<XHook> getInstances(boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();

		if (server) {
			listHook.add(new XContentResolver(Methods.Srv_query, null, "com.android.internal.telephony.IccProvider"));

			listHook.add(new XContentResolver(Methods.Srv_getCurrentSyncs, PrivacyManager.cAccounts, null));
			listHook.add(new XContentResolver(Methods.Srv_getCurrentSyncsAsUser, PrivacyManager.cAccounts, null));
		} else {
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

			listHook.add(new XContentResolver(Methods.query, null, false));
			listHook.add(new XContentResolver(Methods.query, null, true));
		}

		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case getCurrentSync:
		case getCurrentSyncs:
		case getSyncAdapterTypes:
		case openAssetFile:
		case openFile:
		case openAssetFileDescriptor:
		case openFileDescriptor:
		case openInputStream:
		case openOutputStream:
		case openTypedAssetFileDescriptor:
			// Do nothing
			break;

		case Srv_call:
			break;

		case query:
		case Srv_query:
			handleUriBefore(param);
			break;

		case Srv_getCurrentSyncs:
		case Srv_getCurrentSyncsAsUser:
			// Do nothing
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getCurrentSync:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case getCurrentSyncs:
			if (isRestricted(param))
				param.setResult(new ArrayList<SyncInfo>());
			break;

		case getSyncAdapterTypes:
			if (isRestricted(param))
				param.setResult(new SyncAdapterType[0]);
			break;

		case openAssetFileDescriptor:
		case openFileDescriptor:
		case openInputStream:
		case openOutputStream:
		case openTypedAssetFileDescriptor:
		case openAssetFile:
		case openFile:
			if (param.args.length > 0 && param.args[0] instanceof Uri) {
				String uri = ((Uri) param.args[0]).toString();
				if (isRestrictedExtra(param, uri))
					param.setThrowable(new FileNotFoundException("XPrivacy"));
			}
			break;

		case Srv_call:
			handleCallAfter(param);
			break;

		case query:
		case Srv_query:
			handleUriAfter(param);
			break;

		case Srv_getCurrentSyncs:
		case Srv_getCurrentSyncsAsUser:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					int uid = Binder.getCallingUid();
					@SuppressWarnings("unchecked")
					List<SyncInfo> listSync = (List<SyncInfo>) param.getResult();
					List<SyncInfo> listFiltered = new ArrayList<SyncInfo>();
					for (SyncInfo sync : listSync)
						if (XAccountManager.isAccountAllowed(sync.account, uid))
							listFiltered.add(sync);
					param.setResult(listFiltered);
				}
			break;
		}
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
						if (packageName != null && XPackageManager.isPackageAllowed(0, packageName))
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
									if ("android_id".equals(cursor.getString(ikey)) && cursor.getString(ivalue) != null)
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

	private void handleCallAfter(XParam param) throws Throwable {
		if (param.args.length > 1 && param.args[0] instanceof String && param.args[1] instanceof String) {
			String method = (String) param.args[0];
			String request = (String) param.args[1];

			if ("GET_secure".equals(method)) {
				if (Settings.Secure.ANDROID_ID.equals(request)) {
					if (!hasEmptyValue(param.getResult()))
						if (isRestricted(param, PrivacyManager.cIdentification, "Srv_Android_ID")) {
							int uid = Binder.getCallingUid();
							String value = (String) PrivacyManager.getDefacedProp(uid, "ANDROID_ID");
							Bundle bundle = new Bundle(1);
							bundle.putString("value", value);
							param.setResult(bundle);
						}

				}

			} else if ("GET_system".equals(method)) {
				// Do nothing

			} else if ("GET_global".equals(method)) {
				if ("default_dns_server".equals(request)) {
					if (!hasEmptyValue(param.getResult()))
						if (isRestricted(param, PrivacyManager.cNetwork, "Srv_Default_DNS")) {
							int uid = Binder.getCallingUid();
							InetAddress value = (InetAddress) PrivacyManager.getDefacedProp(uid, "InetAddress");
							Bundle bundle = new Bundle(1);
							bundle.putString("value", value.getHostAddress());
							param.setResult(bundle);
						}

				} else if ("wifi_country_code".equals(request)) {
					if (!hasEmptyValue(param.getResult()))
						if (isRestricted(param, PrivacyManager.cNetwork, "Srv_WiFi_Country")) {
							int uid = Binder.getCallingUid();
							String value = (String) PrivacyManager.getDefacedProp(uid, "CountryIso");
							Bundle bundle = new Bundle(1);
							bundle.putString("value", value == null ? null : value.toLowerCase(Locale.ROOT));
							param.setResult(bundle);
						}
				}
			}
		}
	}

	// Helper methods

	private boolean hasEmptyValue(Object result) {
		Bundle bundle = (Bundle) result;
		if (bundle == null)
			return true;
		if (!bundle.containsKey("value"))
			return true;
		return (bundle.get("value") == null);
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

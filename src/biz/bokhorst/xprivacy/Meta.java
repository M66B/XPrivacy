package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.res.Resources;
import android.nfc.NfcAdapter;
import android.os.Build;
import android.provider.MediaStore;
import android.provider.Telephony;
import android.telephony.TelephonyManager;
import android.util.Log;

@SuppressLint("InlinedApi")
public class Meta {
	private static boolean mAnnotated = false;
	private static List<Hook> mListHook = new ArrayList<Hook>();

	public final static String cTypeAccount = "Account";
	public final static String cTypeAccountHash = "AccountHash";
	public final static String cTypeApplication = "Application";
	public final static String cTypeContact = "Contact";
	public final static String cTypeTemplate = "Template";
	public final static String cTypeTemplateName = "TemplateName";

	public final static String cTypeAddress = "Address";
	public final static String cTypeAction = "Action";
	public final static String cTypeCommand = "Command";
	public final static String cTypeFilename = "Filename";
	public final static String cTypeIPAddress = "IPAddress";
	public final static String cTypeLibrary = "Library";
	public final static String cTypeMethod = "Method";
	public final static String cTypePermission = "Permission";
	public final static String cTypeProc = "Proc";
	public final static String cTypeTransaction = "Transaction";
	public final static String cTypeUrl = "Url";

	public static boolean isWhitelist(String type) {
		return (cTypeAddress.equals(type) || cTypeAction.equals(type) || cTypeCommand.equals(type)
				|| cTypeFilename.equals(type) || cTypeIPAddress.equals(type) || cTypeLibrary.equals(type)
				|| cTypeMethod.equals(type) || cTypePermission.equals(type) || cTypeProc.equals(type)
				|| cTypeTransaction.equals(type) || cTypeUrl.equals(type));
	}

	public static List<Hook> get() {
		// http://developer.android.com/reference/android/Manifest.permission.html
		if (mListHook.size() > 0)
			return mListHook;

		// @formatter:off
		mListHook.add(new Hook("accounts", "addOnAccountsUpdatedListener", "GET_ACCOUNTS", 5, null, null).notAOSP(19));
		mListHook.add(new Hook("accounts", "blockingGetAuthToken", "USE_CREDENTIALS", 5, null, null).dangerous().unsafe());
		mListHook.add(new Hook("accounts", "getAccounts", "GET_ACCOUNTS", 5, null, null).notAOSP(19));
		mListHook.add(new Hook("accounts", "getAccountsByType", "GET_ACCOUNTS", 5, null, null).notAOSP(19));
		mListHook.add(new Hook("accounts", "getAccountsByTypeAndFeatures", "GET_ACCOUNTS", 5, null, null).notAOSP(19));
		mListHook.add(new Hook("accounts", "getAuthToken", "USE_CREDENTIALS", 5, null, null).unsafe().dangerous());
		mListHook.add(new Hook("accounts", "getAuthTokenByFeatures", "MANAGE_ACCOUNTS", 5, null, null).unsafe().dangerous());
		mListHook.add(new Hook("accounts", "hasFeatures", "GET_ACCOUNTS", 8, null, null).unsafe().dangerous());
		mListHook.add(new Hook("accounts", "getAccountsByTypeForPackage", "GET_ACCOUNTS", 18, null, null).notAOSP(19));

		mListHook.add(new Hook("accounts", "getTokenGoogle", "GET_ACCOUNTS", 1, null, null).unsafe().dangerous().optional());
		mListHook.add(new Hook("accounts", "getTokenWithNotificationGoogle", "GET_ACCOUNTS", 1, null, null).unsafe().dangerous().optional());

		mListHook.add(new Hook("accounts", "getAuthenticatorTypes", "GET_ACCOUNTS", 5, "1.99.24", null).unsafe().dangerous());
		mListHook.add(new Hook("accounts", "getCurrentSync", "READ_SYNC_SETTINGS", 8, "1.99.24", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("accounts", "getCurrentSyncs", "READ_SYNC_SETTINGS", 11, "1.99.24", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("accounts", "getSyncAdapterTypes", "", 5, "1.99.24", null).unsafe().dangerous());

		mListHook.add(new Hook("accounts", "Srv_getAccounts", "GET_ACCOUNTS", 19, "2.99", "getAccounts").AOSP(19));
		mListHook.add(new Hook("accounts", "Srv_getAccountsAsUser", "GET_ACCOUNTS", 19, "2.99", null).AOSP(19));
		mListHook.add(new Hook("accounts", "Srv_getAccountsByFeatures", "GET_ACCOUNTS", 19, "2.99", "getAccountsByTypeAndFeatures").AOSP(19));
		mListHook.add(new Hook("accounts", "Srv_getAccountsForPackage", "GET_ACCOUNTS", 19, "3.5.6", null).AOSP(19));
		mListHook.add(new Hook("accounts", "Srv_getSharedAccountsAsUser", "GET_ACCOUNTS", 19, "2.99", null).AOSP(19));
		mListHook.add(new Hook("accounts", "Srv_getCurrentSyncs", "READ_SYNC_SETTINGS", 19, "2.99", "getCurrentSyncs").AOSP(19));
		mListHook.add(new Hook("accounts", "Srv_getCurrentSyncsAsUser", "READ_SYNC_SETTINGS", 21, "3.5.6", null).AOSP(21));

		mListHook.add(new Hook("browser", "BrowserProvider2", "com.android.browser.permission.READ_HISTORY_BOOKMARKS,GLOBAL_SEARCH", 1, null, null));
		mListHook.add(new Hook("browser", "Downloads", "ACCESS_DOWNLOAD_MANAGER,ACCESS_DOWNLOAD_MANAGER_ADVANCED,ACCESS_ALL_DOWNLOADS", 1, "1.99.43", null).dangerous());

		mListHook.add(new Hook("calendar", "CalendarProvider2", "READ_CALENDAR,WRITE_CALENDAR", 1, null, null));

		mListHook.add(new Hook("calling", "sendDataMessage", "SEND_SMS", 4, null, null).notAOSP(19).whitelist(cTypeAddress).doNotify());
		mListHook.add(new Hook("calling", "sendMultimediaMessage", "SEND_SMS", 21, "3.5.6", null).doNotify());
		mListHook.add(new Hook("calling", "sendMultipartTextMessage", "SEND_SMS", 4, null, null).notAOSP(19).whitelist(cTypeAddress).doNotify());
		mListHook.add(new Hook("calling", "sendTextMessage", "SEND_SMS", 4, null, null).notAOSP(19).whitelist(cTypeAddress).doNotify());

		mListHook.add(new Hook("calling", "Srv_sendData", "SEND_SMS", 19, "2.99", "sendDataMessage").AOSP(19).whitelist(cTypeAddress).doNotify());
		mListHook.add(new Hook("calling", "Srv_sendMultipartText", "SEND_SMS", 19, "2.99", "sendMultipartTextMessage").AOSP(19).whitelist(cTypeAddress).doNotify());
		mListHook.add(new Hook("calling", "Srv_sendText", "SEND_SMS", 19, "2.99", "sendTextMessage").AOSP(19).whitelist(cTypeAddress).doNotify());

		mListHook.add(new Hook("calling", TelephonyManager.ACTION_RESPOND_VIA_MESSAGE, "SEND_RESPOND_VIA_MESSAGE", 18, null, null).doNotify());
		mListHook.add(new Hook("calling", Intent.ACTION_CALL, "CALL_PHONE", 10, null, null).doNotify());
		mListHook.add(new Hook("calling", Intent.ACTION_DIAL, "", 10, "2.2.2", null).doNotify());
		mListHook.add(new Hook("calling", Intent.ACTION_NEW_OUTGOING_CALL, "PROCESS_OUTGOING_CALLS", 10, "2.1.23", "phone/android.intent.action.NEW_OUTGOING_CALL").doNotify());
		mListHook.add(new Hook("calling", "CallLogProvider", "READ_CALL_LOG,WRITE_CALL_LOG", 1, "2.1.23", "phone/CallLogProvider"));

		mListHook.add(new Hook("calling", "SIP.isApiSupported", "USE_SIP", 9, null, null).unsafe().doNotify());
		mListHook.add(new Hook("calling", "SIP.isSipWifiOnly", "USE_SIP", 9, null, null).unsafe().doNotify());
		mListHook.add(new Hook("calling", "SIP.isVoipSupported", "USE_SIP", 9, null, null).unsafe().doNotify());
		mListHook.add(new Hook("calling", "SIP.newInstance", "USE_SIP", 9, null, null).unsafe().doNotify());

		mListHook.add(new Hook("clipboard", "addPrimaryClipChangedListener", "", 11, null, null).notAOSP(19));
		mListHook.add(new Hook("clipboard", "getPrimaryClip", "", 11, null, null).notAOSP(19).doNotify());
		mListHook.add(new Hook("clipboard", "getPrimaryClipDescription", "", 11, null, null).notAOSP(19).doNotify());
		mListHook.add(new Hook("clipboard", "getText", "", 10, null, null).notAOSP(19).doNotify());
		mListHook.add(new Hook("clipboard", "hasPrimaryClip", "", 11, null, null).notAOSP(19).doNotify());
		mListHook.add(new Hook("clipboard", "hasText", "", 10, null, null).notAOSP(19).doNotify());

		mListHook.add(new Hook("clipboard", "Srv_addPrimaryClipChangedListener", "", 19, "2.99", "addPrimaryClipChangedListener").AOSP(19));
		mListHook.add(new Hook("clipboard", "Srv_getPrimaryClip", "", 19, "2.99", "getPrimaryClip").AOSP(19).doNotify());
		mListHook.add(new Hook("clipboard", "Srv_getPrimaryClipDescription", "", 19, "2.99", "getPrimaryClipDescription").AOSP(19).doNotify());
		mListHook.add(new Hook("clipboard", "Srv_hasClipboardText", "", 19, "2.99", "hasText").AOSP(19).doNotify());
		mListHook.add(new Hook("clipboard", "Srv_hasPrimaryClip", "", 19, "2.99", "hasPrimaryClip").AOSP(19).doNotify());

		mListHook.add(new Hook("contacts", "contacts/contacts", "READ_CONTACTS,WRITE_CONTACTS", 1, null, null));
		mListHook.add(new Hook("contacts", "contacts/data", "READ_CONTACTS,WRITE_CONTACTS", 1, null, null));
		mListHook.add(new Hook("contacts", "contacts/people", "READ_CONTACTS,WRITE_CONTACTS", 1, "1.99.46", null));
		mListHook.add(new Hook("contacts", "contacts/phone_lookup", "READ_CONTACTS,WRITE_CONTACTS", 1, null, null));
		mListHook.add(new Hook("contacts", "contacts/profile", "READ_PROFILE,WRITE_PROFILE", 1, "1.99.38", null).dangerous());
		mListHook.add(new Hook("contacts", "contacts/raw_contacts", "READ_CONTACTS,WRITE_CONTACTS", 1, null, null));
		mListHook.add(new Hook("contacts", "ContactsProvider2", "READ_CONTACTS,WRITE_CONTACTS,READ_PROFILE,WRITE_PROFILE", 1, "1.99.38", null).dangerous());
		mListHook.add(new Hook("contacts", "IccProvider", "READ_CONTACTS,WRITE_CONTACTS", 1, "1.99.38", null));

		mListHook.add(new Hook("dictionary", "UserDictionary", "READ_USER_DICTIONARY", 1, null, null));

		mListHook.add(new Hook("email", "EMailProvider", "com.android.email.permission.ACCESS_PROVIDER", 1, null, null));
		mListHook.add(new Hook("email", "GMailProvider", "com.google.android.gm.permission.READ_CONTENT_PROVIDER", 8, "1.99.20", null));

		mListHook.add(new Hook("identification", "%hostname", "", 1, null, null).unsafe());
		mListHook.add(new Hook("identification", "%imei", "", 1, null, null).unsafe());
		mListHook.add(new Hook("identification", "%macaddr", "", 1, null, null).unsafe());
		mListHook.add(new Hook("identification", "%serialno", "", 1, null, null).unsafe());
		mListHook.add(new Hook("identification", "%cid", "", 1, null, null).unsafe());
		mListHook.add(new Hook("identification", "/proc", "", 1, "1.7", null).unsafe().dangerous().whitelist(cTypeProc));
		mListHook.add(new Hook("identification", "/system/build.prop", "", 1, "1.9.9", null).unsafe().dangerous());
		mListHook.add(new Hook("identification", "/sys/block/.../cid", "", 1, null, null).unsafe().dangerous());
		mListHook.add(new Hook("identification", "/sys/class/.../cid", "", 1, null, null).unsafe().dangerous());
		mListHook.add(new Hook("identification", "AdvertisingId", "", 1, null, null).unsafe().optional());

		mListHook.add(new Hook("identification", "getString", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook("identification", "InputDevice.getDescriptor", "", 16, "2.2.2", "getDescriptor").unsafe());
		mListHook.add(new Hook("identification", "InputDevice.getName", "", 9, null, null).unsafe());
		mListHook.add(new Hook("identification", "GservicesProvider", "com.google.android.providers.gsf.permission.READ_GSERVICES,com.google.android.providers.gsf.permission.WRITE_GSERVICES", 1, null, null).dangerous());
		mListHook.add(new Hook("identification", "SERIAL", "", 1, null, null).restart().noUsageData());

		mListHook.add(new Hook("identification", "USB.getDeviceId", "", 12, "2.1.7", null).unsafe());
		mListHook.add(new Hook("identification", "USB.getDeviceName", "", 12, "2.1.7", null).unsafe());
		mListHook.add(new Hook("identification", "USB.getSerialNumber", "", 20, "2.1.17", null).unsafe());

		mListHook.add(new Hook("identification", "Srv_Android_ID", "", 19, "2.99", "getString").AOSP(19));

		mListHook.add(new Hook("identification", "Cast.getDeviceId", "", 1, "3.5.11", null).unsafe());
		mListHook.add(new Hook("identification", "Cast.getIpAddress", "", 1, "3.5.11", null).unsafe());

		// java.net.NetworkInterface
		mListHook.add(new Hook("internet", "NetworkInterface.getByIndex", "INTERNET", 19, "2.2.2", null).unsafe());
		mListHook.add(new Hook("internet", "NetworkInterface.getByInetAddress", "INTERNET", 1, "2.2.2", "getByInetAddress").unsafe());
		mListHook.add(new Hook("internet", "NetworkInterface.getByName", "INTERNET", 1, "2.2.2", "getByName").unsafe().dangerous().whitelist(cTypeIPAddress));
		mListHook.add(new Hook("internet", "NetworkInterface.getNetworkInterfaces", "INTERNET", 1, "2.2.2", "getNetworkInterfaces").unsafe());

		mListHook.add(new Hook("internet", "inet", "INTERNET", 1, null, null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("internet", "inet_admin", "NET_ADMIN", 1, "2.1.1", null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("internet", "inet_bw", "READ_NETWORK_USAGE_HISTORY,MODIFY_NETWORK_ACCOUNTING", 1, "2.1.1", null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("internet", "inet_vpn", "NET_TUNNELING", 1, "2.1.1", null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("internet", "inet_mesh", "LOOP_RADIO", 1, "2.1.1", null).dangerous().restart().noUsageData());

		// android.net.ConnectivityManager
		mListHook.add(new Hook("internet", "Connectivity.getActiveNetworkInfo", null, 1, "2.2.2", "getActiveNetworkInfo").unsafe().dangerous());
		mListHook.add(new Hook("internet", "Connectivity.getAllNetworkInfo", null, 1, "2.2.2", "getAllNetworkInfo").unsafe());
		mListHook.add(new Hook("internet", "Connectivity.getNetworkInfo", null, 1, "2.2.2", "getNetworkInfo").unsafe().dangerous());

		// android.net.NetworkInfo
		mListHook.add(new Hook("internet", "NetworkInfo.getDetailedState", null, 1, "2.2.2", "getDetailedState").unsafe());
		mListHook.add(new Hook("internet", "NetworkInfo.getState", null, 1, "2.2.2", "getState").unsafe());
		mListHook.add(new Hook("internet", "NetworkInfo.isConnected", null, 1, "2.2.2", "isConnected").unsafe());
		mListHook.add(new Hook("internet", "NetworkInfo.isConnectedOrConnecting", null, 1, "2.2.2", "isConnectedOrConnecting").unsafe());

		// android.net.wifi.WifiManager
		mListHook.add(new Hook("internet", "WiFi.getConnectionInfo", null, 10, "2.2.2", "getConnectionInfo").notAOSP(19));
		mListHook.add(new Hook("internet", "WiFi.Srv_getConnectionInfo", null, 10, "2.99", "WiFi.getConnectionInfo").AOSP(19));

		// java.net.InetAddress
		mListHook.add(new Hook("internet", "InetAddress.getAllByName", "INTERNET", 1, null, null).unsafe().dangerous().whitelist(cTypeIPAddress));
		mListHook.add(new Hook("internet", "InetAddress.getAllByNameOnNet", "INTERNET", 21, "3.5.6", null).unsafe().dangerous().whitelist(cTypeIPAddress));
		mListHook.add(new Hook("internet", "InetAddress.getByAddress", "INTERNET", 1, null, null).unsafe().dangerous().whitelist(cTypeIPAddress));
		mListHook.add(new Hook("internet", "InetAddress.getByName", "INTERNET", 1, null, null).unsafe().dangerous().whitelist(cTypeIPAddress));
		mListHook.add(new Hook("internet", "InetAddress.getByNameOnNet", "INTERNET", 21, "3.5.6", null).unsafe().dangerous().whitelist(cTypeIPAddress));

		// android.net.IpPrefix
		mListHook.add(new Hook("internet", "IpPrefix.getAddress", null, 21, "3.5.6", null).dangerous().unsafe());
		mListHook.add(new Hook("internet", "IpPrefix.getRawAddress", null, 21, "3.5.6", null).dangerous().unsafe());

		// android.net.LinkProperties
		mListHook.add(new Hook("internet", "LinkProperties.getAddresses", null, 19, "3.5.6", null).dangerous().unsafe());
		mListHook.add(new Hook("internet", "LinkProperties.getAllAddresses", null, 19, "3.5.6", null).dangerous().unsafe());
		mListHook.add(new Hook("internet", "LinkProperties.getAllLinkAddresses", null, 19, "3.5.6", null).dangerous().unsafe());
		mListHook.add(new Hook("internet", "LinkProperties.getLinkAddresses", null, 19, "3.5.6", null).dangerous().unsafe());
		mListHook.add(new Hook("internet", "LinkProperties.getStackedLinks", null, 19, "3.5.6", null).dangerous().unsafe());

		mListHook.add(new Hook("internet", "connect", null, 1, "1.99.45", null).unsafe().dangerous().whitelist(cTypeIPAddress));

		mListHook.add(new Hook("ipc", "Binder", "", 1, "2.1.21", null).notAOSP(19).dangerous().whitelist(cTypeTransaction));

		mListHook.add(new Hook("location", "addGeofence", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 17, null, null).notAOSP(19));
		mListHook.add(new Hook("location", "addGpsStatusListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 3, "2.1.17", null).notAOSP(19));
		mListHook.add(new Hook("location", "addNmeaListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 5, null, null).notAOSP(19));
		mListHook.add(new Hook("location", "addProximityAlert", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null).notAOSP(19));
		mListHook.add(new Hook("location", "getAllProviders", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.20", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("location", "getBestProvider", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.20", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("location", "getGpsStatus", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 3, "1.99.29", null).notAOSP(19));
		mListHook.add(new Hook("location", "getLastKnownLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null).notAOSP(19));
		mListHook.add(new Hook("location", "getProviders", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "1.99.1", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("location", "isProviderEnabled", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "1.99.1", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("location", "requestLocationUpdates", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null).restart().notAOSP(19));
		mListHook.add(new Hook("location", "requestSingleUpdate", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 9, null, null).restart().notAOSP(19));
		mListHook.add(new Hook("location", "sendExtraCommand", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 3, null, null).notAOSP(19));

		mListHook.add(new Hook("location", "Srv_requestGeofence", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 17, "2.99", "addGeofence").AOSP(19));
		mListHook.add(new Hook("location", "Srv_addGpsStatusListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 3, "2.99", "addGpsStatusListener").AOSP(19));
		mListHook.add(new Hook("location", "Srv_getAllProviders", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99", "getAllProviders").AOSP(19).dangerous());
		mListHook.add(new Hook("location", "Srv_getBestProvider", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99", "getBestProvider").AOSP(19).dangerous());
		mListHook.add(new Hook("location", "Srv_getProviders", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99", "getProviders").AOSP(19).dangerous());
		mListHook.add(new Hook("location", "Srv_isProviderEnabled", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99", "isProviderEnabled").AOSP(19).dangerous());
		mListHook.add(new Hook("location", "Srv_getLastLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99", "getLastKnownLocation").AOSP(19));
		mListHook.add(new Hook("location", "Srv_requestLocationUpdates", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99", "requestLocationUpdates").restart().AOSP(19));
		mListHook.add(new Hook("location", "Srv_sendExtraCommand", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 3, "2.99", "sendExtraCommand").AOSP(19));
		mListHook.add(new Hook("location", "Srv_addGpsMeasurementsListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 21, "3.5.6", null).AOSP(21));
		mListHook.add(new Hook("location", "Srv_addGpsNavigationMessageListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 21, "3.5.6", null).AOSP(21));

		mListHook.add(new Hook("location", "enableLocationUpdates", "CONTROL_LOCATION_UPDATES", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("location", "getAllCellInfo", "ACCESS_COARSE_UPDATES", 17, null, null).notAOSP(19));
		mListHook.add(new Hook("location", "getCellLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null).notAOSP(19));
		mListHook.add(new Hook("location", "getNeighboringCellInfo", "ACCESS_COARSE_UPDATES", 3, null, null).notAOSP(19));

		mListHook.add(new Hook("location", "Srv_enableLocationUpdates", "CONTROL_LOCATION_UPDATES", 10, "2.99", "enableLocationUpdates").AOSP(19));
		mListHook.add(new Hook("location", "Srv_enableLocationUpdatesForSubscriber", "CONTROL_LOCATION_UPDATES", 21, "3.5.6", null).AOSP(21));
		mListHook.add(new Hook("location", "Srv_getAllCellInfo", "ACCESS_COARSE_UPDATES", 17, "2.99", "getAllCellInfo").AOSP(19));
		mListHook.add(new Hook("location", "Srv_getCellLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99", "getCellLocation").AOSP(19));
		mListHook.add(new Hook("location", "Srv_getNeighboringCellInfo", "ACCESS_COARSE_UPDATES", 3, "2.99", "getNeighboringCellInfo").AOSP(19));

		mListHook.add(new Hook("location", "WiFi.getScanResults", "ACCESS_WIFI_STATE", 1, "2.2.2", "getScanResults").notAOSP(19).dangerous());
		mListHook.add(new Hook("location", "WiFi.Srv_getScanResults", "ACCESS_WIFI_STATE", 1, "2.99", "WiFi.getScanResults").AOSP(19).dangerous());

		mListHook.add(new Hook("location", "listen", "ACCESS_COARSE_LOCATION", 1, null, null).notAOSP(19));
		mListHook.add(new Hook("location", "Srv_listen", "ACCESS_COARSE_LOCATION", 1, null, null).AOSP(19));

		mListHook.add(new Hook("location", "GMS.addGeofences", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null).unsafe().optional());
		mListHook.add(new Hook("location", "GMS.getLastLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null).unsafe().optional());
		mListHook.add(new Hook("location", "GMS.requestLocationUpdates", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null).unsafe().optional());
		mListHook.add(new Hook("location", "GMS.requestActivityUpdates", "com.google.android.gms.permission.ACTIVITY_RECOGNITION", 1, null, null).unsafe());

		mListHook.add(new Hook("location", "GMS5.getLastLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99.26", null).unsafe().optional());
		mListHook.add(new Hook("location", "GMS5.requestLocationUpdates", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.99.26", null).unsafe().optional());
		mListHook.add(new Hook("location", "GMS5.requestActivityUpdates", "com.google.android.gms.permission.ACTIVITY_RECOGNITION", 1, "2.99.26", null).unsafe());

		mListHook.add(new Hook("location", "GMS5.getCurrentPlace", "ACCESS_FINE_LOCATION", 1, "3.6.9", null).unsafe());

		mListHook.add(new Hook("location", "MapV1.enableMyLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.25", null).unsafe().optional());

		mListHook.add(new Hook("location", "MapV2.getMyLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.25", null).unsafe().optional());
		mListHook.add(new Hook("location", "MapV2.getPosition", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.25", null).unsafe().optional());
		mListHook.add(new Hook("location", "MapV2.setLocationSource", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.25", null).unsafe().optional());
		mListHook.add(new Hook("location", "MapV2.setOnMapClickListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.25", null).unsafe().optional());
		mListHook.add(new Hook("location", "MapV2.setOnMapLongClickListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.25", null).unsafe().optional());
		mListHook.add(new Hook("location", "MapV2.setOnMyLocationChangeListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "2.1.25", null).unsafe().optional());

		mListHook.add(new Hook("media", "Audio.startRecording", "RECORD_AUDIO", 3, "2.2.3", "startRecording").unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera.setPreviewCallback", "CAMERA", 1, "2.99.21", "setPreviewCallback").unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera.setPreviewCallbackWithBuffer", "CAMERA", 8, "2.99.21", null).unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera.setPreviewDisplay", "CAMERA", 1, "2.99.21", null).unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera.setPreviewTexture", "CAMERA", 11, "2.99.21", null).unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera.setOneShotPreviewCallback", "CAMERA", 11, "2.99.21", null).unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera.startPreview", "CAMERA", 1, "2.2.3", "setPreviewCallback").unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera.takePicture", "CAMERA", 1, "2.2.3", "takePicture").unsafe().doNotify());
		mListHook.add(new Hook("media", "MediaRecorder.start", "RECORD_AUDIO,RECORD_VIDEO", 1, "2.2.3", "setOutputFile").unsafe().doNotify());
		mListHook.add(new Hook("media", "MediaRecorder.setOutputFile", "RECORD_AUDIO,RECORD_VIDEO", 1, "2.99.20", "setOutputFile").unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera.permission", "CAMERA", 1, "2.2.3", null).dangerous().doNotify());
		mListHook.add(new Hook("media", "Record.Audio.permission", "RECORD_AUDIO", 3, "2.2.3", null).dangerous().doNotify());
		mListHook.add(new Hook("media", "Record.Video.permission", "RECORD_VIDEO", 3, "2.2.3", null).dangerous().doNotify());

		mListHook.add(new Hook("media", MediaStore.ACTION_IMAGE_CAPTURE, "", 3, null, null).doNotify());
		mListHook.add(new Hook("media", MediaStore.ACTION_IMAGE_CAPTURE_SECURE, "", 17, null, null).doNotify());
		mListHook.add(new Hook("media", MediaStore.ACTION_VIDEO_CAPTURE, "", 3, null, null).doNotify());

		mListHook.add(new Hook("media", "Camera2.capture", "CAMERA", 20, null, null).unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera2.captureBurst", "CAMERA", 20, null, null).unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera2.setRepeatingRequest", "CAMERA", 20, null, null).unsafe().doNotify());
		mListHook.add(new Hook("media", "Camera2.setRepeatingBurst", "CAMERA", 20, null, null).unsafe().doNotify());

		mListHook.add(new Hook("messages", "getAllMessagesFromIcc", "RECEIVE_SMS", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("messages", "getCarrierConfigValues", "", 21, "3.5.6", null));
		mListHook.add(new Hook("messages", "Srv_getAllMessagesFromIccEf", "RECEIVE_SMS", 19, "2.99", "getAllMessagesFromIcc").AOSP(19));

		mListHook.add(new Hook("messages", "SmsProvider", "READ_SMS,WRITE_SMS", 1, null, null));
		mListHook.add(new Hook("messages", "MmsProvider", "READ_SMS,WRITE_SMS", 1, null, null));
		mListHook.add(new Hook("messages", "MmsSmsProvider", "READ_SMS,WRITE_SMS", 1, null, null));
		mListHook.add(new Hook("messages", "VoicemailContentProvider", "com.android.voicemail.permission.READ_WRITE_ALL_VOICEMAIL", 1, null, null));

		mListHook.add(new Hook("messages", Telephony.Sms.Intents.DATA_SMS_RECEIVED_ACTION, "RECEIVE_SMS", 1, null, null));
		mListHook.add(new Hook("messages", Telephony.Sms.Intents.SMS_RECEIVED_ACTION, "RECEIVE_SMS", 1, null, null));
		mListHook.add(new Hook("messages", Telephony.Sms.Intents.WAP_PUSH_RECEIVED_ACTION, "RECEIVE_WAP_PUSH", 1, null, null));
		mListHook.add(new Hook("messages", Telephony.Sms.Intents.SMS_DELIVER_ACTION, "BROADCAST_SMS", 19, "2.2.2", null));
		mListHook.add(new Hook("messages", Telephony.Sms.Intents.WAP_PUSH_DELIVER_ACTION, "BROADCAST_WAP_PUSH", 19, "2.2.2", null));

		// android.bluetooth.BluetoothAdapter/BluetoothDevice
		mListHook.add(new Hook("network", "Bluetooth.getAddress", "BLUETOOTH", 5, "2.2.3", "getAddress").unsafe());
		mListHook.add(new Hook("network", "Bluetooth.getBondedDevices", "BLUETOOTH", 5, "2.2.3", "getBondedDevices").unsafe());
		mListHook.add(new Hook("network", "Bluetooth.Srv_getAddress", "BLUETOOTH", 5, "2.99", "getAddress").AOSP(19));
		mListHook.add(new Hook("network", "Bluetooth.Srv_getName", "BLUETOOTH", 5, "2.99", null).AOSP(19));

		// java.net.NetworkInterface
		mListHook.add(new Hook("network", "NetworkInterface.getHardwareAddress", "ACCESS_NETWORK_STATE", 9, "2.2.2", "getHardwareAddress").unsafe());
		mListHook.add(new Hook("network", "NetworkInterface.getInetAddresses", "ACCESS_NETWORK_STATE", 9, "2.2.2", "getInetAddresses").unsafe());
		mListHook.add(new Hook("network", "NetworkInterface.getInterfaceAddresses", "ACCESS_NETWORK_STATE", 9, "2.2.2", "getInterfaceAddresses").unsafe());

		// android.net.wifi.WifiManager
		mListHook.add(new Hook("network", "WiFi.getConfiguredNetworks", "ACCESS_WIFI_STATE", 10, "2.2.2", "getConfiguredNetworks").notAOSP(19));
		mListHook.add(new Hook("network", "WiFi.getConnectionInfo", "ACCESS_WIFI_STATE", 10, "2.2.2", "getConnectionInfo").notAOSP(19));
		mListHook.add(new Hook("network", "WiFi.getDhcpInfo", "ACCESS_WIFI_STATE", 10, "2.2.2", "getDhcpInfo").notAOSP(19));
		mListHook.add(new Hook("network", "WiFi.getScanResults", "ACCESS_WIFI_STATE", 10, "2.2.2", "getScanResults").notAOSP(19).dangerous());
		mListHook.add(new Hook("network", "WiFi.getWifiApConfiguration", "ACCESS_WIFI_STATE", 10, "2.2.2", "getWifiApConfiguration").notAOSP(19));

		mListHook.add(new Hook("network", "WiFi.Srv_getBatchedScanResults", "ACCESS_WIFI_STATE", 10, "2.99", null).AOSP(19).dangerous());
		mListHook.add(new Hook("network", "WiFi.Srv_getConfiguredNetworks", "ACCESS_WIFI_STATE", 10, "2.99", "WiFi.getConfiguredNetworks").AOSP(19));
		mListHook.add(new Hook("network", "WiFi.Srv_getConnectionInfo", "ACCESS_WIFI_STATE", 10, "2.99", "WiFi.getConnectionInfo").AOSP(19));
		mListHook.add(new Hook("network", "WiFi.Srv_getDhcpInfo", "ACCESS_WIFI_STATE", 10, "2.99", "WiFi.getDhcpInfo").AOSP(19));
		mListHook.add(new Hook("network", "WiFi.Srv_getScanResults", "ACCESS_WIFI_STATE", 10, "2.99", "WiFi.getScanResults").AOSP(19).dangerous());
		mListHook.add(new Hook("network", "WiFi.Srv_getWifiApConfiguration", "ACCESS_WIFI_STATE", 10, "2.99", "WiFi.getWifiApConfiguration").AOSP(19));

		mListHook.add(new Hook("network", "Srv_Default_DNS", "", 19, "2.99", "getString").AOSP(19).dangerous());
		mListHook.add(new Hook("network", "Srv_WiFi_Country", "", 19, "2.99", "getString").AOSP(19).dangerous());

		// android.net.NetworkInfo
		mListHook.add(new Hook("network", "NetworkInfo.getExtraInfo", null, 1, "2.2.2", "internet/getExtraInfo").unsafe());

		mListHook.add(new Hook("nfc", "getNfcAdapter", "NFC", 14, null, null).unsafe());
		mListHook.add(new Hook("nfc", "getDefaultAdapter", "NFC", 10, null, null).unsafe());

		mListHook.add(new Hook("nfc", NfcAdapter.ACTION_ADAPTER_STATE_CHANGED, "NFC", 18, null, null));
		mListHook.add(new Hook("nfc", NfcAdapter.ACTION_NDEF_DISCOVERED, "NFC", 10, null, null));
		mListHook.add(new Hook("nfc", NfcAdapter.ACTION_TAG_DISCOVERED, "NFC", 10, null, null));
		mListHook.add(new Hook("nfc", NfcAdapter.ACTION_TECH_DISCOVERED, "NFC", 10, null, null));

		mListHook.add(new Hook("notifications", "android.service.notification.NotificationListenerService", "BIND_NOTIFICATION_LISTENER_SERVICE", 18, null, null).unsafe());
		mListHook.add(new Hook("notifications", "com.google.android.c2dm.intent.REGISTRATION", "com.google.android.c2dm.permission.RECEIVE", 10, null, null).dangerous());
		mListHook.add(new Hook("notifications", "com.google.android.c2dm.intent.RECEIVE", "com.google.android.c2dm.permission.RECEIVE", 10, null, null).dangerous());

		mListHook.add(new Hook("overlay", "addView", "SYSTEM_ALERT_WINDOW", 1, null, null).unsafe().optional());

		mListHook.add(new Hook("phone", "getDeviceId", "READ_PHONE_STATE", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getGroupIdLevel1", "READ_PHONE_STATE", 18, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getIsimDomain", "READ_PRIVILEGED_PHONE_STATE", 14, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getIsimImpi", "READ_PRIVILEGED_PHONE_STATE", 14, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getIsimImpu", "READ_PRIVILEGED_PHONE_STATE", 14, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getLine1AlphaTag", "READ_PHONE_STATE", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getLine1Number", "READ_PHONE_STATE", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getMsisdn", "READ_PHONE_STATE", 14, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getSimSerialNumber", "READ_PHONE_STATE", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getSubscriberId", "READ_PHONE_STATE", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getVoiceMailAlphaTag", "READ_PHONE_STATE", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "getVoiceMailNumber", "READ_PHONE_STATE", 10, null, null).notAOSP(19));

		mListHook.add(new Hook("phone", "Srv_getDeviceId", "READ_PHONE_STATE", 10, "2.99", "getDeviceId").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getGroupIdLevel1", "READ_PHONE_STATE", 18, "2.99", "getGroupIdLevel1").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getIsimDomain", "READ_PRIVILEGED_PHONE_STATE", 14, "2.99", "getIsimDomain").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getIsimImpi", "READ_PRIVILEGED_PHONE_STATE", 14, "2.99", "getIsimImpi").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getIsimImpu", "READ_PRIVILEGED_PHONE_STATE", 14, "2.99", "getIsimImpu").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getLine1AlphaTag", "READ_PHONE_STATE", 10, "2.99", "getLine1AlphaTag").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getLine1Number", "READ_PHONE_STATE", 10, "2.99", "getLine1Number").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getMsisdn", "READ_PHONE_STATE", 14, "2.99", "getMsisdn").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getIccSerialNumber", "READ_PHONE_STATE", 10, "2.99", "getSimSerialNumber").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getSubscriberId", "READ_PHONE_STATE", 10, "2.99", "getSubscriberId").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getVoiceMailAlphaTag", "READ_PHONE_STATE", 10, "2.99", "getVoiceMailAlphaTag").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getVoiceMailNumber", "READ_PHONE_STATE", 10, "2.99", "getVoiceMailNumber").AOSP(19).to(20));
		mListHook.add(new Hook("phone", "Srv_getCompleteVoiceMailNumber", "READ_PHONE_STATE", 10, "2.99", null).AOSP(19).to(20));

		mListHook.add(new Hook("phone", "Srv_getImei", "READ_PHONE_STATE", 21, "3.5.6", null).AOSP(21).obsolete());
		mListHook.add(new Hook("phone", "Srv_getIsimIst", "READ_PRIVILEGED_PHONE_STATE", 21, "3.5.6", null).AOSP(21).obsolete());
		mListHook.add(new Hook("phone", "Srv_getIsimPcscf", "READ_PRIVILEGED_PHONE_STATE", 21, "3.5.6", null).AOSP(21).obsolete());

		mListHook.add(new Hook("phone", "Srv_getCdmaMdn", "MODIFY_PHONE_STATE", 21, "3.5.6", null).AOSP(21));
		mListHook.add(new Hook("phone", "Srv_getCdmaMin", "MODIFY_PHONE_STATE", 21, "3.5.6", null).AOSP(21));
		mListHook.add(new Hook("phone", "Srv_getLine1AlphaTagForDisplay", "READ_PHONE_STATE", 21, "3.5.6", null).AOSP(21).obsolete());
		mListHook.add(new Hook("phone", "Srv_getLine1NumberForDisplay", "READ_PHONE_STATE", 21, "3.5.6", null).AOSP(21).obsolete());

		mListHook.add(new Hook("phone", "Srv_getCompleteVoiceMailNumberForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getCompleteVoiceMailNumber").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getDeviceId5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getDeviceId").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getDeviceIdForPhone5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getDeviceId").AOSP(Build.VERSION_CODES.LOLLIPOP_MR1));
		mListHook.add(new Hook("phone", "Srv_getDeviceIdForSubscriber5", "READ_PHONE_STATE", 21, "3.6.13", "Srv_getDeviceId").AOSP(Build.VERSION_CODES.LOLLIPOP).to(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getGroupIdLevel1ForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getGroupIdLevel1").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getIccSerialNumberForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getIccSerialNumber").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getImeiForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getImei").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getIsimDomain5", "READ_PRIVILEGED_PHONE_STATE", 21, "3.6.12", "Srv_getIsimDomain").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getIsimImpi5", "READ_PRIVILEGED_PHONE_STATE", 21, "3.6.12", "Srv_getIsimImpi").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getIsimImpu5", "READ_PRIVILEGED_PHONE_STATE", 21, "3.6.12", "Srv_getIsimImpu").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getIsimIst5", "READ_PRIVILEGED_PHONE_STATE", 21, "3.6.12", "Srv_getIsimIst").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getIsimPcscf5", "READ_PRIVILEGED_PHONE_STATE", 21, "3.6.12", "Srv_getIsimPcscf").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getLine1AlphaTagForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getLine1AlphaTagForDisplay").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getLine1NumberForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getLine1NumberForDisplay").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getMsisdnForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getMsisdn").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getNaiForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", null).AOSP(Build.VERSION_CODES.LOLLIPOP_MR1));
		mListHook.add(new Hook("phone", "Srv_getSubscriberIdForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getSubscriberId").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getVoiceMailAlphaTagForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getVoiceMailAlphaTag").AOSP(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook("phone", "Srv_getVoiceMailNumberForSubscriber5", "READ_PHONE_STATE", 21, "3.6.12", "Srv_getVoiceMailNumber").AOSP(Build.VERSION_CODES.LOLLIPOP));

		mListHook.add(new Hook("phone", "listen", "READ_PHONE_STATE", 10, null, null).notAOSP(19));
		mListHook.add(new Hook("phone", "Srv_listen", "READ_PHONE_STATE", 10, null, null).AOSP(19));

		mListHook.add(new Hook("phone", "getNetworkCountryIso", "", 10, null, null).unsafe());
		mListHook.add(new Hook("phone", "getNetworkOperator", "", 10, null, null).unsafe());
		mListHook.add(new Hook("phone", "getNetworkOperatorName", "", 10, null, null).unsafe());
		mListHook.add(new Hook("phone", "getSimCountryIso", "", 10, null, null).unsafe());
		mListHook.add(new Hook("phone", "getSimOperator", "", 10, null, null).unsafe());
		mListHook.add(new Hook("phone", "getSimOperatorName", "", 10, null, null).unsafe());

		mListHook.add(new Hook("phone", TelephonyManager.ACTION_PHONE_STATE_CHANGED, "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "TelephonyProvider", "WRITE_APN_SETTINGS", 1, null, null));

		mListHook.add(new Hook("phone", "Configuration.MCC", "", 1, "2.0", null).unsafe().noUsageData().noOnDemand());
		mListHook.add(new Hook("phone", "Configuration.MNC", "", 1, "2.0", null).unsafe().noUsageData().noOnDemand());

		mListHook.add(new Hook("sensors", "getDefaultSensor", "", 3, null, null).unsafe().dangerous());
		mListHook.add(new Hook("sensors", "getSensorList", "", 3, null, null).unsafe().dangerous());
		mListHook.add(new Hook("sensors", "registerListener", "", 3, "2.99.27", null).unsafe());
		mListHook.add(new Hook("sensors", "acceleration", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "gravity", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "humidity", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "light", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "magnetic", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "motion", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "orientation", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "pressure", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "proximity", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "rotation", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "temperature", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "step", "", 3, null, null).unsafe());
		mListHook.add(new Hook("sensors", "heartrate", "", 20, null, null).unsafe());

		mListHook.add(new Hook("shell", "sh", "", 10, null, null).unsafe().dangerous().whitelist(cTypeCommand));
		mListHook.add(new Hook("shell", "su", "", 10, null, null).unsafe().dangerous().whitelist(cTypeCommand));
		mListHook.add(new Hook("shell", "exec", "", 10, null, null).unsafe().dangerous().whitelist(cTypeCommand));
		mListHook.add(new Hook("shell", "load", "", 10, null, null).unsafe().dangerous().restart().whitelist(cTypeLibrary));
		mListHook.add(new Hook("shell", "loadLibrary", "", 10, null, null).unsafe().dangerous().restart().whitelist(cTypeLibrary));
		mListHook.add(new Hook("shell", "start", "", 10, null, null).unsafe().dangerous().whitelist(cTypeCommand));

		mListHook.add(new Hook("storage", "media", "WRITE_MEDIA_STORAGE", 10, null, null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("storage", "sdcard", "READ_EXTERNAL_STORAGE,WRITE_EXTERNAL_STORAGE,ACCESS_ALL_EXTERNAL_STORAGE", 10, null, null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("storage", "mtp", "ACCESS_MTP", 10, "2.1.1", null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("storage", "getExternalStorageState", null, 10, null, null).unsafe().whitelist(cTypeFilename));
		mListHook.add(new Hook("storage", "open", null, 1, "1.99.46", null).unsafe().dangerous().whitelist(cTypeFilename));

		mListHook.add(new Hook("storage", "openAssetFileDescriptor", null, 3, "2.1.17", null).unsafe().dangerous().whitelist(cTypeFilename));
		mListHook.add(new Hook("storage", "openFileDescriptor", null, 1, "2.1.17", null).unsafe().dangerous().whitelist(cTypeFilename));
		mListHook.add(new Hook("storage", "openInputStream", null, 1, "2.1.17", null).unsafe().dangerous().whitelist(cTypeFilename));
		mListHook.add(new Hook("storage", "openOutputStream", null, 1, "2.1.17", null).unsafe().dangerous().whitelist(cTypeFilename));
		mListHook.add(new Hook("storage", "openTypedAssetFileDescriptor", null, 11, "2.1.17", null).unsafe().dangerous().whitelist(cTypeFilename));
		mListHook.add(new Hook("storage", "openAssetFile", null, 5, "2.1.17", null).unsafe().dangerous().whitelist(cTypeFilename));
		mListHook.add(new Hook("storage", "openFile", null, 5, "2.1.17", null).unsafe().dangerous().whitelist(cTypeFilename));

		mListHook.add(new Hook("system", "getInstalledApplications", "", 1, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getInstalledPackages", "", 1, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getPackagesForUid", "", 1, "2.1.17", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getPackagesHoldingPermissions", "", 18, "1.99.1", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getPreferredActivities", "", 1, "1.99.44", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getPreferredPackages", "", 1, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "queryBroadcastReceivers", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "queryContentProviders", "", 1, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "queryIntentActivities", "", 1, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "queryIntentActivityOptions", "", 1, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "queryIntentContentProviders", "", 19, "1.99.1", null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "queryIntentServices", "", 1, null, null).notAOSP(19).dangerous());

		mListHook.add(new Hook("system", "Srv_getPackageInfo", "", 19, "2.99.30", null).AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getApplicationInfo", "", 19, "2.99.30", null).AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getInstalledApplications", "", 19, "2.99", "getInstalledApplications").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getInstalledPackages", "", 19, "2.99", "getInstalledPackages").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getPackagesForUid", "", 19, "2.99", "getPackagesForUid").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getPackagesHoldingPermissions", "", 19, "2.99", "getPackagesHoldingPermissions").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getPersistentApplications", "", 19, "2.99", null).AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getPreferredPackages", "", 19, "2.99", "getPreferredPackages").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_queryContentProviders", "", 19, "2.99", "queryContentProviders").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_queryIntentActivities", "", 19, "2.99", "queryIntentActivities").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_queryIntentActivityOptions", "", 19, "2.99", "queryIntentActivityOptions").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_queryIntentContentProviders", "", 19, "2.99", "queryIntentContentProviders").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_queryIntentReceivers", "", 19, "2.99", "queryBroadcastReceivers").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_queryIntentServices", "", 19, "2.99", "queryIntentServices").AOSP(19).dangerous());

		mListHook.add(new Hook("system", "getInstalledProviders", "", 3, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getInstalledProvidersForProfile", "", 21, "3.5.6", null).notAOSP(21).dangerous());
		mListHook.add(new Hook("system", "Srv_getInstalledProviders", "", 3, "2.99", "getInstalledProviders").AOSP(19).to(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getInstalledProvidersForProfile", "", 3, "3.6.6", null).AOSP(21).dangerous());

		mListHook.add(new Hook("system", "getRecentTasks", "GET_TASKS", 1, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getRunningAppProcesses", "", 3, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getRunningServices", "", 1, null, null).notAOSP(19).dangerous());
		mListHook.add(new Hook("system", "getRunningTasks", "GET_TASKS", 1, null, null).notAOSP(19).dangerous());

		mListHook.add(new Hook("system", "Srv_getRecentTasks", "GET_TASKS", 1, "2.99", "getRecentTasks").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getRunningAppProcesses", "", 3, "2.99", "getRunningAppProcesses").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getServices", "", 1, "2.99", "getRunningServices").AOSP(19).dangerous());
		mListHook.add(new Hook("system", "Srv_getTasks", "GET_TASKS", 1, "2.99", "getRunningTasks").AOSP(19).dangerous());

		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_ADDED, "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_REPLACED, "", 3, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_RESTARTED, "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_REMOVED, "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_CHANGED, "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_DATA_CLEARED, "", 3, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_FIRST_LAUNCH, "", 12, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_FULLY_REMOVED, "", 14, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_NEEDS_VERIFICATION, "", 14, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_PACKAGE_VERIFIED, "", 17, "2.2.2", null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_EXTERNAL_APPLICATIONS_AVAILABLE, "", 8, null, null).dangerous());
		mListHook.add(new Hook("system", Intent.ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE, "", 8, null, null).dangerous());

		mListHook.add(new Hook("system", "ApplicationsProvider", "", 1, null, null).to(18));

		mListHook.add(new Hook("system", "checkPermission", "", 1, "2.1.24", null).AOSP(19).dangerous().whitelist(cTypePermission));
		mListHook.add(new Hook("system", "checkUidPermission", "", 1, "2.1.24", null).AOSP(19).dangerous().whitelist(cTypePermission));

		mListHook.add(new Hook("system", "IntentFirewall", "", 19, "2.2.2", null).AOSP(19).dangerous().whitelist(cTypeAction));

		mListHook.add(new Hook("system", "queryAndAggregateUsageStats", null, 21, "3.5.6", null).notAOSP(21));
		mListHook.add(new Hook("system", "queryConfigurations", null, 21, "3.5.6", null).notAOSP(21));
		mListHook.add(new Hook("system", "queryEvents", null, 21, "3.5.6", null).notAOSP(21));
		mListHook.add(new Hook("system", "queryUsageStats", null, 21, "3.5.6", null).notAOSP(21));
		mListHook.add(new Hook("system", "Srv_queryConfigurationStats", null, 21, "3.5.6", null).AOSP(21));
		mListHook.add(new Hook("system", "Srv_queryEvents", null, 21, "3.5.6", null).AOSP(21));
		mListHook.add(new Hook("system", "Srv_queryUsageStats", null, 21, "3.5.6", null).AOSP(21));

		mListHook.add(new Hook("view", "loadUrl", "", 1, "3.6.2", "false").unsafe().whitelist(cTypeUrl));
		mListHook.add(new Hook("view", "postUrl", "", 1, "3.6.2", null).unsafe().whitelist(cTypeUrl));
		mListHook.add(new Hook("view", "initUserAgentString", "", 3, "3.6.2", null).unsafe());
		mListHook.add(new Hook("view", "getDefaultUserAgent", "", 17, null, null).unsafe());
		mListHook.add(new Hook("view", "getUserAgent", "", 3, null, null).unsafe());
		mListHook.add(new Hook("view", "getUserAgentString", "", 3, null, null).unsafe());
		mListHook.add(new Hook("view", "setUserAgent", "", 3, null, null).unsafe());
		mListHook.add(new Hook("view", "setUserAgentString", "", 3, null, null).unsafe());

		mListHook.add(new Hook("view", Intent.ACTION_VIEW, "", 1, null, null).notAOSP(19).doNotify().whitelist(cTypeUrl));
		mListHook.add(new Hook("view", "Srv_" + Intent.ACTION_VIEW, "", 19, "2.99", Intent.ACTION_VIEW).AOSP(19).doNotify().whitelist(cTypeUrl));

		mListHook.add(new Hook("view", "GMS5.view", "", 1, "2.99.27", null).unsafe());

		// AccountManager
		mListHook.add(new Hook(null, "removeOnAccountsUpdatedListener", "", 5, null, null));

		// Activity
		mListHook.add(new Hook(null, "startActivities", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "startActivity", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "startActivityForResult", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "startActivityFromChild", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "startActivityFromFragment", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "startActivityIfNeeded", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "startNextMatchingActivity", "", 1, null, null).notAOSP(19));

		// ActivityThread / MessageQueue
		mListHook.add(new Hook(null, "next", "", 1, null, null).notAOSP(19).optional());
		mListHook.add(new Hook(null, "handleReceiver", "", 1, null, null).notAOSP(19).optional());

		// ActivityManager(Service)
		mListHook.add(new Hook(null, "Srv_startActivities", "", 19, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_startActivity", "", 19, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_startActivityAsUser", "", 19, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_startActivityAsCaller", "", 21, null, null).AOSP(21));
		mListHook.add(new Hook(null, "Srv_startActivityAndWait", "", 19, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_startActivityWithConfig", "", 19, null, null).AOSP(19));

		mListHook.add(new Hook(null, "inputDispatchingTimedOut", "", 17, null, null));
		mListHook.add(new Hook(null, "appNotResponding", "", 15, null, null).optional());
		mListHook.add(new Hook(null, "systemReady", "", 15, null, null));
		mListHook.add(new Hook(null, "finishBooting", "", 15, null, null));
		mListHook.add(new Hook(null, "setLockScreenShown", "", 17, null, null).optional());
		mListHook.add(new Hook(null, "goingToSleep", "", 16, null, null).to(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook(null, "wakingUp", "", 16, null, null).to(Build.VERSION_CODES.LOLLIPOP));
		mListHook.add(new Hook(null, "updateSleepIfNeededLocked", "", Build.VERSION_CODES.LOLLIPOP_MR1, null, null));
		mListHook.add(new Hook(null, "shutdown", "", 15, null, null));
		mListHook.add(new Hook(null, "activityResumed", "", Build.VERSION_CODES.JELLY_BEAN_MR1, null, null));
		mListHook.add(new Hook(null, "activityPaused", "", Build.VERSION_CODES.JELLY_BEAN_MR1, null, null));

		// AppIndexApi
		mListHook.add(new Hook(null, "GMS5.viewEnd", "", 1, null, null));

		// Application
		mListHook.add(new Hook(null, "onCreate", "", 1, null, null));

		// AudioRecord
		mListHook.add(new Hook(null, "Audio.stop", "", 3, null, null));

		// Binder
		mListHook.add(new Hook(null, "execTransact", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "transact", "", 1, null, null).notAOSP(19));

		// ClipboardManager/Service
		mListHook.add(new Hook(null, "removePrimaryClipChangedListener", "", 11, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "Srv_removePrimaryClipChangedListener", "", 11, null, null).AOSP(19));

		// Content resolvers
		mListHook.add(new Hook(null, "query", "", 1, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "Srv_call", "", 1, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_query", "", 1, null, null).AOSP(19));

		// Camera
		mListHook.add(new Hook(null, "Camera.stopPreview", "", 1, null, null));

		// ContextImpl
		mListHook.add(new Hook(null, "getPackageManager", "", 1, null, null).notAOSP(19));

		// ContextImpl / Activity
		mListHook.add(new Hook(null, "getSystemService", "", 1, null, null).notAOSP(19));

		// FusedLocationProviderApi // ActivityRecognitionApi
		mListHook.add(new Hook(null, "GMS5.removeLocationUpdates", "", 1, "2.99.26", null).optional());
		mListHook.add(new Hook(null, "GMS5.removeActivityUpdates", "", 1, "2.99.26", null).optional());

		// GoogleApiClient.Builder
		mListHook.add(new Hook(null, "GMS5.addConnectionCallbacks", "", 1, null, null).optional());
		mListHook.add(new Hook(null, "GMS5.onConnected", "", 1, null, null));

		// IntentFirewall
		mListHook.add(new Hook(null, "checkIntent", "", 19, null, null));

		// LocationClient / ActivityRecognitionClient
		mListHook.add(new Hook(null, "GMS.removeActivityUpdates", "", 1, null, null));
		mListHook.add(new Hook(null, "GMS.removeGeofences", "", 1, null, null).optional());
		mListHook.add(new Hook(null, "GMS.removeLocationUpdates", "", 1, null, null).optional());

		// LocationManager/Service
		mListHook.add(new Hook(null, "removeUpdates", "", 3, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "Srv_removeUpdates", "", 19, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_removeGeofence", "", 19, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_removeGpsStatusListener", "", 19, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_removeGpsMeasurementsListener", "", 21, null, null).AOSP(21));
		mListHook.add(new Hook(null, "Srv_removeGpsNavigationMessageListener", "", 21, null, null).AOSP(21));
		mListHook.add(new Hook(null, "MapV1.disableMyLocation", "", 1, null, null).optional());

		// MediaRecorder
		mListHook.add(new Hook(null, "MediaRecorder.prepare", "", 1, null, null));
		mListHook.add(new Hook(null, "MediaRecorder.stop", "", 1, null, null));

		// Resources
		mListHook.add(new Hook(null, "updateConfiguration", "", 1, null, null));

		// TelephonyManager
		mListHook.add(new Hook(null, "disableLocationUpdates", "", 10, null, null).notAOSP(19));
		mListHook.add(new Hook(null, "Srv_disableLocationUpdates", "", 19, null, null).AOSP(19));
		mListHook.add(new Hook(null, "Srv_disableLocationUpdatesForSubscriber", "", 21, null, null).AOSP(21));

		// UtilHook
		mListHook.add(new Hook(null, "isXposedEnabled", "", 15, null, null));

		// WebView
		mListHook.add(new Hook(null, "WebView", "", 1, null, null));
		mListHook.add(new Hook(null, "getSettings", "", 1, null, null));

		// WindowManagerImpl
		mListHook.add(new Hook(null, "removeView", "", 1, null, null).optional());
		mListHook.add(new Hook(null, "updateViewLayout", "", 1, null, null).optional());

		// @formatter:on
		return mListHook;
	}

	public static void annotate(Resources resources) {
		if (mAnnotated)
			return;

		String self = Meta.class.getPackage().getName();
		for (Hook hook : get())
			if (hook.getRestrictionName() != null) {
				String name = hook.getRestrictionName() + "_" + hook.getName();
				name = name.replace(".", "_").replace("/", "_").replace("%", "_").replace("-", "_");
				int resId = resources.getIdentifier(name, "string", self);
				if (resId > 0)
					hook.annotate(resources.getString(resId));
				else
					Util.log(null, Log.WARN, "Missing annotation hook=" + hook);
			}

		mAnnotated = true;
	}
}
package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;

public class Meta {
	private static boolean mAnnotated = false;
	private static List<Hook> mListHook = new ArrayList<Hook>();

	public static List<Hook> get() {
		// http://developer.android.com/reference/android/Manifest.permission.html
		if (mListHook.size() > 0)
			return mListHook;

		// @formatter:off
		mListHook.add(new Hook("accounts", "addOnAccountsUpdatedListener", "GET_ACCOUNTS", 10, null, null));
		mListHook.add(new Hook("accounts", "blockingGetAuthToken", "USE_CREDENTIALS", 10, null, null));
		mListHook.add(new Hook("accounts", "getAccounts", "GET_ACCOUNTS", 10, null, null));
		mListHook.add(new Hook("accounts", "getAccountsByType", "GET_ACCOUNTS", 10, null, null));
		mListHook.add(new Hook("accounts", "getAccountsByTypeAndFeatures", "GET_ACCOUNTS", 10, null, null));
		mListHook.add(new Hook("accounts", "getAuthToken", "USE_CREDENTIALS", 10, null, null));
		mListHook.add(new Hook("accounts", "getAuthTokenByFeatures", "MANAGE_ACCOUNTS", 10, null, null));
		mListHook.add(new Hook("accounts", "hasFeatures", "GET_ACCOUNTS", 10, null, null));
		mListHook.add(new Hook("accounts", "getAccountsByTypeForPackage", "GET_ACCOUNTS", 18, null, null));
		mListHook.add(new Hook("accounts", "getTokenGoogle", "GET_ACCOUNTS", 10, null, null));
		mListHook.add(new Hook("accounts", "getTokenWithNotificationGoogle", "GET_ACCOUNTS", 10, null, null));

		mListHook.add(new Hook("accounts", "getAuthenticatorTypes", "GET_ACCOUNTS", 5, "1.99.24", null).dangerous());
		mListHook.add(new Hook("accounts", "getCurrentSync", "GET_ACCOUNTS", 8, "1.99.24", null).dangerous());
		mListHook.add(new Hook("accounts", "getCurrentSyncs", "GET_ACCOUNTS", 11, "1.99.24", null).dangerous());
		mListHook.add(new Hook("accounts", "getSyncAdapterTypes", "GET_ACCOUNTS", 5, "1.99.24", null).dangerous());

		mListHook.add(new Hook("browser", "BrowserProvider2", "com.android.browser.permission.READ_HISTORY_BOOKMARKS,GLOBAL_SEARCH", 1, null, null));
		mListHook.add(new Hook("browser", "Downloads", "ACCESS_DOWNLOAD_MANAGER,ACCESS_DOWNLOAD_MANAGER_ADVANCED,ACCESS_ALL_DOWNLOADS", 1, null, null));

		mListHook.add(new Hook("calendar", "CalendarProvider2", "READ_CALENDAR", 1, null, null));

		mListHook.add(new Hook("calling", "sendDataMessage", "SEND_SMS", 4, null, null).doNotify());
		mListHook.add(new Hook("calling", "sendMultipartTextMessage", "SEND_SMS", 4, null, null).doNotify());
		mListHook.add(new Hook("calling", "sendTextMessage", "SEND_SMS", 4, null, null).doNotify());
		mListHook.add(new Hook("calling", "android.intent.action.RESPOND_VIA_MESSAGE", "SEND_RESPOND_VIA_MESSAGE", 18, null, null).doNotify());
		mListHook.add(new Hook("calling", "android.intent.action.CALL", "CALL_PHONE", 10, null, null).doNotify());

		mListHook.add(new Hook("clipboard", "addPrimaryClipChangedListener", "", 11, null, null));
		mListHook.add(new Hook("clipboard", "getPrimaryClip", "", 11, null, null).doNotify());
		mListHook.add(new Hook("clipboard", "getPrimaryClipDescription", "", 11, null, null));
		mListHook.add(new Hook("clipboard", "getText", "", 10, null, null).doNotify());
		mListHook.add(new Hook("clipboard", "hasPrimaryClip", "", 11, null, null));
		mListHook.add(new Hook("clipboard", "hasText", "", 10, null, null));
		mListHook.add(new Hook("clipboard", "removePrimaryClipChangedListener", "", 11, null, null));

		mListHook.add(new Hook("contacts", "contacts/contacts", "READ_CONTACTS", 1, null, null));
		mListHook.add(new Hook("contacts", "contacts/data", "READ_CONTACTS", 1, null, null));
		mListHook.add(new Hook("contacts", "contacts/phone_lookup", "READ_CONTACTS", 1, null, null));
		mListHook.add(new Hook("contacts", "contacts/profile", "READ_PROFILE", 1, "1.99.38", null).dangerous());
		mListHook.add(new Hook("contacts", "ContactsProvider2", "READ_CONTACTS,READ_PROFILE", 1, "1.99.38", null).dangerous());
		mListHook.add(new Hook("contacts", "IccProvider", "", 1, "1.99.38", null));

		mListHook.add(new Hook("dictionary", "UserDictionary", "READ_USER_DICTIONARY", 1, null, null));

		mListHook.add(new Hook("email", "EMailProvider", "com.android.email.permission.ACCESS_PROVIDER", 1, null, null));
		mListHook.add(new Hook("email", "GMailProvider", "com.google.android.gm.permission.READ_CONTENT_PROVIDER", 8, "1.99.20", null));

		mListHook.add(new Hook("identification", "%hostname", "", 10, null, null));
		mListHook.add(new Hook("identification", "%imei", "", 10, null, null));
		mListHook.add(new Hook("identification", "%macaddr", "", 10, null, null));
		mListHook.add(new Hook("identification", "%serialno", "", 10, null, null));
		mListHook.add(new Hook("identification", "%cid", "", 10, null, null));
		mListHook.add(new Hook("identification", "/proc", "", 10, "1.7", null).dangerous());
		mListHook.add(new Hook("identification", "/system/build.prop", "", 10, "1.9.9", null).dangerous());
		mListHook.add(new Hook("identification", "/sys/block/.../cid", "", 10, null, null));
		mListHook.add(new Hook("identification", "/sys/class/.../cid", "", 10, null, null));
		mListHook.add(new Hook("identification", "AdvertisingId", "", 10, null, null));
		mListHook.add(new Hook("identification", "getString", "", 10, null, null));
		mListHook.add(new Hook("identification", "getDescriptor", "", 16, null, null));
		mListHook.add(new Hook("identification", "GservicesProvider", "READ_GSERVICES", 10, null, null).dangerous());
		mListHook.add(new Hook("identification", "SERIAL", "", 10, null, null).restart().noUsageData());

		mListHook.add(new Hook("internet", "getAllByName", "INTERNET", 10, null, null));
		mListHook.add(new Hook("internet", "getByAddress", "INTERNET", 10, null, null));
		mListHook.add(new Hook("internet", "getByName", "INTERNET", 10, null, null));
		mListHook.add(new Hook("internet", "getByInetAddress", "INTERNET", 10, null, null));
		mListHook.add(new Hook("internet", "getNetworkInterfaces", "INTERNET", 10, null, null));
		mListHook.add(new Hook("internet", "inet", "INTERNET", 10, null, null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("internet", "getActiveNetworkInfo", null, 10, null, null).dangerous());
		mListHook.add(new Hook("internet", "getAllNetworkInfo", null, 10, null, null));
		mListHook.add(new Hook("internet", "getNetworkInfo", null, 10, null, null).dangerous());

		mListHook.add(new Hook("internet", "getDetailedState", null, 1, null, null));
		mListHook.add(new Hook("internet", "getExtraInfo", null, 1, null, null));
		mListHook.add(new Hook("internet", "getState", null, 1, null, null));
		mListHook.add(new Hook("internet", "isConnected", null, 1, null, null));
		mListHook.add(new Hook("internet", "isConnectedOrConnecting", null, 1, null, null));

		mListHook.add(new Hook("internet", "getConnectionInfo", null, 10, null, null));

		mListHook.add(new Hook("ipc", "android.accounts.IAccountManager", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "android.app.IActivityManager", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "android.content.IClipboard", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "android.net.IConnectivityManager", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "android.content.IContentService", "", 1, "1.99.40", null));
		mListHook.add(new Hook("ipc", "android.location.ILocationManager", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "com.android.internal.telephony.ITelephonyRegistry", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "com.android.internal.telephony.ITelephonyRegistryMSim", "", 1, "1.99.26", null));
		mListHook.add(new Hook("ipc", "android.content.pm.IPackageManager", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "com.android.internal.telephony.IPhoneSubInfo", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "com.android.internal.telephony.msim.IPhoneSubInfoMSim", "", 1, "1.99.26", null));
		mListHook.add(new Hook("ipc", "android.view.IWindowManager", "", 1, "1.99.1", null));
		mListHook.add(new Hook("ipc", "android.net.wifi.IWifiManager", "", 1, "1.99.1", null));

		mListHook.add(new Hook("location", "addGeofence", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 17, null, null));
		mListHook.add(new Hook("location", "addNmeaListener", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 5, null, null));
		mListHook.add(new Hook("location", "addProximityAlert", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null));
		mListHook.add(new Hook("location", "getGpsStatus", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 3, "1.99.29", null));
		mListHook.add(new Hook("location", "getLastKnownLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null));
		mListHook.add(new Hook("location", "getProviders", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "1.99.1", null).dangerous());
		mListHook.add(new Hook("location", "isProviderEnabled", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, "1.99.1", null).dangerous());
		mListHook.add(new Hook("location", "requestLocationUpdates", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 1, null, null));
		mListHook.add(new Hook("location", "requestSingleUpdate", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 9, null, null));
		mListHook.add(new Hook("location", "sendExtraCommand", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 3, null, null));

		mListHook.add(new Hook("location", "disableLocationUpdates", "CONTROL_LOCATION_UPDATES", 10, null, null));
		mListHook.add(new Hook("location", "enableLocationUpdates", "CONTROL_LOCATION_UPDATES", 10, null, null));
		mListHook.add(new Hook("location", "getCellLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		mListHook.add(new Hook("location", "getNeighboringCellInfo", "ACCESS_COARSE_UPDATES", 10, null, null));
		mListHook.add(new Hook("location", "getAllCellInfo", "ACCESS_COARSE_UPDATES", 17, null, null));
		mListHook.add(new Hook("location", "getScanResults", "ACCESS_WIFI_STATE", 10, null, null).dangerous());
		mListHook.add(new Hook("location", "listen", "ACCESS_COARSE_LOCATION", 10, null, null));
		mListHook.add(new Hook("location", "GMS.addGeofences", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));
		mListHook.add(new Hook("location", "GMS.getLastLocation", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));
		mListHook.add(new Hook("location", "GMS.removeGeofences", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));
		mListHook.add(new Hook("location", "GMS.requestLocationUpdates", "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));

		mListHook.add(new Hook("media", "startRecording", "RECORD_AUDIO", 10, null, null).doNotify());
		mListHook.add(new Hook("media", "setPreviewCallback", "CAMERA", 10, null, null).doNotify());
		mListHook.add(new Hook("media", "setPreviewCallbackWithBuffer", "CAMERA", 10, null, null).doNotify());
		mListHook.add(new Hook("media", "setOneShotPreviewCallback", "CAMERA", 10, null, null).doNotify());
		mListHook.add(new Hook("media", "takePicture", "CAMERA", 10, null, null).doNotify());
		mListHook.add(new Hook("media", "setOutputFile", "RECORD_AUDIO,RECORD_VIDEO", 10, null, null).doNotify());
		mListHook.add(new Hook("media", "android.media.action.IMAGE_CAPTURE", "CAMERA", 10, null, null).doNotify());
		mListHook.add(new Hook("media", "android.media.action.IMAGE_CAPTURE_SECURE", "CAMERA", 17, null, null).doNotify());
		mListHook.add(new Hook("media", "android.media.action.VIDEO_CAPTURE", "CAMERA", 10, null, null).doNotify());

		mListHook.add(new Hook("messages", "getAllMessagesFromIcc", "RECEIVE_SMS", 10, null, null));
		mListHook.add(new Hook("messages", "SmsProvider", "READ_SMS", 1, null, null));
		mListHook.add(new Hook("messages", "MmsProvider", "READ_SMS", 1, null, null));
		mListHook.add(new Hook("messages", "MmsSmsProvider", "READ_SMS", 1, null, null));
		mListHook.add(new Hook("messages", "VoicemailContentProvider", "com.android.voicemail.permission.READ_WRITE_ALL_VOICEMAIL", 1, null, null));
		mListHook.add(new Hook("messages", "android.intent.action.DATA_SMS_RECEIVED", "RECEIVE_SMS", 19, null, null));
		mListHook.add(new Hook("messages", "android.provider.Telephony.SMS_RECEIVED", "RECEIVE_SMS", 19, null, null));
		mListHook.add(new Hook("messages", "android.provider.Telephony.WAP_PUSH_RECEIVED", "RECEIVE_WAP_PUSH", 19, null, null));

		mListHook.add(new Hook("network", "getAddress", "android.permission.BLUETOOTH", 5, null, null));
		mListHook.add(new Hook("network", "getBondedDevices", "android.permission.BLUETOOTH", 5, null, null));
		mListHook.add(new Hook("network", "getHardwareAddress", "ACCESS_NETWORK_STATE", 10, null, null));
		mListHook.add(new Hook("network", "getInetAddresses", "ACCESS_NETWORK_STATE", 10, null, null));
		mListHook.add(new Hook("network", "getInterfaceAddresses", "ACCESS_NETWORK_STATE", 10, null, null));
		mListHook.add(new Hook("network", "getConfiguredNetworks", "ACCESS_WIFI_STATE", 10, null, null));
		mListHook.add(new Hook("network", "getConnectionInfo", "ACCESS_WIFI_STATE", 10, null, null));
		mListHook.add(new Hook("network", "getDhcpInfo", "ACCESS_WIFI_STATE", 10, null, null));
		mListHook.add(new Hook("network", "getScanResults", "ACCESS_WIFI_STATE", 10, null, null));
		mListHook.add(new Hook("network", "getWifiApConfiguration", "ACCESS_WIFI_STATE", 10, null, null));

		mListHook.add(new Hook("nfc", "getNfcAdapter", "android.permission.NFC", 14, null, null));
		mListHook.add(new Hook("nfc", "getDefaultAdapter", "android.permission.NFC", 10, null, null));
		mListHook.add(new Hook("nfc", "android.nfc.action.ADAPTER_STATE_CHANGED", "android.permission.NFC", 18, null, null));
		mListHook.add(new Hook("nfc", "android.nfc.action.NDEF_DISCOVERED", "android.permission.NFC", 10, null, null));
		mListHook.add(new Hook("nfc", "android.nfc.action.TAG_DISCOVERED", "android.permission.NFC", 10, null, null));
		mListHook.add(new Hook("nfc", "android.nfc.action.TECH_DISCOVERED", "android.permission.NFC", 10, null, null));

		mListHook.add(new Hook("notifications", "android.service.notification.NotificationListenerService", "BIND_NOTIFICATION_LISTENER_SERVICE", 18, null, null));
		mListHook.add(new Hook("notifications", "com.google.android.c2dm.intent.REGISTRATION", "com.google.android.c2dm.permission.RECEIVE", 10, null, null).dangerous());
		mListHook.add(new Hook("notifications", "com.google.android.c2dm.intent.RECEIVE", "com.google.android.c2dm.permission.RECEIVE", 10, null, null).dangerous());

		mListHook.add(new Hook("overlay", "addView", "SYSTEM_ALERT_WINDOW", 1, null, null));
		mListHook.add(new Hook("overlay", "removeView", "SYSTEM_ALERT_WINDOW", 1, null, null));
		mListHook.add(new Hook("overlay", "updateViewLayout", "SYSTEM_ALERT_WINDOW", 1, null, null));

		mListHook.add(new Hook("phone", "getDeviceId", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "getIsimDomain", "READ_PRIVILEGED_PHONE_STATE", 14, null, null));
		mListHook.add(new Hook("phone", "getIsimImpi", "READ_PRIVILEGED_PHONE_STATE", 14, null, null));
		mListHook.add(new Hook("phone", "getIsimImpu", "READ_PRIVILEGED_PHONE_STATE", 14, null, null));
		mListHook.add(new Hook("phone", "getLine1AlphaTag", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "getLine1Number", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "getMsisdn", "READ_PHONE_STATE", 14, null, null));
		mListHook.add(new Hook("phone", "getSimSerialNumber", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "getSubscriberId", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "getVoiceMailAlphaTag", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "getVoiceMailNumber", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "listen", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "getNetworkCountryIso", "", 10, null, null));
		mListHook.add(new Hook("phone", "getNetworkOperator", "", 10, null, null));
		mListHook.add(new Hook("phone", "getNetworkOperatorName", "", 10, null, null));
		mListHook.add(new Hook("phone", "getNetworkType", "", 10, null, null));
		mListHook.add(new Hook("phone", "getPhoneType", "", 10, null, null));
		mListHook.add(new Hook("phone", "getSimCountryIso", "", 10, null, null));
		mListHook.add(new Hook("phone", "getSimOperator", "", 10, null, null));
		mListHook.add(new Hook("phone", "getSimOperatorName", "", 10, null, null));
		mListHook.add(new Hook("phone", "getGroupIdLevel1", "READ_PHONE_STATE", 18, null, null));
		mListHook.add(new Hook("phone", "android.intent.action.NEW_OUTGOING_CALL", "PROCESS_OUTGOING_CALLS", 10, null, null));
		mListHook.add(new Hook("phone", "android.intent.action.PHONE_STATE", "READ_PHONE_STATE", 10, null, null));
		mListHook.add(new Hook("phone", "TelephonyProvider", "WRITE_APN_SETTINGS", 1, null, null));
		mListHook.add(new Hook("phone", "CallLogProvider", "READ_CALL_LOG", 1, null, null));
		mListHook.add(new Hook("phone", "gsm.operator.iso-country", "", 10, "1.99.1", null));
		mListHook.add(new Hook("phone", "gsm.operator.numeric", "", 10, "1.99.1", null));
		mListHook.add(new Hook("phone", "gsm.operator.alpha", "", 10, "1.99.1", null));
		mListHook.add(new Hook("phone", "gsm.current.phone-type", "", 10, "1.99.1", null));
		mListHook.add(new Hook("phone", "gsm.sim.operator.iso-country", "", 10, "1.99.1", null));
		mListHook.add(new Hook("phone", "gsm.sim.operator.numeric", "", 10, "1.99.1", null));
		mListHook.add(new Hook("phone", "gsm.sim.operator.alpha", "", 10, "1.99.1", null));
		mListHook.add(new Hook("phone", "Configuration.MCC", "", 1, "2.0", null).noUsageData().noOnDemand());
		mListHook.add(new Hook("phone", "Configuration.MNC", "", 1, "2.0", null).noUsageData().noOnDemand());

		mListHook.add(new Hook("sensors", "getDefaultSensor", "", 3, null, null));
		mListHook.add(new Hook("sensors", "getSensorList", "", 3, null, null));

		mListHook.add(new Hook("shell", "sh", "", 10, null, null));
		mListHook.add(new Hook("shell", "su", "", 10, null, null));
		mListHook.add(new Hook("shell", "exec", "", 10, null, null));
		mListHook.add(new Hook("shell", "load", "", 10, null, null).dangerous());
		mListHook.add(new Hook("shell", "loadLibrary", "", 10, null, null).dangerous());
		mListHook.add(new Hook("shell", "start", "", 10, null, null));

		mListHook.add(new Hook("storage", "media", "WRITE_MEDIA_STORAGE", 10, null, null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("storage", "sdcard", "READ_EXTERNAL_STORAGE,WRITE_EXTERNAL_STORAGE", 10, null, null).dangerous().restart().noUsageData());
		mListHook.add(new Hook("storage", "getExternalStorageState", null, 10, null, null));

		mListHook.add(new Hook("system", "getInstalledApplications", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "getInstalledPackages", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "getPackagesHoldingPermissions", "", 18, "1.99.1", null).dangerous());
		mListHook.add(new Hook("system", "getPreferredPackages", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "queryBroadcastReceivers", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "queryContentProviders", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "queryIntentActivities", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "queryIntentActivityOptions", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "queryIntentContentProviders", "", 19, "1.99.1", null).dangerous());
		mListHook.add(new Hook("system", "queryIntentServices", "", 1, null, null).dangerous());
		mListHook.add(new Hook("system", "getInstalledProviders", "", 3, null, null).dangerous());
		mListHook.add(new Hook("system", "getRecentTasks", "GET_TASKS", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "getRunningAppProcesses", "GET_TASKS", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "getRunningServices", "GET_TASKS", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "getRunningTasks", "GET_TASKS", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_ADDED", "", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_REPLACED", "", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_RESTARTED", "", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_REMOVED", "", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_CHANGED", "", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_DATA_CLEARED", "", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_FIRST_LAUNCH", "", 12, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_FULLY_REMOVED", "", 14, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_NEEDS_VERIFICATION", "", 14, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.PACKAGE_VERIFIED", "", 17, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.EXTERNAL_APPLICATIONS_AVAILABLE", "", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "android.intent.action.EXTERNAL_APPLICATIONS_UNAVAILABLE", "", 10, null, null).dangerous());
		mListHook.add(new Hook("system", "ApplicationsProvider", "", 1, null, null));

		mListHook.add(new Hook("view", "loadUrl", "", 1, null, null));
		mListHook.add(new Hook("view", "WebView", "", 1, null, null));
		mListHook.add(new Hook("view", "getDefaultUserAgent", "", 17, null, null));
		mListHook.add(new Hook("view", "getUserAgent", "", 1, null, null));
		mListHook.add(new Hook("view", "getUserAgentString", "", 1, null, null));
		mListHook.add(new Hook("view", "setUserAgent", "", 1, null, null));
		mListHook.add(new Hook("view", "setUserAgentString", "", 1, null, null));
		mListHook.add(new Hook("view", "android.intent.action.VIEW", "", 10, null, null).doNotify());
		// @formatter:on
		return mListHook;
	}

	public static void annotate(Context context) {
		if (mAnnotated)
			return;

		String self = Meta.class.getPackage().getName();
		for (Hook hook : get()) {
			String name = hook.getRestrictionName() + "_" + hook.getName();
			int resId = context.getResources().getIdentifier(name, "string", self);
			if (resId > 0)
				hook.annotate(context.getString(resId));
		}

		mAnnotated = true;
	}
}
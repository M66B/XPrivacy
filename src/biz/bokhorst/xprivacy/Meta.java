package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

public class Meta {
	public static List<Hook> get() {
		List<Hook> listHook = new ArrayList<Hook>();
		// @formatter:off
		listHook.add(new Hook("accounts", "addOnAccountsUpdatedListener", false, false, "GET_ACCOUNTS", 10, null, null));
		listHook.add(new Hook("accounts", "blockingGetAuthToken", false, false, "USE_CREDENTIALS", 10, null, null));
		listHook.add(new Hook("accounts", "getAccounts", false, false, "GET_ACCOUNTS", 10, null, null));
		listHook.add(new Hook("accounts", "getAccountsByType", false, false, "GET_ACCOUNTS", 10, null, null));
		listHook.add(new Hook("accounts", "getAccountsByTypeAndFeatures", false, false, "GET_ACCOUNTS", 10, null, null));
		listHook.add(new Hook("accounts", "getAuthToken", false, false, "USE_CREDENTIALS", 10, null, null));
		listHook.add(new Hook("accounts", "getAuthTokenByFeatures", false, false, "MANAGE_ACCOUNTS", 10, null, null));
		listHook.add(new Hook("accounts", "hasFeatures", false, false, "GET_ACCOUNTS", 10, null, null));
		listHook.add(new Hook("accounts", "removeOnAccountsUpdatedListener", false, false, "GET_ACCOUNTS", 10, null, null));
		listHook.add(new Hook("accounts", "getAccountsByTypeForPackage", false, false, "GET_ACCOUNTS", 18, null, null));
		listHook.add(new Hook("accounts", "getTokenGoogle", false, false, "GET_ACCOUNTS", 10, null, null));
		listHook.add(new Hook("accounts", "getTokenWithNotificationGoogle", false, false, "GET_ACCOUNTS", 10, null, null));

		listHook.add(new Hook("accounts", "getAuthenticatorTypes", true, false, "GET_ACCOUNTS", 5, "1.99.24", null));
		listHook.add(new Hook("accounts", "getCurrentSync", true, false, "GET_ACCOUNTS", 8, "1.99.24", null));
		listHook.add(new Hook("accounts", "getCurrentSyncs", true, false, "GET_ACCOUNTS", 11, "1.99.24", null));
		listHook.add(new Hook("accounts", "getSyncAdapterTypes", true, false, "GET_ACCOUNTS", 5, "1.99.24", null));

		listHook.add(new Hook("browser", "BrowserProvider", false, false, "READ_HISTORY_BOOKMARKS,GLOBAL_SEARCH", 10, null, null));
		listHook.add(new Hook("browser", "BrowserProvider2", false, false, "READ_HISTORY_BOOKMARKS,GLOBAL_SEARCH", 14, null, null));

		listHook.add(new Hook("calendar", "CalendarProvider2", false, false, "READ_CALENDAR", 10, null, null));

		listHook.add(new Hook("calling", "sendDataMessage", false, false, "SEND_SMS", 4, null, null));
		listHook.add(new Hook("calling", "sendMultipartTextMessage", false, false, "SEND_SMS", 4, null, null));
		listHook.add(new Hook("calling", "sendTextMessage", false, false, "SEND_SMS", 4, null, null));
		listHook.add(new Hook("calling", "android.intent.action.RESPOND_VIA_MESSAGE", false, false, "SEND_RESPOND_VIA_MESSAGE", 18, null, null));
		listHook.add(new Hook("calling", "android.intent.action.CALL", false, false, "CALL_PHONE", 10, null, null));

		listHook.add(new Hook("clipboard", "addPrimaryClipChangedListener", false, false, "", 11, null, null));
		listHook.add(new Hook("clipboard", "getPrimaryClip", false, false, "", 11, null, null));
		listHook.add(new Hook("clipboard", "getPrimaryClipDescription", false, false, "", 11, null, null));
		listHook.add(new Hook("clipboard", "getText", false, false, "", 10, null, null));
		listHook.add(new Hook("clipboard", "hasPrimaryClip", false, false, "", 11, null, null));
		listHook.add(new Hook("clipboard", "hasText", false, false, "", 10, null, null));
		listHook.add(new Hook("clipboard", "removePrimaryClipChangedListener", false, false, "", 11, null, null));

		listHook.add(new Hook("contacts", "contacts/contacts", false, false, "READ_CONTACTS", 10, null, null));
		listHook.add(new Hook("contacts", "contacts/data", false, false, "READ_CONTACTS", 10, null, null));
		listHook.add(new Hook("contacts", "contacts/raw_contacts", false, false, "READ_CONTACTS", 10, null, null));
		listHook.add(new Hook("contacts", "contacts/phone_lookup", false, false, "READ_CONTACTS", 10, null, null));
		listHook.add(new Hook("contacts", "contacts/profile", false, false, "READ_CONTACTS", 10, null, null));

		listHook.add(new Hook("dictionary", "UserDictionary", false, false, "READ_USER_DICTIONARY", 10, null, null));

		listHook.add(new Hook("email", "EMailProvider", false, false, "com.android.email.permission.ACCESS_PROVIDER", 10, null, null));
		listHook.add(new Hook("email", "GMailProvider", false, false, "com.google.android.gm.permission.READ_CONTENT_PROVIDER", 8, "1.99.20", null));

		listHook.add(new Hook("identification", "%hostname", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "%imei", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "%macaddr", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "%serialno", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "%cid", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "/proc", true, false, "", 10, "1.7", null));
		listHook.add(new Hook("identification", "/system/build.prop", true, false, "", 10, "1.9.9", null));
		listHook.add(new Hook("identification", "/sys/block/.../cid", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "/sys/class/.../cid", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "AdvertisingId", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "getString", false, false, "", 10, null, null));
		listHook.add(new Hook("identification", "getDescriptor", false, false, "", 16, null, null));
		listHook.add(new Hook("identification", "GservicesProvider", true, false, "READ_GSERVICES", 10, null, null));
		listHook.add(new Hook("identification", "SERIAL", false, true, "", 10, null, null).noUsageData());

		listHook.add(new Hook("internet", "getAllByName", false, false, "INTERNET", 10, null, null));
		listHook.add(new Hook("internet", "getByAddress", false, false, "INTERNET", 10, null, null));
		listHook.add(new Hook("internet", "getByName", false, false, "INTERNET", 10, null, null));
		listHook.add(new Hook("internet", "getByInetAddress", false, false, "INTERNET", 10, null, null));
		listHook.add(new Hook("internet", "getNetworkInterfaces", false, false, "INTERNET", 10, null, null));
		listHook.add(new Hook("internet", "inet", true, true, "INTERNET", 10, null, null).noUsageData());
		listHook.add(new Hook("internet", "getActiveNetworkInfo", true, false, null, 10, null, null));
		listHook.add(new Hook("internet", "getAllNetworkInfo", false, false, null, 10, null, null));
		listHook.add(new Hook("internet", "getNetworkInfo", true, false, null, 10, null, null));

		listHook.add(new Hook("internet", "getDetailedState", false, false, null, 1, null, null));
		listHook.add(new Hook("internet", "getExtraInfo", false, false, null, 1, null, null));
		listHook.add(new Hook("internet", "getState", false, false, null, 1, null, null));
		listHook.add(new Hook("internet", "isConnected", false, false, null, 1, null, null));
		listHook.add(new Hook("internet", "isConnectedOrConnecting", false, false, null, 1, null, null));

		listHook.add(new Hook("internet", "getConnectionInfo", false, false, null, 10, null, null));

		listHook.add(new Hook("ipc", "android.accounts.IAccountManager", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "android.app.IActivityManager", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "android.content.IClipboard", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "android.net.IConnectivityManager", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "android.location.ILocationManager", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "com.android.internal.telephony.ITelephonyRegistry", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "com.android.internal.telephony.ITelephonyRegistryMSim", false, false, "", 1, "1.99.26", null));
		listHook.add(new Hook("ipc", "android.content.pm.IPackageManager", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "com.android.internal.telephony.IPhoneSubInfo", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "com.android.internal.telephony.msim.IPhoneSubInfoMSim", false, false, "", 1, "1.99.26", null));
		listHook.add(new Hook("ipc", "android.view.IWindowManager", false, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("ipc", "android.net.wifi.IWifiManager", false, false, "", 1, "1.99.1", null));

		listHook.add(new Hook("location", "addNmeaListener", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "addProximityAlert", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "getLastKnownLocation", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "getProviders", true, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, "1.99.1", null));
		listHook.add(new Hook("location", "isProviderEnabled", true, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, "1.99.1", null));
		listHook.add(new Hook("location", "removeUpdates", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "requestLocationUpdates", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "requestSingleUpdate", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "sendExtraCommand", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "addGeofence", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 17, null, null));
		listHook.add(new Hook("location", "getLastLocation", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 17, null, null));
		listHook.add(new Hook("location", "disableLocationUpdates", false, false, "CONTROL_LOCATION_UPDATES", 10, null, null));
		listHook.add(new Hook("location", "enableLocationUpdates", false, false, "CONTROL_LOCATION_UPDATES", 10, null, null));
		listHook.add(new Hook("location", "getCellLocation", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "getNeighboringCellInfo", false, false, "ACCESS_COARSE_UPDATES", 10, null, null));
		listHook.add(new Hook("location", "getAllCellInfo", false, false, "ACCESS_COARSE_UPDATES", 17, null, null));
		listHook.add(new Hook("location", "getScanResults", true, false, "ACCESS_WIFI_STATE", 10, null, null));
		listHook.add(new Hook("location", "listen", false, false, "ACCESS_COARSE_LOCATION", 10, null, null));
		listHook.add(new Hook("location", "GMS.addGeofences", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));
		listHook.add(new Hook("location", "GMS.getLastLocation", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));
		listHook.add(new Hook("location", "GMS.removeGeofences", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));
		listHook.add(new Hook("location", "GMS.removeLocationUpdates", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));
		listHook.add(new Hook("location", "GMS.requestLocationUpdates", false, false, "ACCESS_COARSE_LOCATION,ACCESS_FINE_LOCATION", 0, null, null));

		listHook.add(new Hook("media", "startRecording", false, false, "RECORD_AUDIO", 10, null, null));
		listHook.add(new Hook("media", "setPreviewCallback", false, false, "CAMERA", 10, null, null));
		listHook.add(new Hook("media", "setPreviewCallbackWithBuffer", false, false, "CAMERA", 10, null, null));
		listHook.add(new Hook("media", "setOneShotPreviewCallback", false, false, "CAMERA", 10, null, null));
		listHook.add(new Hook("media", "takePicture", false, false, "CAMERA", 10, null, null));
		listHook.add(new Hook("media", "setOutputFile", false, false, "RECORD_AUDIO,RECORD_VIDEO", 10, null, null));
		listHook.add(new Hook("media", "android.media.action.IMAGE_CAPTURE", false, false, "CAMERA", 10, null, null));
		listHook.add(new Hook("media", "android.media.action.IMAGE_CAPTURE_SECURE", false, false, "CAMERA", 17, null, null));
		listHook.add(new Hook("media", "android.media.action.VIDEO_CAPTURE", false, false, "CAMERA", 10, null, null));

		listHook.add(new Hook("messages", "getAllMessagesFromIcc", false, false, "RECEIVE_SMS", 10, null, null));
		listHook.add(new Hook("messages", "SmsProvider", false, false, "READ_SMS", 10, null, null));
		listHook.add(new Hook("messages", "MmsProvider", false, false, "READ_SMS", 10, null, null));
		listHook.add(new Hook("messages", "MmsSmsProvider", false, false, "READ_SMS", 10, null, null));
		listHook.add(new Hook("messages", "VoicemailContentProvider", false, false, "READ_WRITE_ALL_VOICEMAIL", 14, null, null));
		listHook.add(new Hook("messages", "android.intent.action.DATA_SMS_RECEIVED", false, false, "RECEIVE_SMS", 19, null, null));
		listHook.add(new Hook("messages", "android.provider.Telephony.SMS_RECEIVED", false, false, "RECEIVE_SMS", 19, null, null));
		listHook.add(new Hook("messages", "android.provider.Telephony.WAP_PUSH_RECEIVED", false, false, "RECEIVE_WAP_PUSH", 19, null, null));

		listHook.add(new Hook("network", "getAddress", false, false, "BLUETOOTH", 5, null, null));
		listHook.add(new Hook("network", "getBondedDevices", false, false, "BLUETOOTH", 5, null, null));
		listHook.add(new Hook("network", "getHardwareAddress", false, false, "ACCESS_NETWORK_STATE", 10, null, null));
		listHook.add(new Hook("network", "getInetAddresses", false, false, "ACCESS_NETWORK_STATE", 10, null, null));
		listHook.add(new Hook("network", "getInterfaceAddresses", false, false, "ACCESS_NETWORK_STATE", 10, null, null));
		listHook.add(new Hook("network", "getConfiguredNetworks", false, false, "ACCESS_WIFI_STATE", 10, null, null));
		listHook.add(new Hook("network", "getConnectionInfo", false, false, "ACCESS_WIFI_STATE", 10, null, null));
		listHook.add(new Hook("network", "getDhcpInfo", false, false, "ACCESS_WIFI_STATE", 10, null, null));
		listHook.add(new Hook("network", "getScanResults", false, false, "ACCESS_WIFI_STATE", 10, null, null));
		listHook.add(new Hook("network", "getWifiApConfiguration", false, false, "ACCESS_WIFI_STATE", 10, null, null));
		listHook.add(new Hook("network", "getConfiguredNetworks", true, false, "", -1, null, null));

		listHook.add(new Hook("nfc", "getNfcAdapter", false, false, "NFC", 14, null, null));
		listHook.add(new Hook("nfc", "getDefaultAdapter", false, false, "NFC", 10, null, null));
		listHook.add(new Hook("nfc", "android.nfc.action.ADAPTER_STATE_CHANGED", false, false, "NFC", 18, null, null));
		listHook.add(new Hook("nfc", "android.nfc.action.NDEF_DISCOVERED", false, false, "NFC", 10, null, null));
		listHook.add(new Hook("nfc", "android.nfc.action.TAG_DISCOVERED", false, false, "NFC", 10, null, null));
		listHook.add(new Hook("nfc", "android.nfc.action.TECH_DISCOVERED", false, false, "NFC", 10, null, null));

		listHook.add(new Hook("notifications", "android.service.notification.NotificationListenerService", false, false, "BIND_NOTIFICATION_LISTENER_SERVICE", 18, null, null));
		listHook.add(new Hook("notifications", "com.google.android.c2dm.intent.REGISTRATION", true, false, "com.google.android.c2dm.permission.RECEIVE", 10, null, null));
		listHook.add(new Hook("notifications", "com.google.android.c2dm.intent.RECEIVE", true, false, "com.google.android.c2dm.permission.RECEIVE", 10, null, null));

		listHook.add(new Hook("overlay", "addView", false, false, "SYSTEM_ALERT_WINDOW", 1, null, null));
		listHook.add(new Hook("overlay", "removeView", false, false, "SYSTEM_ALERT_WINDOW", 1, null, null));
		listHook.add(new Hook("overlay", "updateViewLayout", false, false, "SYSTEM_ALERT_WINDOW", 1, null, null));

		listHook.add(new Hook("phone", "getDeviceId", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "getIsimDomain", false, false, "READ_PRIVILEGED_PHONE_STATE", 14, null, null));
		listHook.add(new Hook("phone", "getIsimImpi", false, false, "READ_PRIVILEGED_PHONE_STATE", 14, null, null));
		listHook.add(new Hook("phone", "getIsimImpu", false, false, "READ_PRIVILEGED_PHONE_STATE", 14, null, null));
		listHook.add(new Hook("phone", "getLine1AlphaTag", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "getLine1Number", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "getMsisdn", false, false, "READ_PHONE_STATE", 14, null, null));
		listHook.add(new Hook("phone", "getSimSerialNumber", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "getSubscriberId", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "getVoiceMailAlphaTag", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "getVoiceMailNumber", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "listen", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "getNetworkCountryIso", false, false, "", 10, null, null));
		listHook.add(new Hook("phone", "getNetworkOperator", false, false, "", 10, null, null));
		listHook.add(new Hook("phone", "getNetworkOperatorName", false, false, "", 10, null, null));
		listHook.add(new Hook("phone", "getNetworkType", false, false, "", 10, null, null));
		listHook.add(new Hook("phone", "getPhoneType", false, false, "", 10, null, null));
		listHook.add(new Hook("phone", "getSimCountryIso", false, false, "", 10, null, null));
		listHook.add(new Hook("phone", "getSimOperator", false, false, "", 10, null, null));
		listHook.add(new Hook("phone", "getSimOperatorName", false, false, "", 10, null, null));
		listHook.add(new Hook("phone", "getGroupIdLevel1", false, false, "READ_PHONE_STATE", 18, null, null));
		listHook.add(new Hook("phone", "android.intent.action.NEW_OUTGOING_CALL", false, false, "PROCESS_OUTGOING_CALLS", 10, null, null));
		listHook.add(new Hook("phone", "android.intent.action.PHONE_STATE", false, false, "READ_PHONE_STATE", 10, null, null));
		listHook.add(new Hook("phone", "TelephonyProvider", false, false, "WRITE_APN_SETTINGS", 10, null, null));
		listHook.add(new Hook("phone", "CallLogProvider", false, false, "READ_CALL_LOG", 10, null, null));
		listHook.add(new Hook("phone", "gsm.operator.iso-country", false, false, "", 10, "1.99.1", null));
		listHook.add(new Hook("phone", "gsm.operator.numeric", false, false, "", 10, "1.99.1", null));
		listHook.add(new Hook("phone", "gsm.operator.alpha", false, false, "", 10, "1.99.1", null));
		listHook.add(new Hook("phone", "gsm.current.phone-type", false, false, "", 10, "1.99.1", null));
		listHook.add(new Hook("phone", "gsm.sim.operator.iso-country", false, false, "", 10, "1.99.1", null));
		listHook.add(new Hook("phone", "gsm.sim.operator.numeric", false, false, "", 10, "1.99.1", null));
		listHook.add(new Hook("phone", "gsm.sim.operator.alpha", false, false, "", 10, "1.99.1", null));
		listHook.add(new Hook("phone", "Configuration.MCC", false, false, "", 1, "2.0", null).noUsageData());
		listHook.add(new Hook("phone", "Configuration.MNC", false, false, "", 1, "2.0", null).noUsageData());

		listHook.add(new Hook("sensors", "getDefaultSensor", false, false, "", 3, null, null));
		listHook.add(new Hook("sensors", "getSensorList", false, false, "", 3, null, null));

		listHook.add(new Hook("shell", "sh", false, false, "", 10, null, null));
		listHook.add(new Hook("shell", "su", false, false, "", 10, null, null));
		listHook.add(new Hook("shell", "exec", false, false, "", 10, null, null));
		listHook.add(new Hook("shell", "load", true, false, "", 10, null, null));
		listHook.add(new Hook("shell", "loadLibrary", true, false, "", 10, null, null));
		listHook.add(new Hook("shell", "start", false, false, "", 10, null, null));

		listHook.add(new Hook("storage", "media", true, true, "WRITE_MEDIA_STORAGE", 10, null, null).noUsageData());
		listHook.add(new Hook("storage", "sdcard", true, true, "READ_EXTERNAL_STORAGE,WRITE_EXTERNAL_STORAGE", 10, null, null).noUsageData());
		listHook.add(new Hook("storage", "getExternalStorageState", false, false, null, 10, null, null));

		listHook.add(new Hook("system", "getInstalledApplications", true, false, "", 1, null, null));
		listHook.add(new Hook("system", "getInstalledPackages", true, false, "", 1, null, null));
		listHook.add(new Hook("system", "getPackagesHoldingPermissions", true, false, "", 1, "1.99.1", null));
		listHook.add(new Hook("system", "getPreferredPackages", true, false, "", 1, null, null));
		listHook.add(new Hook("system", "queryBroadcastReceivers", true, false, "", 1, null, null));
		listHook.add(new Hook("system", "queryContentProviders", true, false, "", 1, null, null));
		listHook.add(new Hook("system", "queryIntentActivities", true, false, "", 1, null, null));
		listHook.add(new Hook("system", "queryIntentActivityOptions", true, false, "", 1, null, null));
		listHook.add(new Hook("system", "queryIntentContentProviders", true, false, "", 19, "1.99.1", null));
		listHook.add(new Hook("system", "queryIntentServices", true, false, "", 1, null, null));
		listHook.add(new Hook("system", "getInstalledProviders", true, false, "", 3, null, null));
		listHook.add(new Hook("system", "getRecentTasks", true, false, "GET_TASKS", 10, null, null));
		listHook.add(new Hook("system", "getRunningAppProcesses", true, false, "GET_TASKS", 10, null, null));
		listHook.add(new Hook("system", "getRunningServices", true, false, "GET_TASKS", 10, null, null));
		listHook.add(new Hook("system", "getRunningTasks", true, false, "GET_TASKS", 10, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_ADDED", true, false, "", 10, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_REPLACED", true, false, "", 10, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_RESTARTED", true, false, "", 10, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_REMOVED", true, false, "", 10, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_CHANGED", true, false, "", 10, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_DATA_CLEARED", true, false, "", 10, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_FIRST_LAUNCH", true, false, "", 12, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_FULLY_REMOVED", true, false, "", 14, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_NEEDS_VERIFICATION", true, false, "", 14, null, null));
		listHook.add(new Hook("system", "android.intent.action.PACKAGE_VERIFIED", true, false, "", 17, null, null));
		listHook.add(new Hook("system", "android.intent.action.EXTERNAL_APPLICATIONS_AVAILABLE", true, false, "", 10, null, null));
		listHook.add(new Hook("system", "android.intent.action.EXTERNAL_APPLICATIONS_UNAVAILABLE", true, false, "", 10, null, null));
		listHook.add(new Hook("system", "ApplicationsProvider", false, false, "", 10, null, null));

		listHook.add(new Hook("view", "loadUrl", false, false, "", 1, null, null));
		listHook.add(new Hook("view", "WebView", false, false, "", 1, null, null));
		listHook.add(new Hook("view", "getDefaultUserAgent", false, false, "", 17, null, null));
		listHook.add(new Hook("view", "getUserAgent", false, false, "", 1, null, null));
		listHook.add(new Hook("view", "getUserAgentString", false, false, "", 1, null, null));
		listHook.add(new Hook("view", "setUserAgent", false, false, "", 1, null, null));
		listHook.add(new Hook("view", "setUserAgentString", false, false, "", 1, null, null));
		listHook.add(new Hook("view", "android.intent.action.VIEW", false, false, "", 10, null, null));
		// @formatter:on
		return listHook;
	}
}
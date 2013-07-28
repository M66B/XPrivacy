package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

import android.annotation.SuppressLint;
import android.app.AndroidAppHelper;
import android.content.Context;
import android.content.Intent;
import android.nfc.NfcAdapter;
import android.os.Build;
import android.os.Process;
import android.provider.MediaStore;
import android.service.notification.NotificationListenerService;
import android.telephony.TelephonyManager;
import android.util.Log;
import android.widget.Toast;

import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers;
import de.robv.android.xposed.callbacks.XC_LoadPackage.LoadPackageParam;
import de.robv.android.xposed.XC_MethodHook;
import static de.robv.android.xposed.XposedHelpers.findClass;

public class XPrivacy implements IXposedHookLoadPackage, IXposedHookZygoteInit {

	// @formatter:off

	// http://developer.android.com/reference/android/Manifest.permission.html

	// @formatter:on

	@SuppressLint("InlinedApi")
	public void initZygote(StartupParam startupParam) throws Throwable {
		// Log load
		Util.log(null, Log.INFO, String.format("load %s", startupParam.modulePath));

		// Set preferences readable
		// For compatibility with older versions
		PrivacyProvider.setPrefFileReadable(PrivacyProvider.PREF_RESTRICTION);
		PrivacyProvider.setPrefFileReadable(PrivacyProvider.PREF_SETTINGS);

		// Account manager
		hook(new XAccountManager("addOnAccountsUpdatedListener", PrivacyManager.cAccounts));
		hook(new XAccountManager("blockingGetAuthToken", PrivacyManager.cAccounts));
		hook(new XAccountManager("getAccounts", PrivacyManager.cAccounts));
		hook(new XAccountManager("getAccountsByType", PrivacyManager.cAccounts));
		hook(new XAccountManager("getAccountsByTypeAndFeatures", PrivacyManager.cAccounts));
		hook(new XAccountManager("getAuthToken", PrivacyManager.cAccounts));
		hook(new XAccountManager("getAuthTokenByFeatures", PrivacyManager.cAccounts));
		hook(new XAccountManager("hasFeatures", PrivacyManager.cAccounts));
		hook(new XAccountManager("removeOnAccountsUpdatedListener", PrivacyManager.cAccounts));

		hook(new XAccountManager("getAccountsByTypeForPackage", PrivacyManager.cAccounts,
				Build.VERSION_CODES.JELLY_BEAN_MR2));

		// Activity manager
		String[] acts = new String[] { "getRecentTasks", "getRunningAppProcesses", "getRunningServices",
				"getRunningTasks" };
		for (String act : acts)
			hook(new XActivityManager(act, PrivacyManager.cSystem));

		// App widget manager
		hook(new XAppWidgetManager("getInstalledProviders", PrivacyManager.cSystem));

		// Application package manager
		String[] ams = new String[] { "getInstalledApplications", "getInstalledPackages", "getPreferredPackages",
				"queryBroadcastReceivers", "queryContentProviders", "queryIntentActivities",
				"queryIntentActivityOptions", "queryIntentServices" };
		for (String am : ams)
			hook(new XApplicationPackageManager(am, PrivacyManager.cSystem));

		// Audio record
		hook(new XAudioRecord("startRecording", PrivacyManager.cMedia));

		// Bluetooth adapter
		hook(new XBluetoothAdapter("getAddress", PrivacyManager.cNetwork));
		hook(new XBluetoothAdapter("getBondedDevices", PrivacyManager.cNetwork));

		// Bluetooth device
		hook(new XBluetoothDevice("getAddress", PrivacyManager.cNetwork));

		// Camera
		String[] cams = new String[] { "setPreviewCallback", "setPreviewCallbackWithBuffer",
				"setOneShotPreviewCallback", "takePicture" };
		for (String cam : cams)
			hook(new XCamera(cam, PrivacyManager.cMedia));

		// Clipboard manager
		String[] clips = new String[] { "addPrimaryClipChangedListener", "getPrimaryClip", "getPrimaryClipDescription",
				"getText", "hasPrimaryClip", "hasText" };
		for (String clip : clips)
			hook(new XClipboardManager(clip, PrivacyManager.cSystem));

		// Connectivity manager
		// This is to fake "offline", no permission required
		String[] connmgrs = new String[] { "getActiveNetworkInfo", "getAllNetworkInfo", "getNetworkInfo" };
		for (String connmgr : connmgrs)
			hook(new XConnectivityManager(connmgr, PrivacyManager.cInternet));

		// Environment
		// This is to fake "unmounted", no permission required
		hook(new XEnvironment("getExternalStorageState", PrivacyManager.cStorage));

		// InetAddress
		String[] addrs = new String[] { "getAllByName", "getByAddress", "getByName" };
		for (String addr : addrs)
			hook(new XInetAddress(addr, PrivacyManager.cInternet, null));

		// IO bridge
		hook(new XIoBridge("open", PrivacyManager.cIdentification, "/proc"));

		// Location manager
		String[] locs = new String[] { "addNmeaListener", "addProximityAlert", "getLastKnownLocation", "getProviders",
				"isProviderEnabled", "removeUpdates", "requestLocationUpdates", "requestSingleUpdate",
				"sendExtraCommand" };
		for (String loc : locs)
			hook(new XLocationManager(loc, PrivacyManager.cLocation));

		hook(new XLocationManager("addGeofence", PrivacyManager.cLocation, Build.VERSION_CODES.JELLY_BEAN_MR1));
		hook(new XLocationManager("getLastLocation", PrivacyManager.cLocation, Build.VERSION_CODES.JELLY_BEAN_MR1));

		// Media recorder
		hook(new XMediaRecorder("setOutputFile", PrivacyManager.cMedia));

		// Network info
		// This is to fake "offline", no permission required
		String[] ninfos = new String[] { "getDetailedState", "getState", "isConnected", "isConnectedOrConnecting" };
		for (String ninfo : ninfos)
			hook(new XNetworkInfo(ninfo, PrivacyManager.cInternet));

		// Network interface
		String[] nets = new String[] { "getHardwareAddress", "getInetAddresses", "getInterfaceAddresses" };
		for (String net : nets)
			hook(new XNetworkInterface(net, PrivacyManager.cNetwork));

		String[] inets = new String[] { "getByInetAddress", "getByName", "getNetworkInterfaces" };
		for (String inet : inets)
			hook(new XNetworkInterface(inet, PrivacyManager.cInternet));

		// Package manager service
		hook(new XPackageManagerService("getPackageGids", PrivacyManager.cInternet, "inet"));
		hook(new XPackageManagerService("getPackageGids", PrivacyManager.cStorage, "media"));
		hook(new XPackageManagerService("getPackageGids", PrivacyManager.cStorage, "sdcard"));

		// Process builder
		hook(new XProcessBuilder("start", PrivacyManager.cShell, "sh"));
		hook(new XProcessBuilder("start", PrivacyManager.cShell, "su"));
		hook(new XProcessBuilder("start", PrivacyManager.cShell, null));

		// Runtime
		hook(new XRuntime("exec", PrivacyManager.cShell, "sh"));
		hook(new XRuntime("exec", PrivacyManager.cShell, "su"));
		hook(new XRuntime("exec", PrivacyManager.cShell, null));
		hook(new XRuntime("load", PrivacyManager.cShell, null));
		hook(new XRuntime("loadLibrary", PrivacyManager.cShell, null));

		// Settings secure
		hook(new XSettingsSecure("getString", PrivacyManager.cIdentification));

		// SMS manager
		hook(new XSmsManager("getAllMessagesFromIcc", PrivacyManager.cMessages));
		String[] smses = new String[] { "sendDataMessage", "sendMultipartTextMessage", "sendTextMessage" };
		for (String sms : smses)
			hook(new XSmsManager(sms, PrivacyManager.cCalling));

		// System properties
		String[] props = new String[] { "%imei", "%hostname", "%serialno", "%macaddr" };
		String[] getters = new String[] { "get", "getBoolean", "getInt", "getLong" };
		for (String prop : props)
			for (String getter : getters)
				hook(new XSystemProperties(getter, PrivacyManager.cIdentification, prop));

		// Telephony
		hook(new XTelephonyManager("disableLocationUpdates", PrivacyManager.cLocation));
		hook(new XTelephonyManager("enableLocationUpdates", PrivacyManager.cLocation));
		hook(new XTelephonyManager("getCellLocation", PrivacyManager.cLocation));
		hook(new XTelephonyManager("getNeighboringCellInfo", PrivacyManager.cLocation));

		hook(new XTelephonyManager("getAllCellInfo", PrivacyManager.cLocation, Build.VERSION_CODES.JELLY_BEAN_MR1));

		String[] phones = new String[] { "getDeviceId", "getIsimDomain", "getIsimImpi", "getIsimImpu",
				"getLine1AlphaTag", "getLine1Number", "getMsisdn", "getSimSerialNumber", "getSubscriberId",
				"getVoiceMailAlphaTag", "getVoiceMailNumber", "listen" };
		for (String phone : phones)
			hook(new XTelephonyManager(phone, PrivacyManager.cPhone));

		// No permissions required
		String[] phones1 = new String[] { "getNetworkCountryIso", "getNetworkOperator", "getNetworkOperatorName",
				"getSimCountryIso", "getSimOperator", "getSimOperatorName" };
		for (String phone1 : phones1)
			hook(new XTelephonyManager(phone1, PrivacyManager.cPhone));

		hook(new XTelephonyManager("getGroupIdLevel1", PrivacyManager.cPhone, Build.VERSION_CODES.JELLY_BEAN_MR2));

		// Wi-Fi manager
		String[] wifis = new String[] { "getConfiguredNetworks", "getConnectionInfo", "getDhcpInfo", "getScanResults",
				"getWifiApConfiguration" };
		for (String wifi : wifis)
			hook(new XWifiManager(wifi, PrivacyManager.cNetwork));
		hook(new XWifiManager("getScanResults", PrivacyManager.cLocation));
		// This is to fake "offline", no permission required
		hook(new XWifiManager("getConnectionInfo", PrivacyManager.cInternet));

		// Intent receive: calling
		hook(new XActivityThread("handleReceiver", PrivacyManager.cPhone, Intent.ACTION_NEW_OUTGOING_CALL));
		hook(new XActivityThread("handleReceiver", PrivacyManager.cPhone, TelephonyManager.ACTION_PHONE_STATE_CHANGED));

		hook(new XActivityThread("handleReceiver", PrivacyManager.cCalling,
				TelephonyManager.ACTION_RESPOND_VIA_MESSAGE, Build.VERSION_CODES.JELLY_BEAN_MR2));

		// Intent receive: NFC
		hook(new XActivityThread("handleReceiver", PrivacyManager.cNfc, NfcAdapter.ACTION_NDEF_DISCOVERED));
		hook(new XActivityThread("handleReceiver", PrivacyManager.cNfc, NfcAdapter.ACTION_TAG_DISCOVERED));
		hook(new XActivityThread("handleReceiver", PrivacyManager.cNfc, NfcAdapter.ACTION_TECH_DISCOVERED));

		// Intent receive: notifications
		hook(new XActivityThread("handleReceiver", PrivacyManager.cSystem,
				NotificationListenerService.SERVICE_INTERFACE, Build.VERSION_CODES.JELLY_BEAN_MR2));

		// Intent receive: package changes
		hook(new XActivityThread("handleReceiver", PrivacyManager.cSystem, Intent.ACTION_PACKAGE_ADDED));
		hook(new XActivityThread("handleReceiver", PrivacyManager.cSystem, Intent.ACTION_PACKAGE_REPLACED));
		hook(new XActivityThread("handleReceiver", PrivacyManager.cSystem, Intent.ACTION_PACKAGE_RESTARTED));
		hook(new XActivityThread("handleReceiver", PrivacyManager.cSystem, Intent.ACTION_PACKAGE_REMOVED));

		String[] startActivities = new String[] { "startActivities", "startActivity", "startActivityForResult",
				"startActivityFromChild", "startActivityFromFragment", "startActivityIfNeeded" };

		// Intent send: browser
		for (String activity : startActivities)
			hook(new XActivity(activity, PrivacyManager.cView, Intent.ACTION_VIEW));

		// Intent send: call
		for (String activity : startActivities)
			hook(new XActivity(activity, PrivacyManager.cCalling, Intent.ACTION_CALL));

		// Intent send: media
		hook(new XActivity("startActivityForResult", PrivacyManager.cMedia, MediaStore.ACTION_IMAGE_CAPTURE));
		hook(new XActivity("startActivityForResult", PrivacyManager.cMedia, MediaStore.ACTION_IMAGE_CAPTURE_SECURE,
				Build.VERSION_CODES.JELLY_BEAN_MR1));
		hook(new XActivity("startActivityForResult", PrivacyManager.cMedia, MediaStore.ACTION_VIDEO_CAPTURE));
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		// Log load
		Util.log(null, Log.INFO, String.format("load package=%s uid=%d", lpparam.packageName, Process.myUid()));

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (lpparam.packageName.equals(self)) {
			hook(new XUtilHook("isXposedEnabled", null), lpparam.classLoader);
			return;
		}

		// Build SERIAL
		if (PrivacyManager.getRestricted(null, null, Process.myUid(), PrivacyManager.cIdentification, "SERIAL", true,
				false))
			XposedHelpers.setStaticObjectField(Build.class, "SERIAL", PrivacyManager.getDefacedProp("SERIAL"));

		// Applications provider
		if (lpparam.packageName.equals("com.android.providers.applications"))
			hook(new XContentProvider(PrivacyManager.cSystem, "ApplicationsProvider",
					"com.android.providers.applications.ApplicationsProvider"), lpparam.classLoader);

		// Browser provider
		else if (lpparam.packageName.equals("com.android.browser")) {
			hook(new XContentProvider(PrivacyManager.cBrowser, "BrowserProvider",
					"com.android.browser.provider.BrowserProvider"), lpparam.classLoader);
			hook(new XContentProvider(PrivacyManager.cBrowser, "BrowserProvider2",
					"com.android.browser.provider.BrowserProvider2"), lpparam.classLoader);
		}

		// Calendar provider
		else if (lpparam.packageName.equals("com.android.providers.calendar"))
			hook(new XContentProvider(PrivacyManager.cCalendar, "CalendarProvider2",
					"com.android.providers.calendar.CalendarProvider2"), lpparam.classLoader);

		// Contacts provider
		else if (lpparam.packageName.equals("com.android.providers.contacts")) {
			String[] uris = new String[] { "contacts/contacts", "contacts/data", "contacts/raw_contacts",
					"contacts/phone_lookup", "contacts/profile" };
			for (String uri : uris)
				hook(new XContentProvider(PrivacyManager.cContacts, "ContactsProvider2",
						"com.android.providers.contacts.ContactsProvider2", "content://com.android." + uri),
						lpparam.classLoader);

			hook(new XContentProvider(PrivacyManager.cPhone, "CallLogProvider",
					"com.android.providers.contacts.CallLogProvider"), lpparam.classLoader);
			hook(new XContentProvider(PrivacyManager.cMessages, "VoicemailContentProvider",
					"com.android.providers.contacts.VoicemailContentProvider"), lpparam.classLoader);
		}

		// E-mail provider
		else if (lpparam.packageName.equals("com.android.email"))
			hook(new XContentProvider(PrivacyManager.cEMail, "EMailProvider",
					"com.android.email.provider.EmailProvider"), lpparam.classLoader);

		// Google services provider
		else if (lpparam.packageName.equals("com.google.android.gsf"))
			hook(new XContentProvider(PrivacyManager.cIdentification, "GservicesProvider",
					"com.google.android.gsf.gservices.GservicesProvider"), lpparam.classLoader);

		// Telephony providers
		else if (lpparam.packageName.equals("com.android.providers.telephony")) {
			hook(new XContentProvider(PrivacyManager.cMessages, "SmsProvider",
					"com.android.providers.telephony.SmsProvider"), lpparam.classLoader);
			hook(new XContentProvider(PrivacyManager.cMessages, "MmsProvider",
					"com.android.providers.telephony.MmsProvider"), lpparam.classLoader);
			hook(new XContentProvider(PrivacyManager.cMessages, "MmsSmsProvider",
					"com.android.providers.telephony.MmsSmsProvider"), lpparam.classLoader);
			hook(new XContentProvider(PrivacyManager.cPhone, "TelephonyProvider",
					"com.android.providers.telephony.TelephonyProvider"), lpparam.classLoader);
		}

		// User dictionary
		else if (lpparam.packageName.equals("com.android.providers.userdictionary"))
			hook(new XContentProvider(PrivacyManager.cDictionary, "UserDictionary",
					"com.android.providers.userdictionary.UserDictionaryProvider"), lpparam.classLoader);

		// Google auth
		try {
			Class.forName("com.google.android.gms.auth.GoogleAuthUtil", false, lpparam.classLoader);
			hook(new XGoogleAuthUtil("getToken", PrivacyManager.cAccounts, "getTokenGoogle"), lpparam.classLoader);
			hook(new XGoogleAuthUtil("getTokenWithNotification", PrivacyManager.cAccounts,
					"getTokenWithNotificationGoogle"), lpparam.classLoader);
		} catch (Throwable ex) {
		}

		// Location client
		try {
			Class.forName("com.google.android.gms.location.LocationClient", false, lpparam.classLoader);
			hook(new XLocationClient("connect", PrivacyManager.cLocation), lpparam.classLoader);
		} catch (Throwable ex) {
		}
	}

	private void hook(final XHook hook) {
		hook(hook, null);
	}

	private void hook(final XHook hook, ClassLoader classLoader) {
		// Check SDK version
		if (Build.VERSION.SDK_INT < hook.getSdk())
			return;

		try {
			// Create hook method
			XC_MethodHook methodHook = new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
					try {
						hook.before(param);
					} catch (Throwable ex) {
						Util.bug(null, ex);
						report(ex);
						throw ex;
					}
				}

				@Override
				protected void afterHookedMethod(MethodHookParam param) throws Throwable {
					// Throw any exception
					param.getResultOrThrowable();
					try {
						hook.after(param);
					} catch (Throwable ex) {
						Util.bug(null, ex);
						report(ex);
						throw ex;
					}
				}
			};

			// Find class
			Class<?> hookClass = findClass(hook.getClassName(), classLoader);
			if (hookClass == null) {
				Util.log(
						hook,
						Log.WARN,
						String.format("%s: class not found: %s", AndroidAppHelper.currentPackageName(),
								hook.getClassName()));
				return;
			}

			// Add hook
			Set<XC_MethodHook.Unhook> hookSet = new HashSet<XC_MethodHook.Unhook>();
			if (hook.getMethodName() == null) {
				for (Constructor<?> constructor : hookClass.getDeclaredConstructors())
					if (Modifier.isPublic(constructor.getModifiers()) ? hook.isVisible() : !hook.isVisible())
						hookSet.add(XposedBridge.hookMethod(constructor, methodHook));
			} else
				for (Method method : hookClass.getDeclaredMethods())
					if (method.getName().equals(hook.getMethodName())
							&& (Modifier.isPublic(method.getModifiers()) ? hook.isVisible() : !hook.isVisible()))
						hookSet.add(XposedBridge.hookMethod(method, methodHook));

			// Check if found
			if (hookSet.isEmpty()) {
				Util.log(
						hook,
						Log.WARN,
						String.format("%s: method not found: %s.%s", AndroidAppHelper.currentPackageName(),
								hookClass.getName(), hook.getMethodName()));
				return;
			}

			// Log
			for (XC_MethodHook.Unhook unhook : hookSet) {
				Util.log(hook, Log.INFO, String.format("%s: hooked %s.%s/%s (%d)",
						AndroidAppHelper.currentPackageName(), hookClass.getName(), unhook.getHookedMethod().getName(),
						hook.getRestrictionName(), hookSet.size()));
				break;
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private void report(Throwable ex) {
		Context context = AndroidAppHelper.currentApplication();
		if (context != null) {
			Toast toast = Toast.makeText(context, ex.toString(), Toast.LENGTH_LONG);
			toast.show();
		}
	}
}

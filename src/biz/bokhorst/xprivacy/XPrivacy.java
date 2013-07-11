package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

import android.app.AndroidAppHelper;
import android.content.Context;
import android.content.Intent;
import android.nfc.NfcAdapter;
import android.os.Build;
import android.os.Process;
import android.provider.MediaStore;
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

	public void initZygote(StartupParam startupParam) throws Throwable {
		// Log load
		Util.log(null, Log.INFO, String.format("load %s", startupParam.modulePath));

		// Set preferences readable
		// For compatibility with older versions
		PrivacyProvider.setPrefFileReadable(PrivacyProvider.PREF_RESTRICTION);
		PrivacyProvider.setPrefFileReadable(PrivacyProvider.PREF_SETTINGS);

		// Account manager
		hook(new XAccountManager("addOnAccountsUpdatedListener", PrivacyManager.cAccounts,
				new String[] { "GET_ACCOUNTS" }), "android.accounts.AccountManager");
		hook(new XAccountManager("blockingGetAuthToken", PrivacyManager.cAccounts, new String[] { "USE_CREDENTIALS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("getAccounts", PrivacyManager.cAccounts, new String[] { "GET_ACCOUNTS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("getAccountsByType", PrivacyManager.cAccounts, new String[] { "GET_ACCOUNTS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("getAccountsByTypeAndFeatures", PrivacyManager.cAccounts,
				new String[] { "GET_ACCOUNTS" }), "android.accounts.AccountManager");
		hook(new XAccountManager("getAuthToken", PrivacyManager.cAccounts, new String[] { "USE_CREDENTIALS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("getAuthTokenByFeatures", PrivacyManager.cAccounts, new String[] { "MANAGE_ACCOUNTS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("hasFeatures", PrivacyManager.cAccounts, new String[] { "GET_ACCOUNTS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("removeOnAccountsUpdatedListener", PrivacyManager.cAccounts,
				new String[] { "GET_ACCOUNTS" }), "android.accounts.AccountManager");

		// Activity manager
		String[] acts = new String[] { "getRecentTasks", "getRunningAppProcesses", "getRunningServices",
				"getRunningTasks" };
		for (String act : acts)
			hook(new XActivityManager(act, PrivacyManager.cSystem, new String[] { "GET_TASKS" }),
					"android.app.ActivityManager");

		// App widget manager
		hook(new XAppWidgetManager("getInstalledProviders", PrivacyManager.cSystem, new String[] {}),
				"android.appwidget.AppWidgetManager");

		// Application package manager
		String[] ams = new String[] { "getInstalledApplications", "getInstalledPackages", "getPreferredPackages",
				"queryBroadcastReceivers", "queryContentProviders", "queryIntentActivities",
				"queryIntentActivityOptions", "queryIntentServices" };
		for (String am : ams)
			hook(new XApplicationPackageManager(am, PrivacyManager.cSystem, new String[] {}),
					"android.app.ApplicationPackageManager");

		// Audio record
		hook(new XCamera("startRecording", PrivacyManager.cMedia, new String[] { "RECORD_AUDIO" }),
				"android.media.AudioRecord");

		// Bluetooth adapter
		hook(new XBluetoothAdapter("getAddress", PrivacyManager.cNetwork, new String[] { "BLUETOOTH" }),
				"android.bluetooth.BluetoothAdapter");
		hook(new XBluetoothAdapter("getBondedDevices", PrivacyManager.cNetwork, new String[] { "BLUETOOTH" }),
				"android.bluetooth.BluetoothAdapter");

		// Camera
		String[] cams = new String[] { "setPreviewCallback", "setPreviewCallbackWithBuffer",
				"setOneShotPreviewCallback", "takePicture" };
		for (String cam : cams)
			hook(new XCamera(cam, PrivacyManager.cMedia, new String[] { "CAMERA" }), "android.hardware.Camera");

		// Environment
		// This is to fake "unmounted", no permission required
		hook(new XEnvironment("getExternalStorageState", PrivacyManager.cStorage, null), "android.os.Environment");

		// Location manager
		String[] locs = new String[] { "addNmeaListener", "addProximityAlert", "getLastKnownLocation", "removeUpdates",
				"requestLocationUpdates", "requestSingleUpdate", "sendExtraCommand" };
		for (String loc : locs)
			hook(new XLocationManager(loc, PrivacyManager.cLocation, new String[] { "ACCESS_COARSE_LOCATION",
					"ACCESS_FINE_LOCATION" }), "android.location.LocationManager");

		// Media recorder
		hook(new XMediaRecorder("setOutputFile", PrivacyManager.cMedia, new String[] { "RECORD_AUDIO", "RECORD_VIDEO" }),
				"android.media.MediaRecorder");

		// Network info
		// This is to fake "offline", no permission required
		String[] ninfos = new String[] { "getDetailedState", "getState", "isConnected", "isConnectedOrConnecting" };
		for (String ninfo : ninfos)
			hook(new XNetworkInfo(ninfo, PrivacyManager.cInternet, null), "android.net.NetworkInfo");
		// TODO: BT PAN

		// Network interface
		String[] nets = new String[] { "getHardwareAddress", "getInetAddresses", "getInterfaceAddresses" };
		for (String net : nets)
			hook(new XNetworkInterface(net, PrivacyManager.cNetwork, new String[] { "ACCESS_NETWORK_STATE" }),
					"java.net.NetworkInterface");

		// Package manager service
		hook(new XPackageManagerService("getPackageGids", PrivacyManager.cInternet, new String[] { "INTERNET" }, "inet"),
				"com.android.server.pm.PackageManagerService");
		hook(new XPackageManagerService("getPackageGids", PrivacyManager.cStorage,
				new String[] { "WRITE_MEDIA_STORAGE" }, "media"), "com.android.server.pm.PackageManagerService");
		hook(new XPackageManagerService("getPackageGids", PrivacyManager.cStorage, new String[] {
				"READ_EXTERNAL_STORAGE", "WRITE_EXTERNAL_STORAGE" }, "sdcard"),
				"com.android.server.pm.PackageManagerService");

		// Runtime
		hook(new XRuntime("exec", PrivacyManager.cShell, new String[] {}, "sh"), "java.lang.Runtime");
		hook(new XRuntime("exec", PrivacyManager.cShell, new String[] {}, "su"), "java.lang.Runtime");
		hook(new XRuntime("exec", PrivacyManager.cShell, new String[] {}, null), "java.lang.Runtime");
		hook(new XRuntime("load", PrivacyManager.cShell, new String[] {}, null), "java.lang.Runtime", false);
		hook(new XRuntime("loadLibrary", PrivacyManager.cShell, new String[] {}, null), "java.lang.Runtime", false);

		// Process builder
		hook(new XProcessBuilder("start", PrivacyManager.cShell, new String[] {}, "sh"), "java.lang.ProcessBuilder");
		hook(new XProcessBuilder("start", PrivacyManager.cShell, new String[] {}, "su"), "java.lang.ProcessBuilder");
		hook(new XProcessBuilder("start", PrivacyManager.cShell, new String[] {}, null), "java.lang.ProcessBuilder");

		// Settings secure
		hook(new XSettingsSecure("getString", PrivacyManager.cIdentification), "android.provider.Settings.Secure");

		// SMS manager
		hook(new XSmsManager("getAllMessagesFromIcc", PrivacyManager.cMessages, new String[] { "RECEIVE_SMS" }),
				"android.telephony.SmsManager");
		String[] smses = new String[] { "sendDataMessage", "sendMultipartTextMessage", "sendTextMessage" };
		for (String sms : smses)
			hook(new XSmsManager(sms, PrivacyManager.cCalling, new String[] { "SEND_SMS" }),
					"android.telephony.SmsManager");

		// System properties
		String[] props = new String[] { "ro.gsm.imei", "net.hostname", "ro.serialno", "ro.boot.serialno",
				"ro.boot.wifimacaddr", "ro.boot.btmacaddr" };
		String[] getters = new String[] { "get", "getBoolean", "getInt", "getLong" };
		for (String prop : props)
			for (String getter : getters)
				hook(new XSystemProperties(getter, PrivacyManager.cIdentification, new String[] {}, prop),
						"android.os.SystemProperties");

		// Telephony
		hook(new XTelephonyManager("disableLocationUpdates", PrivacyManager.cLocation,
				new String[] { "CONTROL_LOCATION_UPDATES", }), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("enableLocationUpdates", PrivacyManager.cLocation,
				new String[] { "CONTROL_LOCATION_UPDATES", }), "android.telephony.TelephonyManager");

		if (Build.VERSION.SDK_INT >= 17)
			hook(new XTelephonyManager("getAllCellInfo", PrivacyManager.cLocation,
					new String[] { "ACCESS_COARSE_UPDATES", }), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getCellLocation", PrivacyManager.cLocation, new String[] {
				"ACCESS_COARSE_LOCATION", "ACCESS_FINE_LOCATION" }), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getNeighboringCellInfo", PrivacyManager.cLocation,
				new String[] { "ACCESS_COARSE_UPDATES" }), "android.telephony.TelephonyManager");

		String[] phones = new String[] { "getDeviceId", "getIsimDomain", "getIsimImpi", "getIsimImpu",
				"getLine1AlphaTag", "getLine1Number", "getMsisdn", "getNetworkCountryIso", "getNetworkOperator",
				"getNetworkOperatorName", "getSimCountryIso", "getSimOperator", "getSimOperatorName",
				"getSimSerialNumber", "getSubscriberId", "getVoiceMailAlphaTag", "getVoiceMailNumber", "listen" };
		for (String phone : phones)
			hook(new XTelephonyManager(phone, PrivacyManager.cPhone, new String[] { "READ_PHONE_STATE" }),
					"android.telephony.TelephonyManager");

		// Wi-Fi manager
		String[] wifis = new String[] { "getConfiguredNetworks", "getConnectionInfo", "getDhcpInfo", "getScanResults" };
		for (String wifi : wifis)
			hook(new XWifiManager(wifi, PrivacyManager.cNetwork, new String[] { "ACCESS_WIFI_STATE" }),
					"android.net.wifi.WifiManager");
		hook(new XWifiManager("getScanResults", PrivacyManager.cLocation, new String[] { "ACCESS_WIFI_STATE" }),
				"android.net.wifi.WifiManager");
		// This is to fake "offline", no permission required
		hook(new XWifiManager("getConnectionInfo", PrivacyManager.cInternet, null), "android.net.wifi.WifiManager");

		// Intent receive: calling
		hook(new XActivityThread("handleReceiver", PrivacyManager.cPhone, new String[] { "PROCESS_OUTGOING_CALLS" },
				Intent.ACTION_NEW_OUTGOING_CALL), "android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", PrivacyManager.cPhone, new String[] { "READ_PHONE_STATE" },
				TelephonyManager.ACTION_PHONE_STATE_CHANGED), "android.app.ActivityThread", false);

		// Intent receive: NFC
		hook(new XActivityThread("handleReceiver", PrivacyManager.cNfc, new String[] { "NFC" },
				NfcAdapter.ACTION_NDEF_DISCOVERED), "android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", PrivacyManager.cNfc, new String[] { "NFC" },
				NfcAdapter.ACTION_TAG_DISCOVERED), "android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", PrivacyManager.cNfc, new String[] { "NFC" },
				NfcAdapter.ACTION_TECH_DISCOVERED), "android.app.ActivityThread", false);

		String[] startActivities = new String[] { "startActivities", "startActivity", "startActivityForResult",
				"startActivityFromChild", "startActivityFromFragment", "startActivityIfNeeded" };

		// Intent send: browser
		for (String activity : startActivities)
			hook(new XActivity(activity, PrivacyManager.cView, new String[] {}, Intent.ACTION_VIEW),
					"android.app.Activity");

		// Intent send: call
		for (String activity : startActivities)
			hook(new XActivity(activity, PrivacyManager.cCalling, new String[] { "CALL_PHONE" }, Intent.ACTION_CALL),
					"android.app.Activity");

		// Intent send: media
		hook(new XActivity("startActivityForResult", PrivacyManager.cMedia, new String[] { "CAMERA" },
				MediaStore.ACTION_IMAGE_CAPTURE), "android.app.Activity");
		if (Build.VERSION.SDK_INT >= 17)
			hook(new XActivity("startActivityForResult", PrivacyManager.cMedia, new String[] { "CAMERA" },
					"android.media.action.IMAGE_CAPTURE_SECURE"), "android.app.Activity");
		hook(new XActivity("startActivityForResult", PrivacyManager.cMedia, new String[] { "CAMERA" },
				MediaStore.ACTION_VIDEO_CAPTURE), "android.app.Activity");
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		// Log load
		Util.log(null, Log.INFO, String.format("load package=%s uid=%d", lpparam.packageName, Process.myUid()));

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (lpparam.packageName.equals(self)) {
			hook(new XUtilHook("isXposedEnabled", null, null), lpparam.classLoader, Util.class.getName());
			return;
		}

		// Build SERIAL
		if (PrivacyManager.getRestricted(null, null, Process.myUid(), PrivacyManager.cIdentification, "SERIAL", true,
				false))
			XposedHelpers.setStaticObjectField(Build.class, "SERIAL", PrivacyManager.getDefacedProp("SERIAL"));

		// Applications provider
		if (lpparam.packageName.equals("com.android.providers.applications"))
			hook(new XContentProvider(PrivacyManager.cSystem, new String[] {}, "ApplicationsProvider"),
					lpparam.classLoader, "com.android.providers.applications.ApplicationsProvider");

		// Browser provider
		else if (lpparam.packageName.equals("com.android.browser")) {
			hook(new XContentProvider(PrivacyManager.cBrowser,
					new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" }, "BrowserProvider"),
					lpparam.classLoader, "com.android.browser.provider.BrowserProvider");
			hook(new XContentProvider(PrivacyManager.cBrowser,
					new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" }, "BrowserProvider2"),
					lpparam.classLoader, "com.android.browser.provider.BrowserProvider2");
		}

		// Calendar provider
		else if (lpparam.packageName.equals("com.android.providers.calendar"))
			hook(new XContentProvider(PrivacyManager.cCalendar, new String[] { "READ_CALENDAR" }, "CalendarProvider2"),
					lpparam.classLoader, "com.android.providers.calendar.CalendarProvider2");

		// Contacts provider
		else if (lpparam.packageName.equals("com.android.providers.contacts")) {
			hook(new XContentProvider(PrivacyManager.cContacts, new String[] { "READ_CONTACTS" }, "ContactsProvider2"),
					lpparam.classLoader, "com.android.providers.contacts.ContactsProvider2");
			hook(new XContentProvider(PrivacyManager.cPhone, new String[] { "READ_CALL_LOG" }, "CallLogProvider"),
					lpparam.classLoader, "com.android.providers.contacts.CallLogProvider");
			hook(new XContentProvider(PrivacyManager.cMessages, new String[] { "READ_WRITE_ALL_VOICEMAIL" },
					"VoicemailContentProvider"), lpparam.classLoader,
					"com.android.providers.contacts.VoicemailContentProvider");
		}

		// E-mail provider
		else if (lpparam.packageName.equals("com.android.email"))
			hook(new XContentProvider(PrivacyManager.cEMail,
					new String[] { "com.android.email.permission.ACCESS_PROVIDER" }, "EMailProvider"),
					lpparam.classLoader, "com.android.email.provider.EmailProvider");

		// Google services provider
		else if (lpparam.packageName.equals("com.google.android.gsf"))
			hook(new XContentProvider(PrivacyManager.cIdentification, new String[] { "READ_GSERVICES" },
					"GservicesProvider"), lpparam.classLoader, "com.google.android.gsf.gservices.GservicesProvider");

		// Telephony providers
		else if (lpparam.packageName.equals("com.android.providers.telephony")) {
			hook(new XContentProvider(PrivacyManager.cMessages, new String[] { "READ_SMS" }, "SmsProvider"),
					lpparam.classLoader, "com.android.providers.telephony.SmsProvider");
			hook(new XContentProvider(PrivacyManager.cMessages, new String[] { "READ_SMS" }, "MmsProvider"),
					lpparam.classLoader, "com.android.providers.telephony.MmsProvider");
			hook(new XContentProvider(PrivacyManager.cMessages, new String[] { "READ_SMS" }, "MmsSmsProvider"),
					lpparam.classLoader, "com.android.providers.telephony.MmsSmsProvider");
			hook(new XContentProvider(PrivacyManager.cPhone, new String[] { "WRITE_APN_SETTINGS" }, "TelephonyProvider"),
					lpparam.classLoader, "com.android.providers.telephony.TelephonyProvider");
		}

		// User dictionary
		else if (lpparam.packageName.equals("com.android.providers.userdictionary"))
			hook(new XContentProvider(PrivacyManager.cDictionary, new String[] { "READ_USER_DICTIONARY" },
					"UserDictionary"), lpparam.classLoader,
					"com.android.providers.userdictionary.UserDictionaryProvider");
	}

	private void hook(final XHook hook, String className) {
		hook(hook, null, className, true);
	}

	private void hook(final XHook hook, String className, boolean visible) {
		hook(hook, null, className, visible);
	}

	private void hook(final XHook hook, ClassLoader classLoader, String className) {
		hook(hook, classLoader, className, true);
	}

	private void hook(final XHook hook, ClassLoader classLoader, String className, boolean visible) {
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
			Class<?> hookClass = findClass(className, classLoader);
			if (hookClass == null) {
				Util.log(hook, Log.WARN,
						String.format("%s: class not found: %s", AndroidAppHelper.currentPackageName(), className));
				return;
			}

			// Add hook
			Set<XC_MethodHook.Unhook> hookSet = new HashSet<XC_MethodHook.Unhook>();
			if (hook.getMethodName() == null) {
				for (Constructor<?> constructor : hookClass.getDeclaredConstructors())
					if (Modifier.isPublic(constructor.getModifiers()) ? visible : !visible)
						hookSet.add(XposedBridge.hookMethod(constructor, methodHook));
			} else
				for (Method method : hookClass.getDeclaredMethods())
					if (method.getName().equals(hook.getMethodName())
							&& (Modifier.isPublic(method.getModifiers()) ? visible : !visible))
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

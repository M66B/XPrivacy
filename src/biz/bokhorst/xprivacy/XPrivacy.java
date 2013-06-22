package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

import android.app.AndroidAppHelper;
import android.content.Intent;
import android.os.Build;
import android.os.Process;
import android.provider.MediaStore;
import android.telephony.TelephonyManager;
import android.util.Log;

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
		XUtil.log(null, Log.INFO, String.format("load %s", startupParam.modulePath));

		// Set preferences readable
		// For compatibility with older versions
		XPrivacyProvider.setPrefFileReadable(XPrivacyProvider.PREF_RESTRICTION);

		// Account manager
		hook(new XAccountManager("getAccounts", XRestriction.cAccounts, new String[] { "GET_ACCOUNTS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("getAccountsByType", XRestriction.cAccounts, new String[] { "GET_ACCOUNTS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("getAccountsByTypeAndFeatures", XRestriction.cAccounts,
				new String[] { "GET_ACCOUNTS" }), "android.accounts.AccountManager");
		hook(new XAccountManager("hasFeatures", XRestriction.cAccounts, new String[] { "GET_ACCOUNTS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("addOnAccountsUpdatedListener", XRestriction.cAccounts,
				new String[] { "GET_ACCOUNTS" }), "android.accounts.AccountManager");
		hook(new XAccountManager("getAuthToken", XRestriction.cAccounts, new String[] { "USE_CREDENTIALS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("getAuthTokenByFeatures", XRestriction.cAccounts, new String[] { "MANAGE_ACCOUNTS" }),
				"android.accounts.AccountManager");
		hook(new XAccountManager("blockingGetAuthToken", XRestriction.cAccounts, new String[] { "USE_CREDENTIALS" }),
				"android.accounts.AccountManager");

		// Application package manager
		String[] ams = new String[] { "getInstalledApplications", "getInstalledPackages", "getInstalledThemePackages",
				"getPreferredPackages" };
		for (String am : ams)
			hook(new XApplicationPackageManager(am, XRestriction.cSystem, new String[] {}),
					"android.app.ApplicationPackageManager");

		// Audio record
		hook(new XCamera("startRecording", XRestriction.cMedia, new String[] { "RECORD_AUDIO" }),
				"android.media.AudioRecord");

		// Bluetooth adapter
		hook(new XBluetoothAdapter("getAddress", XRestriction.cNetwork, new String[] { "BLUETOOTH" }),
				"android.bluetooth.BluetoothAdapter");
		hook(new XBluetoothAdapter("getBondedDevices", XRestriction.cNetwork, new String[] { "BLUETOOTH" }),
				"android.bluetooth.BluetoothAdapter");

		// Camera
		String[] cams = new String[] { "setPreviewCallback", "setPreviewCallbackWithBuffer",
				"setOneShotPreviewCallback", "takePicture" };
		for (String cam : cams)
			hook(new XCamera(cam, XRestriction.cMedia, new String[] { "CAMERA" }), "android.hardware.Camera");

		// Location manager
		hook(new XLocationManager("addNmeaListener", XRestriction.cLocation, new String[] { "ACCESS_COARSE_LOCATION",
				"ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("addProximityAlert", XRestriction.cLocation, new String[] { "ACCESS_COARSE_LOCATION",
				"ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("getLastKnownLocation", XRestriction.cLocation, new String[] {
				"ACCESS_COARSE_LOCATION", "ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("removeUpdates", XRestriction.cLocation, new String[] { "ACCESS_COARSE_LOCATION",
				"ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("requestLocationUpdates", XRestriction.cLocation, new String[] {
				"ACCESS_COARSE_LOCATION", "ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("requestSingleUpdate", XRestriction.cLocation, new String[] {
				"ACCESS_COARSE_LOCATION", "ACCESS_FINE_LOCATION" }), "android.location.LocationManager");

		// Media recorder
		hook(new XMediaRecorder("setOutputFile", XRestriction.cMedia, new String[] { "RECORD_AUDIO", "RECORD_VIDEO" }),
				"android.media.MediaRecorder");

		// Network interface
		String[] nets = new String[] { "getHardwareAddress", "getInetAddresses", "getInterfaceAddresses" };
		for (String net : nets)
			hook(new XNetworkInterface(net, XRestriction.cNetwork, new String[] { "ACCESS_NETWORK_STATE" }),
					"java.net.NetworkInterface");

		// Package manager service
		hook(new XPackageManagerService("getPackageGids", XRestriction.cInternet, new String[] { "INTERNET" }),
				"com.android.server.pm.PackageManagerService");
		hook(new XPackageManagerService("getPackageGids", XRestriction.cStorage, new String[] {
				"READ_EXTERNAL_STORAGE", "WRITE_EXTERNAL_STORAGE" }), "com.android.server.pm.PackageManagerService");

		// Runtime
		hook(new XRuntime("exec", XRestriction.cShell, new String[] {}, "sh"), "java.lang.Runtime");
		hook(new XRuntime("exec", XRestriction.cShell, new String[] {}, "su"), "java.lang.Runtime");
		hook(new XRuntime("exec", XRestriction.cShell, new String[] {}, null), "java.lang.Runtime");
		hook(new XRuntime("load", XRestriction.cShell, new String[] {}, null), "java.lang.Runtime");
		hook(new XRuntime("loadLibrary", XRestriction.cShell, new String[] {}, null), "java.lang.Runtime");

		// Process builder
		hook(new XProcessBuilder("start", XRestriction.cShell, new String[] {}, "sh"), "java.lang.ProcessBuilder");
		hook(new XProcessBuilder("start", XRestriction.cShell, new String[] {}, "su"), "java.lang.ProcessBuilder");
		hook(new XProcessBuilder("start", XRestriction.cShell, new String[] {}, null), "java.lang.ProcessBuilder");

		// Settings secure
		hook(new XSettingsSecure("getString", XRestriction.cIdentification), "android.provider.Settings.Secure");

		// SMS manager
		hook(new XSmsManager("getAllMessagesFromIcc", XRestriction.cMessages, new String[] { "RECEIVE_SMS" }),
				"android.telephony.SmsManager");
		String[] smses = new String[] { "sendDataMessage", "sendMultipartTextMessage", "sendTextMessage" };
		for (String sms : smses)
			hook(new XSmsManager(sms, XRestriction.cCalling, new String[] { "SEND_SMS" }),
					"android.telephony.SmsManager");

		// System properties
		String[] props = new String[] { "ro.gsm.imei", "net.hostname", "ro.serialno", "ro.boot.serialno",
				"ro.boot.wifimacaddr", "ro.boot.btmacaddr" };
		String[] getters = new String[] { "get", "getBoolean", "getInt", "getLong", "getLongString" };
		for (String prop : props)
			for (String getter : getters)
				hook(new XSystemProperties(getter, XRestriction.cIdentification, new String[] {}, prop),
						"android.os.SystemProperties");

		// Telephony
		hook(new XTelephonyManager("disableLocationUpdates", XRestriction.cLocation,
				new String[] { "CONTROL_LOCATION_UPDATES", }), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("enableLocationUpdates", XRestriction.cLocation,
				new String[] { "CONTROL_LOCATION_UPDATES", }), "android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getAllCellInfo", XRestriction.cLocation, new String[] { "ACCESS_COARSE_UPDATES", }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getCellLocation", XRestriction.cLocation, new String[] { "ACCESS_COARSE_LOCATION",
				"ACCESS_FINE_LOCATION" }), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getNeighboringCellInfo", XRestriction.cLocation,
				new String[] { "ACCESS_COARSE_UPDATES" }), "android.telephony.TelephonyManager");

		String[] phones = new String[] { "getDeviceId", "getIsimDomain", "getIsimImpi", "getIsimImpu",
				"getLine1AlphaTag", "getLine1Number", "getMsisdn", "getNetworkCountryIso", "getNetworkOperator",
				"getNetworkOperatorName", "getSimCountryIso", "getSimOperator", "getSimOperatorName",
				"getSimSerialNumber", "getSubscriberId", "getVoiceMailAlphaTag", "getVoiceMailNumber", "listen" };
		for (String phone : phones)
			hook(new XTelephonyManager(phone, XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
					"android.telephony.TelephonyManager");

		// Wi-Fi manager
		String[] wifis = new String[] { "getConfiguredNetworks", "getConnectionInfo", "getDhcpInfo", "getScanResults" };
		for (String wifi : wifis)
			hook(new XWifiManager(wifi, XRestriction.cNetwork, new String[] { "ACCESS_WIFI_STATE" }),
					"android.net.wifi.WifiManager");

		// Intent receive: boot
		hook(new XActivityThread("handleReceiver", XRestriction.cBoot, new String[] { "RECEIVE_BOOT_COMPLETED" },
				Intent.ACTION_BOOT_COMPLETED), "android.app.ActivityThread", false);

		// Intent receive: calling
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, new String[] { "PROCESS_OUTGOING_CALLS" },
				Intent.ACTION_NEW_OUTGOING_CALL), "android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" },
				TelephonyManager.ACTION_PHONE_STATE_CHANGED), "android.app.ActivityThread", false);

		String[] startActivities = new String[] { "startActivities", "startActivity", "startActivityForResult",
				"startActivityFromChild", "startActivityFromFragment", "startActivityIfNeeded" };

		// Intent send: browser
		for (String activity : startActivities)
			hook(new XActivity(activity, XRestriction.cView, new String[] {}, Intent.ACTION_VIEW),
					"android.app.Activity");

		// Intent send: call
		for (String activity : startActivities)
			hook(new XActivity(activity, XRestriction.cCalling, new String[] { "CALL_PHONE" }, Intent.ACTION_CALL),
					"android.app.Activity");

		// Intent send: media
		hook(new XActivity("startActivityForResult", XRestriction.cMedia, new String[] { "CAMERA" },
				MediaStore.ACTION_IMAGE_CAPTURE), "android.app.Activity");
		if (Build.VERSION.SDK_INT >= 17)
			hook(new XActivity("startActivityForResult", XRestriction.cMedia, new String[] { "CAMERA" },
					"android.media.action.IMAGE_CAPTURE_SECURE"), "android.app.Activity");
		hook(new XActivity("startActivityForResult", XRestriction.cMedia, new String[] { "CAMERA" },
				MediaStore.ACTION_VIDEO_CAPTURE), "android.app.Activity");
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		// Log load
		XUtil.log(null, Log.INFO, String.format("load package=%s uid=%d", lpparam.packageName, Process.myUid()));

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (lpparam.packageName.equals(self)) {
			hook(new XUtilHook("isXposedEnabled", null, new String[] {}), lpparam.classLoader, XUtil.class.getName());
			return;
		}

		// Build SERIAL
		if (XRestriction.getRestricted(null, null, Process.myUid(), XRestriction.cIdentification, null, true, false))
			XposedHelpers.setStaticObjectField(Build.class, "SERIAL", XRestriction.getDefacedString());

		// Load browser provider
		if (lpparam.packageName.equals("com.android.browser")) {
			hook(new XContentProvider(XRestriction.cBrowser,
					new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" }, "BrowserProvider"),
					lpparam.classLoader, "com.android.browser.provider.BrowserProvider");
			hook(new XContentProvider(XRestriction.cBrowser,
					new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" }, "BrowserProvider2"),
					lpparam.classLoader, "com.android.browser.provider.BrowserProvider2");
		}

		// Load calendar provider
		else if (lpparam.packageName.equals("com.android.providers.calendar"))
			hook(new XContentProvider(XRestriction.cCalendar, new String[] { "READ_CALENDAR" }, "CalendarProvider2"),
					lpparam.classLoader, "com.android.providers.calendar.CalendarProvider2");

		// Load contacts provider
		else if (lpparam.packageName.equals("com.android.providers.contacts")) {
			hook(new XContentProvider(XRestriction.cContacts, new String[] { "READ_CONTACTS" }, "ContactsProvider2"),
					lpparam.classLoader, "com.android.providers.contacts.ContactsProvider2");
			hook(new XContentProvider(XRestriction.cPhone, new String[] { "READ_CALL_LOG" }, "CallLogProvider"),
					lpparam.classLoader, "com.android.providers.contacts.CallLogProvider");
			hook(new XContentProvider(XRestriction.cMessages, new String[] { "READ_WRITE_ALL_VOICEMAIL" },
					"VoicemailContentProvider"), lpparam.classLoader,
					"com.android.providers.contacts.VoicemailContentProvider");
		}

		// Load telephony provider
		else if (lpparam.packageName.equals("com.android.providers.telephony")) {
			hook(new XContentProvider(XRestriction.cMessages, new String[] { "READ_SMS" }, "SmsProvider"),
					lpparam.classLoader, "com.android.providers.telephony.SmsProvider");
			hook(new XContentProvider(XRestriction.cMessages, new String[] { "READ_SMS" }, "MmsProvider"),
					lpparam.classLoader, "com.android.providers.telephony.MmsProvider");
			hook(new XContentProvider(XRestriction.cMessages, new String[] { "READ_SMS" }, "MmsSmsProvider"),
					lpparam.classLoader, "com.android.providers.telephony.MmsSmsProvider");
			hook(new XContentProvider(XRestriction.cPhone, new String[] { "WRITE_APN_SETTINGS" }, "TelephonyProvider"),
					lpparam.classLoader, "com.android.providers.telephony.TelephonyProvider");
		}
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
						if (param.method.getName().equals(hook.getMethodName()))
							hook.before(param);
					} catch (Throwable ex) {
						XUtil.bug(null, ex);
						throw ex;
					}
				}

				@Override
				protected void afterHookedMethod(MethodHookParam param) throws Throwable {
					// Throw any exception
					param.getResultOrThrowable();
					try {
						if (param.method.getName().equals(hook.getMethodName())) {
							hook.after(param);
						}
					} catch (Throwable ex) {
						XUtil.bug(null, ex);
						throw ex;
					}
				}
			};

			// Find class
			Class<?> hookClass = findClass(className, classLoader);
			if (hookClass == null) {
				XUtil.log(hook, Log.WARN,
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
				XUtil.log(
						hook,
						Log.WARN,
						String.format("%s: method not found: %s.%s", AndroidAppHelper.currentPackageName(),
								hookClass.getName(), hook.getMethodName()));
				return;
			}

			// Log
			for (XC_MethodHook.Unhook unhook : hookSet) {
				XUtil.log(
						hook,
						Log.INFO,
						String.format("%s: hooked %s.%s (%d)", AndroidAppHelper.currentPackageName(),
								hookClass.getName(), unhook.getHookedMethod().getName(), hookSet.size()));
				break;
			}
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
		}
	}
}

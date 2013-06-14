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
		hook(new XApplicationPackageManager("getInstalledApplications", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");
		hook(new XApplicationPackageManager("getInstalledPackages", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");
		hook(new XApplicationPackageManager("getInstalledThemePackages", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");
		hook(new XApplicationPackageManager("getPreferredPackages", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");
		hook(new XApplicationPackageManager("queryBroadcastReceivers", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");
		hook(new XApplicationPackageManager("queryContentProviders", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");
		hook(new XApplicationPackageManager("queryIntentActivities", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");
		hook(new XApplicationPackageManager("queryIntentActivityOptions", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");
		hook(new XApplicationPackageManager("queryIntentServices", XRestriction.cSystem, new String[] {}),
				"android.app.ApplicationPackageManager");

		// Audio record
		hook(new XCamera("startRecording", XRestriction.cMedia, new String[] { "RECORD_AUDIO" }),
				"android.media.AudioRecord");

		// Camera
		hook(new XCamera("setPreviewCallback", XRestriction.cMedia, new String[] { "CAMERA" }),
				"android.hardware.Camera");
		hook(new XCamera("setPreviewCallbackWithBuffer", XRestriction.cMedia, new String[] { "CAMERA" }),
				"android.hardware.Camera");
		hook(new XCamera("setOneShotPreviewCallback", XRestriction.cMedia, new String[] { "CAMERA" }),
				"android.hardware.Camera");
		hook(new XCamera("takePicture", XRestriction.cMedia, new String[] { "CAMERA" }), "android.hardware.Camera");

		// Connectivity manager
		hook(new XConnectivityManager("getActiveNetworkInfo", XRestriction.cNetwork,
				new String[] { "ACCESS_NETWORK_STATE" }), "android.net.ConnectivityManager");
		hook(new XConnectivityManager("getActiveNetworkInfoForUid", XRestriction.cNetwork,
				new String[] { "ACCESS_NETWORK_STATE" }), "android.net.ConnectivityManager");
		hook(new XConnectivityManager("getAllNetworkInfo", XRestriction.cNetwork,
				new String[] { "ACCESS_NETWORK_STATE" }), "android.net.ConnectivityManager");
		hook(new XConnectivityManager("getNetworkInfo", XRestriction.cNetwork, new String[] { "ACCESS_NETWORK_STATE" }),
				"android.net.ConnectivityManager");

		// Location manager
		hook(new XLocationManager("addNmeaListener", XRestriction.cLocation, new String[] { "ACCESS_COARSE_LOCATION",
				"ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("addProximityAlert", XRestriction.cLocation, new String[] { "ACCESS_COARSE_LOCATION",
				"ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("getLastKnownLocation", XRestriction.cLocation, new String[] {
				"ACCESS_COARSE_LOCATION", "ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("requestLocationUpdates", XRestriction.cLocation, new String[] {
				"ACCESS_COARSE_LOCATION", "ACCESS_FINE_LOCATION" }), "android.location.LocationManager");
		hook(new XLocationManager("requestSingleUpdate", XRestriction.cLocation, new String[] {
				"ACCESS_COARSE_LOCATION", "ACCESS_FINE_LOCATION" }), "android.location.LocationManager");

		// Media recorder
		hook(new XMediaRecorder("setOutputFile", XRestriction.cMedia, new String[] { "RECORD_AUDIO", "RECORD_VIDEO" }),
				"android.media.MediaRecorder");

		// Network interface
		hook(new XNetworkInterface("getByInetAddress", XRestriction.cNetwork, new String[] { "ACCESS_NETWORK_STATE" }),
				"java.net.NetworkInterface");
		hook(new XNetworkInterface("getByName", XRestriction.cNetwork, new String[] { "ACCESS_NETWORK_STATE" }),
				"java.net.NetworkInterface");
		hook(new XNetworkInterface("getHardwareAddress", XRestriction.cNetwork, new String[] { "ACCESS_NETWORK_STATE" }),
				"java.net.NetworkInterface");
		hook(new XNetworkInterface("getInetAddresses", XRestriction.cNetwork, new String[] { "ACCESS_NETWORK_STATE" }),
				"java.net.NetworkInterface");
		hook(new XNetworkInterface("getInterfaceAddresses", XRestriction.cNetwork,
				new String[] { "ACCESS_NETWORK_STATE" }), "java.net.NetworkInterface");
		hook(new XNetworkInterface("getNetworkInterfaces", XRestriction.cNetwork,
				new String[] { "ACCESS_NETWORK_STATE" }), "java.net.NetworkInterface");

		// Package manager service
		if (XRestriction.cPro) {
			hook(new XPackageManagerService("getPackageGids", XRestriction.cInternet, new String[] { "INTERNET" }),
					"com.android.server.pm.PackageManagerService");
			hook(new XPackageManagerService("getPackageGids", XRestriction.cStorage, new String[] {
					"READ_EXTERNAL_STORAGE", "WRITE_EXTERNAL_STORAGE" }), "com.android.server.pm.PackageManagerService");
		}

		// Settings secure
		hook(new XSettingsSecure("getString", XRestriction.cIdentification), "android.provider.Settings.Secure");

		// SMS manager
		hook(new XSmsManager("getAllMessagesFromIcc", XRestriction.cMessages, new String[] { "RECEIVE_SMS" }),
				"android.telephony.SmsManager");
		if (XRestriction.cPro) {
			hook(new XSmsManager("sendDataMessage", XRestriction.cCalling, new String[] { "SEND_SMS" }),
					"android.telephony.SmsManager");
			hook(new XSmsManager("sendMultipartTextMessage", XRestriction.cCalling, new String[] { "SEND_SMS" }),
					"android.telephony.SmsManager");
			hook(new XSmsManager("sendTextMessage", XRestriction.cCalling, new String[] { "SEND_SMS" }),
					"android.telephony.SmsManager");
		}

		// Telephony
		hook(new XTelephonyManager("disableLocationUpdates", XRestriction.cLocation,
				new String[] { "CONTROL_LOCATION_UPDATES", }), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("enableLocationUpdates", XRestriction.cLocation,
				new String[] { "CONTROL_LOCATION_UPDATES", }), "android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getAllCellInfo", XRestriction.cLocation, new String[] { "ACCESS_COARSE_UPDATES", }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getCellLocation", XRestriction.cLocation, new String[] { "ACCESS_COARSE_LOCATION",
				"ACCESS_FINE_LOCATION" }), "android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getDeviceId", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getIsimDomain", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getIsimImpi", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getIsimImpu", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getLine1AlphaTag", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getLine1Number", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getMsisdn", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getNeighboringCellInfo", XRestriction.cLocation,
				new String[] { "ACCESS_COARSE_UPDATES" }), "android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getNetworkCountryIso", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getNetworkOperator", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getNetworkOperatorName", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getSimCountryIso", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getSimOperator", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getSimOperatorName", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getSimSerialNumber", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getSubscriberId", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");

		hook(new XTelephonyManager("getVoiceMailAlphaTag", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getVoiceMailNumber", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" }),
				"android.telephony.TelephonyManager");

		hook(new XTelephonyManager("listen", XRestriction.cPhone, new String[] { "READ_PHONE_STATE", }),
				"android.telephony.TelephonyManager");

		// Wi-Fi manager
		hook(new XWifiManager("getConfiguredNetworks", XRestriction.cNetwork, new String[] { "ACCESS_WIFI_STATE" }),
				"android.net.wifi.WifiManager");
		hook(new XWifiManager("getConnectionInfo", XRestriction.cNetwork, new String[] { "ACCESS_WIFI_STATE" }),
				"android.net.wifi.WifiManager");
		hook(new XWifiManager("getDhcpInfo", XRestriction.cNetwork, new String[] { "ACCESS_WIFI_STATE" }),
				"android.net.wifi.WifiManager");
		hook(new XWifiManager("getScanResults", XRestriction.cNetwork, new String[] { "ACCESS_WIFI_STATE" }),
				"android.net.wifi.WifiManager");

		// Intent receive: calling
		hook(new XActivityThread("handleReceiver", XRestriction.cBoot, new String[] { "RECEIVE_BOOT_COMPLETED" },
				Intent.ACTION_BOOT_COMPLETED), "android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, new String[] { "PROCESS_OUTGOING_CALLS" },
				Intent.ACTION_NEW_OUTGOING_CALL), "android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, new String[] { "READ_PHONE_STATE" },
				TelephonyManager.ACTION_PHONE_STATE_CHANGED), "android.app.ActivityThread", false);

		// Intent send: call
		if (XRestriction.cPro) {
			hook(new XActivity("startActivities", XRestriction.cCalling, new String[] { "CALL_PHONE" },
					Intent.ACTION_CALL), "android.app.Activity");
			hook(new XActivity("startActivity", XRestriction.cCalling, new String[] { "CALL_PHONE" },
					Intent.ACTION_CALL), "android.app.Activity");
			hook(new XActivity("startActivityForResult", XRestriction.cCalling, new String[] { "CALL_PHONE" },
					Intent.ACTION_CALL), "android.app.Activity");
			hook(new XActivity("startActivityFromChild", XRestriction.cCalling, new String[] { "CALL_PHONE" },
					Intent.ACTION_CALL), "android.app.Activity");
			hook(new XActivity("startActivityFromFragment", XRestriction.cCalling, new String[] { "CALL_PHONE" },
					Intent.ACTION_CALL), "android.app.Activity");
			hook(new XActivity("startActivityIfNeeded", XRestriction.cCalling, new String[] { "CALL_PHONE" },
					Intent.ACTION_CALL), "android.app.Activity");
		}

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

		// Load browser provider
		if (lpparam.packageName.equals("com.android.browser")) {
			hook(new XContentProvider(XRestriction.cBrowser, new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" }),
					lpparam.classLoader, "com.android.browser.provider.BrowserProvider");
			hook(new XContentProvider(XRestriction.cBrowser, new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" }),
					lpparam.classLoader, "com.android.browser.provider.BrowserProvider2");
		}

		// Load calendar provider
		else if (lpparam.packageName.equals("com.android.providers.calendar"))
			hook(new XContentProvider(XRestriction.cCalendar, new String[] { "READ_CALENDAR" }), lpparam.classLoader,
					"com.android.providers.calendar.CalendarProvider2");

		// Load contacts provider
		else if (lpparam.packageName.equals("com.android.providers.contacts")) {
			hook(new XContentProvider(XRestriction.cContacts, new String[] { "READ_CONTACTS" }), lpparam.classLoader,
					"com.android.providers.contacts.ContactsProvider2");
			hook(new XContentProvider(XRestriction.cPhone, new String[] { "READ_CALL_LOG" }), lpparam.classLoader,
					"com.android.providers.contacts.CallLogProvider");
			hook(new XContentProvider(XRestriction.cMessages, new String[] { "READ_WRITE_ALL_VOICEMAIL" }),
					lpparam.classLoader, "com.android.providers.contacts.VoicemailContentProvider");
		}

		// Load telephony provider
		else if (lpparam.packageName.equals("com.android.providers.telephony")) {
			hook(new XContentProvider(XRestriction.cMessages, new String[] { "READ_SMS" }), lpparam.classLoader,
					"com.android.providers.telephony.SmsProvider");
			hook(new XContentProvider(XRestriction.cMessages, new String[] { "READ_SMS" }), lpparam.classLoader,
					"com.android.providers.telephony.MmsProvider");
			hook(new XContentProvider(XRestriction.cMessages, new String[] { "READ_SMS" }), lpparam.classLoader,
					"com.android.providers.telephony.MmsSmsProvider");
			hook(new XContentProvider(XRestriction.cPhone, new String[] { "WRITE_APN_SETTINGS" }), lpparam.classLoader,
					"com.android.providers.telephony.TelephonyProvider");
		}

		// Load settings
		else if (lpparam.packageName.equals("com.android.settings"))
			hook(new XAppDetails("refreshUi"), lpparam.classLoader,
					"com.android.settings.applications.InstalledAppDetails", false);
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

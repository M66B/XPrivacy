package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.callbacks.XC_LoadPackage.LoadPackageParam;
import de.robv.android.xposed.XC_MethodHook;
import static de.robv.android.xposed.XposedHelpers.findClass;

public class XPrivacy implements IXposedHookLoadPackage {

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {

		// Log load
		XUtil.log(null, Log.INFO, String.format("load package=%s", lpparam.packageName));

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (lpparam.packageName.equals(self))
			return;

		// Check version
		if (Build.VERSION.SDK_INT != 16)
			XUtil.log(null, Log.WARN, String.format("Build version %d", Build.VERSION.SDK_INT));

		// Location manager
		hook(new XLocationManager("addGpsStatusListener", XPermissions.cLocation), lpparam,
				"android.location.LocationManager");
		hook(new XLocationManager("addNmeaListener", XPermissions.cLocation), lpparam,
				"android.location.LocationManager");
		hook(new XLocationManager("addProximityAlert", XPermissions.cLocation), lpparam,
				"android.location.LocationManager");
		hook(new XLocationManager("getLastKnownLocation", XPermissions.cLocation), lpparam,
				"android.location.LocationManager");
		hook(new XLocationManager("requestLocationUpdates", XPermissions.cLocation), lpparam,
				"android.location.LocationManager");
		hook(new XLocationManager("requestSingleUpdate", XPermissions.cLocation), lpparam,
				"android.location.LocationManager");
		// requestLocationUpdates not working for all apps for unknown reasons
		hook(new XLocationManager("_requestLocationUpdates", XPermissions.cLocation), lpparam,
				"android.location.LocationManager", false);

		// Settings secure
		hook(new XSettingsSecure("getString", XPermissions.cIdentification), lpparam,
				"android.provider.Settings.Secure");

		// Telephony
		hook(new XTelephonyManager("getDeviceId", XPermissions.cPhone), lpparam, "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getLine1Number", XPermissions.cPhone), lpparam,
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getMsisdn", XPermissions.cPhone), lpparam, "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getSimSerialNumber", XPermissions.cPhone), lpparam,
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getSubscriberId", XPermissions.cPhone), lpparam,
				"android.telephony.TelephonyManager");
		hook(new XTelephonyManager("listen", XPermissions.cPhone), lpparam, "android.telephony.TelephonyManager");

		// Load browser provider
		if (lpparam.packageName.equals("com.android.browser")) {
			hook(new XContentProvider(XPermissions.cBrowser), lpparam, "com.android.browser.provider.BrowserProvider");
			hook(new XContentProvider(XPermissions.cBrowser), lpparam, "com.android.browser.provider.BrowserProvider2");
		}

		// Load calendar provider
		else if (lpparam.packageName.equals("com.android.providers.calendar"))
			hook(new XContentProvider(XPermissions.cCalendar), lpparam,
					"com.android.providers.calendar.CalendarProvider2");

		// Load contacts provider
		else if (lpparam.packageName.equals("com.android.providers.contacts")) {
			hook(new XContentProvider(XPermissions.cPhone), lpparam, "com.android.providers.contacts.CallLogProvider",
					true);
			hook(new XContentProvider(XPermissions.cContacts), lpparam,
					"com.android.providers.contacts.ContactsProvider2");
			hook(new XContentProvider(XPermissions.cVoicemail), lpparam,
					"com.android.providers.contacts.VoicemailContentProvider");
		}

		// Load telephony provider
		else if (lpparam.packageName.equals("com.android.providers.telephony")) {
			hook(new XContentProvider(XPermissions.cMessages), lpparam, "com.android.providers.telephony.SmsProvider");
			hook(new XContentProvider(XPermissions.cMessages), lpparam, "com.android.providers.telephony.MmsProvider");
			hook(new XContentProvider(XPermissions.cMessages), lpparam,
					"com.android.providers.telephony.MmsSmsProvider");
			// com.android.providers.telephony.TelephonyProvider
		}

		// Load settings
		else if (lpparam.packageName.equals("com.android.settings"))
			hook(new XAppDetails("refreshUi", null), lpparam, "com.android.settings.applications.InstalledAppDetails",
					false);
	}

	private void hook(final XHook hook, final LoadPackageParam lpparam, String className) {
		hook(hook, lpparam, className, true);
	}

	private void hook(final XHook hook, final LoadPackageParam lpparam, String className, boolean visible) {
		try {
			// Create hook
			XC_MethodHook methodHook = new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
					try {
						hook.before(param);
					} catch (Throwable ex) {
						XUtil.bug(null, ex);
						throw ex;
					}
				}

				@Override
				protected void afterHookedMethod(MethodHookParam param) throws Throwable {
					try {
						hook.after(param);
					} catch (Throwable ex) {
						XUtil.bug(null, ex);
						throw ex;
					}
				}
			};

			// Find class
			Class<?> hookClass = findClass(className, lpparam.classLoader);
			if (hookClass == null) {
				XUtil.log(hook, Log.WARN, "Class not found: " + className);
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
				XUtil.log(hook, Log.WARN, "Method not found: " + hook.getMethodName());
				return;
			}

			// Log
			for (XC_MethodHook.Unhook unhook : hookSet) {
				XUtil.log(hook, Log.INFO, String.format("%s: hooked %s.%s (%d)", lpparam.packageName,
						hookClass.getName(), unhook.getHookedMethod().getName(), hookSet.size()));
				break;
			}
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
		}
	}
}

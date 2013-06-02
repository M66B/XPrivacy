package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

import android.app.AndroidAppHelper;
import android.content.Intent;
import android.os.Build;
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

	public void initZygote(StartupParam startupParam) throws Throwable {
		// Check version
		if (Build.VERSION.SDK_INT != 16)
			XUtil.log(null, Log.WARN, String.format("Build version %d", Build.VERSION.SDK_INT));

		// Workaround bug in Xposed
		hook(new XLocationManager("_requestLocationUpdates", XRestriction.cLocation),
				"android.location.LocationManager", false);
		hook(new XTelephonyManager("_listen", XRestriction.cPhone), "android.telephony.TelephonyManager", false);

		// Location manager
		hook(new XLocationManager("addGpsStatusListener", XRestriction.cLocation), "android.location.LocationManager");
		hook(new XLocationManager("addNmeaListener", XRestriction.cLocation), "android.location.LocationManager");
		hook(new XLocationManager("addProximityAlert", XRestriction.cLocation), "android.location.LocationManager");
		hook(new XLocationManager("getLastKnownLocation", XRestriction.cLocation), "android.location.LocationManager");
		hook(new XLocationManager("requestLocationUpdates", XRestriction.cLocation), "android.location.LocationManager");
		hook(new XLocationManager("requestSingleUpdate", XRestriction.cLocation), "android.location.LocationManager");

		// Settings secure
		hook(new XSettingsSecure("getString", XRestriction.cIdentification), "android.provider.Settings.Secure");

		// Telephony
		hook(new XTelephonyManager("getDeviceId", XRestriction.cPhone), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getLine1Number", XRestriction.cPhone), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getMsisdn", XRestriction.cPhone), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getSimSerialNumber", XRestriction.cPhone), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("getSubscriberId", XRestriction.cPhone), "android.telephony.TelephonyManager");
		hook(new XTelephonyManager("listen", XRestriction.cPhone), "android.telephony.TelephonyManager");

		// Intent/calling
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, Intent.ACTION_NEW_OUTGOING_CALL),
				"android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, TelephonyManager.ACTION_PHONE_STATE_CHANGED),
				"android.app.ActivityThread", false);

		// Intent/media
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, MediaStore.ACTION_IMAGE_CAPTURE),
				"android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, MediaStore.ACTION_IMAGE_CAPTURE_SECURE),
				"android.app.ActivityThread", false);
		hook(new XActivityThread("handleReceiver", XRestriction.cPhone, MediaStore.ACTION_VIDEO_CAPTURE),
				"android.app.ActivityThread", false);
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		// Log load
		XUtil.log(null, Log.INFO, String.format("load %s", lpparam.packageName));

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (lpparam.packageName.equals(self))
			return;

		// Load browser provider
		if (lpparam.packageName.equals("com.android.browser")) {
			hook(new XContentProvider(XRestriction.cBrowser), lpparam.classLoader,
					"com.android.browser.provider.BrowserProvider");
			hook(new XContentProvider(XRestriction.cBrowser), lpparam.classLoader,
					"com.android.browser.provider.BrowserProvider2");
		}

		// Load calendar provider
		else if (lpparam.packageName.equals("com.android.providers.calendar"))
			hook(new XContentProvider(XRestriction.cCalendar), lpparam.classLoader,
					"com.android.providers.calendar.CalendarProvider2");

		// Load contacts provider
		else if (lpparam.packageName.equals("com.android.providers.contacts")) {
			hook(new XContentProvider(XRestriction.cPhone), lpparam.classLoader,
					"com.android.providers.contacts.CallLogProvider");
			hook(new XContentProvider(XRestriction.cContacts), lpparam.classLoader,
					"com.android.providers.contacts.ContactsProvider2");
			hook(new XContentProvider(XRestriction.cMessages), lpparam.classLoader,
					"com.android.providers.contacts.VoicemailContentProvider");
		}

		// Load telephony provider
		else if (lpparam.packageName.equals("com.android.providers.telephony")) {
			hook(new XContentProvider(XRestriction.cMessages), lpparam.classLoader,
					"com.android.providers.telephony.SmsProvider");
			hook(new XContentProvider(XRestriction.cMessages), lpparam.classLoader,
					"com.android.providers.telephony.MmsProvider");
			hook(new XContentProvider(XRestriction.cMessages), lpparam.classLoader,
					"com.android.providers.telephony.MmsSmsProvider");
			// com.android.providers.telephony.TelephonyProvider
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

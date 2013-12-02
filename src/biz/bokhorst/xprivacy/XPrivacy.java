package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import android.annotation.SuppressLint;
import android.app.AndroidAppHelper;
import android.os.Build;
import android.os.Process;
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

	@SuppressLint("InlinedApi")
	public void initZygote(StartupParam startupParam) throws Throwable {
		// Log load
		Util.log(null, Log.INFO, String.format("Load %s", startupParam.modulePath));

		// Account manager
		hookAll(XAccountManager.getInstances());

		// Activity manager
		hookAll(XActivityManager.getInstances());

		// App widget manager
		hookAll(XAppWidgetManager.getInstances());

		// Application
		hookAll(XApplication.getInstances());

		// Application package manager
		hookAll(XApplicationPackageManager.getInstances());

		// Audio record
		hookAll(XAudioRecord.getInstances());

		// Bluetooth adapter
		hookAll(XBluetoothAdapter.getInstances());

		// Bluetooth device
		hookAll(XBluetoothDevice.getInstances());

		// Camera
		hookAll(XCamera.getInstances());

		// Connectivity manager
		hookAll(XConnectivityManager.getInstances());

		// Context wrapper
		hookAll(XContextWrapper.getInstances());

		// Environment
		hookAll(XEnvironment.getInstances());

		// InetAddress
		hookAll(XInetAddress.getInstances());

		// InputDevice
		hookAll(XInputDevice.getInstances());

		// IO bridge
		hookAll(XIoBridge.getInstances());

		// Location manager
		hookAll(XLocationManager.getInstances());

		// Media recorder
		hookAll(XMediaRecorder.getInstances());

		// Network info
		hookAll(XNetworkInfo.getInstances());

		// Network interface
		hookAll(XNetworkInterface.getInstances());

		// NFC adapter
		hookAll(XNfcAdapter.getInstances());

		// Package manager service
		hookAll(XPackageManagerService.getInstances());

		// Process builder
		hookAll(XProcessBuilder.getInstances());

		// Runtime
		hookAll(XRuntime.getInstances());

		// Sensor manager
		hookAll(XSensorManager.getInstances());

		// Settings secure
		hookAll(XSettingsSecure.getInstances());

		// SMS manager
		hookAll(XSmsManager.getInstances());

		// System properties
		hookAll(XSystemProperties.getInstances());

		// Telephony
		hookAll(XTelephonyManager.getInstances());

		// Web view
		hookAll(XWebView.getInstances());

		// Wi-Fi manager
		hookAll(XWifiManager.getInstances());

		// Intent receive
		hookAll(XActivityThread.getInstances());

		// Intent send
		hookAll(XActivity.getInstances());
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		// Log load
		Util.log(null, Log.INFO, String.format("Load package=%s uid=%d", lpparam.packageName, Process.myUid()));

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (lpparam.packageName.equals(self)) {
			hookAll(XUtilHook.getInstances(), lpparam.classLoader);
			return;
		}

		// Build SERIAL
		if (PrivacyManager.getRestricted(null, null, Process.myUid(), PrivacyManager.cIdentification, "SERIAL", true,
				false))
			XposedHelpers.setStaticObjectField(Build.class, "SERIAL",
					PrivacyManager.getDefacedProp(Process.myUid(), "SERIAL"));

		// Providers
		hookAll(XContentProvider.getInstances(lpparam.packageName), lpparam.classLoader);

		// Advertising Id
		try {
			Class.forName("com.google.android.gms.ads.identifier.AdvertisingIdClient$Info", false, lpparam.classLoader);
			hookAll(XAdvertisingIdClientInfo.getInstances(), lpparam.classLoader);
		} catch (Throwable ex) {
		}

		// Google auth
		try {
			Class.forName("com.google.android.gms.auth.GoogleAuthUtil", false, lpparam.classLoader);
			hookAll(XGoogleAuthUtil.getInstances(), lpparam.classLoader);
		} catch (Throwable ex) {
		}

		// Location client
		try {
			Class.forName("com.google.android.gms.location.LocationClient", false, lpparam.classLoader);
			hookAll(XLocationClient.getInstances(), lpparam.classLoader);
		} catch (Throwable ex) {
		}
	}

	public static void hookAll(List<XHook> listHook) {
		for (XHook hook : listHook)
			hook(hook);
	}

	private static void hookAll(List<XHook> listHook, ClassLoader classLoader) {
		for (XHook hook : listHook)
			hook(hook, classLoader);
	}

	private static void hook(XHook hook) {
		hook(hook, null);
	}

	private static void hook(final XHook hook, ClassLoader classLoader) {
		// Check SDK version
		if (Build.VERSION.SDK_INT < hook.getSdk())
			return;

		try {
			// Create hook method
			XC_MethodHook methodHook = new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
					try {
						if (Process.myUid() <= 0)
							return;
						hook.before(param);
					} catch (Throwable ex) {
						Util.bug(null, ex);
						throw ex;
					}
				}

				@Override
				protected void afterHookedMethod(MethodHookParam param) throws Throwable {
					if (!param.hasThrowable())
						try {
							if (Process.myUid() <= 0)
								return;
							hook.after(param);
						} catch (Throwable ex) {
							Util.bug(null, ex);
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
				String packageName = AndroidAppHelper.currentPackageName();
				String className = hookClass.getName();
				String methodName = unhook.getHookedMethod().getName();
				String restrictionName = hook.getRestrictionName();
				if (className.equals(methodName))
					methodName = "constructor";
				Util.log(hook, Log.INFO, String.format("%s: hooked %s.%s/%s (%d)", packageName, className, methodName,
						restrictionName, hookSet.size()));
				break;
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}
}

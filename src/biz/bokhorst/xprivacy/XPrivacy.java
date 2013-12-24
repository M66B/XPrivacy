package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import android.annotation.SuppressLint;
import android.app.AndroidAppHelper;
import android.content.Context;
import android.os.Binder;
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

		// App widget manager
		hookAll(XAppWidgetManager.getInstances());

		// Application
		hookAll(XApplication.getInstances());

		// Application package manager
		hookAll(XApplicationPackageManager.getInstances());

		// Audio record
		hookAll(XAudioRecord.getInstances());

		// Binder device
		hookAll(XBinder.getInstances());

		// Bluetooth adapater
		if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.JELLY_BEAN_MR1)
			hookAll(XBluetoothAdapter.getInstances());

		// Bluetooth device
		hookAll(XBluetoothDevice.getInstances());

		// Camera
		hookAll(XCamera.getInstances());

		// Context wrapper
		hookAll(XContextImpl.getInstances());

		// Environment
		hookAll(XEnvironment.getInstances());

		// InetAddress
		hookAll(XInetAddress.getInstances());

		// InputDevice
		hookAll(XInputDevice.getInstances());

		// IO bridge
		hookAll(XIoBridge.getInstances());

		// Media recorder
		hookAll(XMediaRecorder.getInstances());

		// Network info
		hookAll(XNetworkInfo.getInstances());

		// Network interface
		hookAll(XNetworkInterface.getInstances());

		// NFC adapter
		hookAll(XNfcAdapter.getInstances());

		// Package manager service
		hookAll(XProcess.getInstances());

		// Process builder
		hookAll(XProcessBuilder.getInstances());

		// Runtime
		hookAll(XRuntime.getInstances());

		// Service
		hookAll(XService.getInstances());

		// Settings secure
		hookAll(XSettingsSecure.getInstances());

		// SMS manager
		hookAll(XSmsManager.getInstances());

		// System properties
		hookAll(XSystemProperties.getInstances());

		// Thread
		hookAll(XThread.getInstances());

		// Web view
		hookAll(XWebView.getInstances());

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

	private static boolean mAccountManagerHooked = false;
	private static boolean mActivityManagerHooked = false;
	private static boolean mBluetoothAdapterHooked = false;
	private static boolean mClipboardManagerHooked = false;
	private static boolean mConnectivityManagerHooked = false;
	private static boolean mLocationManagerHooked = false;
	private static boolean mSensorManagerHooked = false;
	private static boolean mTelephonyManagerHooked = false;
	private static boolean mWindowManagerHooked = false;
	private static boolean mWiFiManagerHooked = false;

	public static void handleGetSystemService(XHook hook, String name, Object instance) {
		Util.log(hook, Log.INFO,
				"getSystemService " + name + "=" + instance.getClass().getName() + " uid=" + Binder.getCallingUid());

		if (name.equals(Context.ACCOUNT_SERVICE)) {
			// Account manager
			if (!mAccountManagerHooked) {
				hookAll(XAccountManager.getInstances(instance));
				mAccountManagerHooked = true;
			}
		} else if (name.equals(Context.ACTIVITY_SERVICE)) {
			// Activity manager
			if (!mActivityManagerHooked) {
				hookAll(XActivityManager.getInstances(instance));
				mActivityManagerHooked = true;
			}
		} else if (name.equals(Context.BLUETOOTH_SERVICE)) {
			// Bluetooth adapter
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2)
				if (!mBluetoothAdapterHooked) {
					hookAll(XBluetoothAdapter.getInstances(instance));
					mBluetoothAdapterHooked = true;
				}
		} else if (name.equals(Context.CLIPBOARD_SERVICE)) {
			// Clipboard manager
			if (!mClipboardManagerHooked) {
				XPrivacy.hookAll(XClipboardManager.getInstances(instance));
				mClipboardManagerHooked = true;
			}
		} else if (name.equals(Context.CONNECTIVITY_SERVICE)) {
			// Connectivity manager
			if (!mConnectivityManagerHooked) {
				hookAll(XConnectivityManager.getInstances(instance));
				mConnectivityManagerHooked = true;
			}
		} else if (name.equals(Context.LOCATION_SERVICE)) {
			// Location manager
			if (!mLocationManagerHooked) {
				hookAll(XLocationManager.getInstances(instance));
				mLocationManagerHooked = true;
			}
		} else if (name.equals(Context.SENSOR_SERVICE)) {
			// Sensor manager
			if (!mSensorManagerHooked) {
				hookAll(XSensorManager.getInstances(instance));
				mSensorManagerHooked = true;
			}
		} else if (name.equals(Context.TELEPHONY_SERVICE)) {
			// Telephony manager
			if (!mTelephonyManagerHooked) {
				hookAll(XTelephonyManager.getInstances(instance));
				mTelephonyManagerHooked = true;
			}
		} else if (name.equals(Context.WINDOW_SERVICE)) {
			// Window manager
			if (!mWindowManagerHooked) {
				XPrivacy.hookAll(XWindowManager.getInstances(instance));
				mWindowManagerHooked = true;
			}
		} else if (name.equals(Context.WIFI_SERVICE)) {
			// WiFi manager
			if (!mWiFiManagerHooked) {
				XPrivacy.hookAll(XWifiManager.getInstances(instance));
				mWiFiManagerHooked = true;
			}
		}
	}

	public static void hookAll(List<XHook> listHook) {
		for (XHook hook : listHook)
			hook(hook);
	}

	public static void hookAll(List<XHook> listHook, ClassLoader classLoader) {
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

			Class<?> clazz = hookClass;
			while (clazz != null) {
				if (hook.getMethodName() == null) {
					for (Constructor<?> constructor : clazz.getDeclaredConstructors())
						if (Modifier.isPublic(constructor.getModifiers()) ? hook.isVisible() : !hook.isVisible())
							hookSet.add(XposedBridge.hookMethod(constructor, methodHook));
				} else
					for (Method method : clazz.getDeclaredMethods())
						if (method.getName().equals(hook.getMethodName())
								&& (Modifier.isPublic(method.getModifiers()) ? hook.isVisible() : !hook.isVisible()))
							hookSet.add(XposedBridge.hookMethod(method, methodHook));
				clazz = (hookSet.isEmpty() ? clazz.getSuperclass() : null);
			}

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
				String className = unhook.getHookedMethod().getDeclaringClass().getName();
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

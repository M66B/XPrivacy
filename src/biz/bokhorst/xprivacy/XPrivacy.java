package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
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

@SuppressLint("DefaultLocale")
public class XPrivacy implements IXposedHookLoadPackage, IXposedHookZygoteInit {
	private static String mSecret = null;
	private static boolean mAccountManagerHooked = false;
	private static boolean mActivityManagerHooked = false;
	private static boolean mClipboardManagerHooked = false;
	private static boolean mConnectivityManagerHooked = false;
	private static boolean mLocationManagerHooked = false;
	private static boolean mPackageManagerHooked = false;
	private static boolean mSensorManagerHooked = false;
	private static boolean mTelephonyManagerHooked = false;
	private static boolean mWindowManagerHooked = false;
	private static boolean mWiFiManagerHooked = false;
	private static List<String> mListHookError = new ArrayList<String>();

	// http://developer.android.com/reference/android/Manifest.permission.html

	@SuppressLint("InlinedApi")
	public void initZygote(StartupParam startupParam) throws Throwable {
		// Check for LBE security master
		if (Util.hasLBE())
			return;

		Util.log(null, Log.INFO, String.format("Load %s", startupParam.modulePath));

		// Generate secret
		mSecret = Long.toHexString(new Random().nextLong());

		PrivacyService.setupDatabase();

		// System server
		try {
			// frameworks/base/services/java/com/android/server/SystemServer.java
			Class<?> cSystemServer = findClass("com.android.server.SystemServer", null);
			Method mMain = cSystemServer.getDeclaredMethod("main", String[].class);
			XposedBridge.hookMethod(mMain, new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
					PrivacyService.register(mListHookError, mSecret);
				}
			});
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		// App widget manager
		hookAll(XAppWidgetManager.getInstances(), mSecret);

		// Application
		hookAll(XApplication.getInstances(), mSecret);

		// Audio record
		hookAll(XAudioRecord.getInstances(), mSecret);

		// Binder device
		hookAll(XBinder.getInstances(), mSecret);

		// Bluetooth adapater
		hookAll(XBluetoothAdapter.getInstances(), mSecret);

		// Bluetooth device
		hookAll(XBluetoothDevice.getInstances(), mSecret);

		// Camera
		hookAll(XCamera.getInstances(), mSecret);

		// Content resolver
		hookAll(XContentResolver.getInstances(), mSecret);

		// Context wrapper
		hookAll(XContextImpl.getInstances(), mSecret);

		// Environment
		hookAll(XEnvironment.getInstances(), mSecret);

		// InetAddress
		hookAll(XInetAddress.getInstances(), mSecret);

		// InputDevice
		hookAll(XInputDevice.getInstances(), mSecret);

		// IO bridge
		hookAll(XIoBridge.getInstances(), mSecret);

		// Media recorder
		hookAll(XMediaRecorder.getInstances(), mSecret);

		// Network info
		hookAll(XNetworkInfo.getInstances(), mSecret);

		// Network interface
		hookAll(XNetworkInterface.getInstances(), mSecret);

		// NFC adapter
		hookAll(XNfcAdapter.getInstances(), mSecret);

		// Package manager service
		hookAll(XProcess.getInstances(), mSecret);

		// Process builder
		hookAll(XProcessBuilder.getInstances(), mSecret);

		// Resources
		hookAll(XResources.getInstances(), mSecret);

		// Runtime
		hookAll(XRuntime.getInstances(), mSecret);

		// Settings secure
		hookAll(XSettingsSecure.getInstances(), mSecret);

		// SMS manager
		hookAll(XSmsManager.getInstances(), mSecret);

		// System properties
		hookAll(XSystemProperties.getInstances(), mSecret);

		// Web view
		hookAll(XWebView.getInstances(), mSecret);

		// Intent receive
		hookAll(XActivityThread.getInstances(), mSecret);

		// Intent send
		hookAll(XActivity.getInstances(), mSecret);
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		// Check for LBE security master
		if (Util.hasLBE())
			return;

		// Log load
		Util.log(null, Log.INFO, String.format("Load package=%s uid=%d", lpparam.packageName, Process.myUid()));

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (lpparam.packageName.equals(self)) {
			hookAll(XUtilHook.getInstances(), lpparam.classLoader, mSecret);
			return;
		}

		// Build SERIAL
		if (PrivacyManager.getRestriction(null, Process.myUid(), PrivacyManager.cIdentification, "SERIAL", mSecret))
			XposedHelpers.setStaticObjectField(Build.class, "SERIAL",
					PrivacyManager.getDefacedProp(Process.myUid(), "SERIAL"));

		// Advertising Id
		try {
			Class.forName("com.google.android.gms.ads.identifier.AdvertisingIdClient$Info", false, lpparam.classLoader);
			hookAll(XAdvertisingIdClientInfo.getInstances(), lpparam.classLoader, mSecret);
		} catch (Throwable ignored) {
		}

		// Google auth
		try {
			Class.forName("com.google.android.gms.auth.GoogleAuthUtil", false, lpparam.classLoader);
			hookAll(XGoogleAuthUtil.getInstances(), lpparam.classLoader, mSecret);
		} catch (Throwable ignored) {
		}

		// Location client
		try {
			Class.forName("com.google.android.gms.location.LocationClient", false, lpparam.classLoader);
			hookAll(XLocationClient.getInstances(), lpparam.classLoader, mSecret);
		} catch (Throwable ignored) {
		}
	}

	public static void handleGetSystemService(XHook hook, String name, Object instance) {
		Util.log(hook, Log.INFO,
				"getSystemService " + name + "=" + instance.getClass().getName() + " uid=" + Binder.getCallingUid());

		if (name.equals(Context.ACCOUNT_SERVICE)) {
			// Account manager
			if (!mAccountManagerHooked) {
				hookAll(XAccountManager.getInstances(instance), mSecret);
				mAccountManagerHooked = true;
			}
		} else if (name.equals(Context.ACTIVITY_SERVICE)) {
			// Activity manager
			if (!mActivityManagerHooked) {
				hookAll(XActivityManager.getInstances(instance), mSecret);
				mActivityManagerHooked = true;
			}
		} else if (name.equals(Context.CLIPBOARD_SERVICE)) {
			// Clipboard manager
			if (!mClipboardManagerHooked) {
				XPrivacy.hookAll(XClipboardManager.getInstances(instance), mSecret);
				mClipboardManagerHooked = true;
			}
		} else if (name.equals(Context.CONNECTIVITY_SERVICE)) {
			// Connectivity manager
			if (!mConnectivityManagerHooked) {
				hookAll(XConnectivityManager.getInstances(instance), mSecret);
				mConnectivityManagerHooked = true;
			}
		} else if (name.equals(Context.LOCATION_SERVICE)) {
			// Location manager
			if (!mLocationManagerHooked) {
				hookAll(XLocationManager.getInstances(instance), mSecret);
				mLocationManagerHooked = true;
			}
		} else if (name.equals("PackageManager")) {
			// Package manager
			if (!mPackageManagerHooked) {
				hookAll(XPackageManager.getInstances(instance), mSecret);
				mPackageManagerHooked = true;
			}
		} else if (name.equals(Context.SENSOR_SERVICE)) {
			// Sensor manager
			if (!mSensorManagerHooked) {
				hookAll(XSensorManager.getInstances(instance), mSecret);
				mSensorManagerHooked = true;
			}
		} else if (name.equals(Context.TELEPHONY_SERVICE)) {
			// Telephony manager
			if (!mTelephonyManagerHooked) {
				hookAll(XTelephonyManager.getInstances(instance), mSecret);
				mTelephonyManagerHooked = true;
			}
		} else if (name.equals(Context.WINDOW_SERVICE)) {
			// Window manager
			if (!mWindowManagerHooked) {
				XPrivacy.hookAll(XWindowManager.getInstances(instance), mSecret);
				mWindowManagerHooked = true;
			}
		} else if (name.equals(Context.WIFI_SERVICE)) {
			// WiFi manager
			if (!mWiFiManagerHooked) {
				XPrivacy.hookAll(XWifiManager.getInstances(instance), mSecret);
				mWiFiManagerHooked = true;
			}
		}
	}

	public static void hookAll(List<XHook> listHook, String secret) {
		for (XHook hook : listHook)
			hook(hook, secret);
	}

	public static void hookAll(List<XHook> listHook, ClassLoader classLoader, String secret) {
		for (XHook hook : listHook)
			hook(hook, classLoader, secret);
	}

	private static void hook(XHook hook, String secret) {
		hook(hook, null, secret);
	}

	private static void hook(final XHook hook, ClassLoader classLoader, String secret) {
		// Check SDK version
		Hook md = null;
		if (hook.getRestrictionName() == null) {
			if (hook.getSdk() == 0)
				Util.log(null, Log.ERROR, "No SDK specified for " + hook.getClassName() + "." + hook.getMethodName());
		} else {
			md = PrivacyManager.getHook(hook.getRestrictionName(), hook.getSpecifier());
			if (md == null)
				Util.log(null, Log.ERROR, "Hook not found " + hook.getRestrictionName() + "/" + hook.getSpecifier());
			else if (hook.getSdk() != 0)
				Util.log(null, Log.ERROR, "SDK specified for " + hook.getRestrictionName() + "/" + hook.getSpecifier());
		}

		int sdk = 0;
		if (hook.getRestrictionName() == null)
			sdk = hook.getSdk();
		else if (md != null)
			sdk = md.getSdk();

		if (Build.VERSION.SDK_INT < sdk)
			return;

		// Provide secret
		hook.setSecret(secret);

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
			Class<?> hookClass;
			try {
				hookClass = findClass(hook.getClassName(), classLoader);
				if (hookClass == null)
					throw new ClassNotFoundException(hook.getClassName());
			} catch (Throwable ex) {
				String packageName = AndroidAppHelper.currentPackageName();
				String restrictionName = hook.getRestrictionName();
				String message = String.format("%s: class not found: %s for %s/%s uid=%d", packageName,
						hook.getClassName(), restrictionName, hook.getSpecifier(), Process.myUid());
				mListHookError.add(message);
				Util.log(hook, hook.isOptional() ? Log.WARN : Log.ERROR, message);
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
			if (hookSet.isEmpty() && !hook.getClassName().startsWith("com.google.android.gms")) {
				String packageName = AndroidAppHelper.currentPackageName();
				String restrictionName = hook.getRestrictionName();
				String message = String.format("%s: method not found: %s.%s for %s/%s uid=%d", packageName,
						hookClass.getName(), hook.getMethodName(), restrictionName, hook.getSpecifier(),
						Process.myUid());
				mListHookError.add(message);
				Util.log(hook, hook.isOptional() ? Log.WARN : Log.ERROR, message);
				return;
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			mListHookError.add(ex.toString());
		}
	}
}

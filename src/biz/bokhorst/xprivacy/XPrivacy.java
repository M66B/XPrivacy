package biz.bokhorst.xprivacy;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import android.content.Context;
import android.os.Build;
import android.os.IBinder;
import android.os.Process;
import android.os.copy.IServiceManager;
import android.os.copy.ServiceManagerProxy;
import android.util.Log;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.callbacks.XC_LoadPackage.LoadPackageParam;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XC_MethodHook;
import static de.robv.android.xposed.XposedHelpers.findClass;

public class XPrivacy implements IXposedHookLoadPackage, IXposedHookZygoteInit {
	private static String mSecret = null;
	private static PrivacyService mPrivacyService = null;
	private static List<String> mListHookError = new ArrayList<String>();
	private static List<CRestriction> mListDisabled = new ArrayList<CRestriction>();

	// http://developer.android.com/reference/android/Manifest.permission.html

	static {
		if (mListDisabled.size() == 0) {
			File disabled = new File("/data/system/xprivacy/disabled");
			if (disabled.exists() && disabled.canRead())
				try {
					Log.w("XPrivacy", "Reading " + disabled.getAbsolutePath());
					FileInputStream fis = new FileInputStream(disabled);
					InputStreamReader ir = new InputStreamReader(fis);
					BufferedReader br = new BufferedReader(ir);
					String line;
					while ((line = br.readLine()) != null) {
						String[] name = line.split("/");
						if (name.length > 0) {
							String methodName = (name.length > 1 ? name[1] : null);
							CRestriction restriction = new CRestriction(0, name[0], methodName, null);
							Log.w("XPrivacy", "Disabling " + restriction);
							mListDisabled.add(restriction);
						}
					}
					br.close();
					ir.close();
					fis.close();
				} catch (Throwable ex) {
					Log.w("XPrivacy", ex.toString());
				}
		}
	}

	// Xposed
	public void initZygote(StartupParam startupParam) throws Throwable {
		// Check for LBE security master
		if (Util.hasLBE()) {
			Util.log(null, Log.ERROR, "LBE installed");
			return;
		}

		init(startupParam.modulePath);
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		// Check for LBE security master
		if (Util.hasLBE())
			return;

		handleLoadPackage(lpparam.packageName, lpparam.classLoader, lpparam.isFirstApplication, mSecret);
	}

	// Common
	private static void init(String path) {
		Util.log(null, Log.WARN, "Init path=" + path);

		// Generate secret
		mSecret = Long.toHexString(new Random().nextLong());

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
			try {
				Class<?> libcore = Class.forName("libcore.io.Libcore");
				Field fOs = libcore.getDeclaredField("os");
				fOs.setAccessible(true);
				Object os = fOs.get(null);
				Method setenv = os.getClass().getMethod("setenv", String.class, String.class, boolean.class);
				setenv.setAccessible(true);
				boolean aosp = new File("/data/system/xprivacy/aosp").exists();
				setenv.invoke(os, "XPrivacy.AOSP", Boolean.toString(aosp), false);
				Util.log(null, Log.WARN, "AOSP mode forced=" + aosp);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {
			// System server
			try {
				// frameworks/base/services/java/com/android/server/SystemServer.java
				Class<?> cSystemServer = Class.forName("com.android.server.SystemServer");
				Method mMain = cSystemServer.getDeclaredMethod("main", String[].class);
				XposedBridge.hookMethod(mMain, new XC_MethodHook() {
					@Override
					protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
						PrivacyService.register(mListHookError, null, mSecret, null);
					}
				});
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

			hookAll(null);
		} else {
			// Register privacy service
			try {
				// Prepare for context switch
				Class<?> cSELinux = Class.forName("android.os.SELinux");
				Method mGetFileContext = cSELinux.getDeclaredMethod("getFileContext", String.class);
				Method mSetFileContext = cSELinux.getDeclaredMethod("setFileContext", String.class, String.class);

				// Save currenc context
				String prevContext = (String) mGetFileContext.invoke(null, path);

				// Switch to system server context
				mSetFileContext.invoke(null, path, "u:r:system_server:s0");

				// Register the privacy service
				Class<?> cBinderInternal = Class.forName("com.android.internal.os.BinderInternal");
				Method mGetContextObject = cBinderInternal.getDeclaredMethod("getContextObject");
				IBinder contextObject = (IBinder) mGetContextObject.invoke(null);
				IServiceManager serviceManager = new ServiceManagerProxy(contextObject);
				mPrivacyService = PrivacyService.register(serviceManager, mSecret);

				// Restore original context
				mSetFileContext.invoke(null, path, prevContext);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}
	}

	private static void handleLoadPackage(String packageName, final ClassLoader classLoader, boolean main, String secret) {
		// Util.log(null, Log.INFO, "Load package=" + packageName + " uid=" +
		// Process.myUid());

		if (main && Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP && "android".equals(packageName))
			try {
				Class<?> cSystemServer = Class.forName("com.android.server.am.ActivityManagerService", false,
						classLoader);
				Method mMain = cSystemServer.getDeclaredMethod("setSystemProcess");
				XposedBridge.hookMethod(mMain, new XC_MethodHook() {
					@Override
					protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
						if (mPrivacyService != null) {
							mPrivacyService.start(mListHookError, classLoader, param.thisObject);
							hookAll(classLoader);
						}
					}
				});
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (packageName.equals(self)) {
			hookAll(XUtilHook.getInstances(), classLoader, secret);
			return;
		}

		// Build SERIAL
		if (Process.myUid() != Process.SYSTEM_UID
				&& PrivacyManager.getRestrictionExtra(null, Process.myUid(), PrivacyManager.cIdentification, "SERIAL",
						null, Build.SERIAL, secret))
			try {
				Field serial = Build.class.getField("SERIAL");
				serial.setAccessible(true);
				serial.set(null, PrivacyManager.getDefacedProp(Process.myUid(), "SERIAL"));
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		// Activity recognition
		try {
			Class.forName("com.google.android.gms.location.ActivityRecognitionClient", false, classLoader);
			hookAll(XActivityRecognitionClient.getInstances(), classLoader, secret);
		} catch (Throwable ignored) {
		}

		// Advertising Id
		try {
			Class.forName("com.google.android.gms.ads.identifier.AdvertisingIdClient$Info", false, classLoader);
			hookAll(XAdvertisingIdClientInfo.getInstances(), classLoader, secret);
		} catch (Throwable ignored) {
		}

		// Cast device
		try {
			Class.forName("com.google.android.gms.cast.CastDevice", false, classLoader);
			hookAll(XCastDevice.getInstances(), classLoader, secret);
		} catch (Throwable ignored) {
		}

		// Google auth
		try {
			Class.forName("com.google.android.gms.auth.GoogleAuthUtil", false, classLoader);
			hookAll(XGoogleAuthUtil.getInstances(), classLoader, secret);
		} catch (Throwable ignored) {
		}

		// GoogleApiClient.Builder
		try {
			Class.forName("com.google.android.gms.common.api.GoogleApiClient$Builder", false, classLoader);
			hookAll(XGoogleApiClient.getInstances(), classLoader, secret);
		} catch (Throwable ignored) {
		}

		// Google Map V1
		try {
			Class.forName("com.google.android.maps.GeoPoint", false, classLoader);
			hookAll(XGoogleMapV1.getInstances(), classLoader, secret);
		} catch (Throwable ignored) {
		}

		// Google Map V2
		try {
			Class.forName("com.google.android.gms.maps.GoogleMap", false, classLoader);
			hookAll(XGoogleMapV2.getInstances(), classLoader, secret);
		} catch (Throwable ignored) {
		}

		// Location client
		try {
			Class.forName("com.google.android.gms.location.LocationClient", false, classLoader);
			hookAll(XLocationClient.getInstances(), classLoader, secret);
		} catch (Throwable ignored) {
		}

		// Phone interface manager
		if ("com.android.phone".equals(packageName))
			hookAll(XTelephonyManager.getPhoneInstances(), classLoader, secret);

		// Providers
		hookAll(XContentResolver.getPackageInstances(packageName, classLoader), classLoader, secret);
	}

	private static void hookAll(final ClassLoader classLoader) {
		// Account manager
		hookAll(XAccountManager.getInstances(null, true), classLoader, mSecret);

		// Activity manager
		hookAll(XActivityManager.getInstances(null, true), classLoader, mSecret);

		// Activity manager service
		hookAll(XActivityManagerService.getInstances(), classLoader, mSecret);

		// App widget manager
		hookAll(XAppWidgetManager.getInstances(), classLoader, mSecret);

		// Application
		hookAll(XApplication.getInstances(), classLoader, mSecret);

		// Audio record
		hookAll(XAudioRecord.getInstances(), classLoader, mSecret);

		// Binder device
		hookAll(XBinder.getInstances(), classLoader, mSecret);

		// Bluetooth adapater
		hookAll(XBluetoothAdapter.getInstances(), classLoader, mSecret);

		// Bluetooth device
		hookAll(XBluetoothDevice.getInstances(), classLoader, mSecret);

		// Camera
		hookAll(XCamera.getInstances(), classLoader, mSecret);

		// Camera2 device
		hookAll(XCameraDevice2.getInstances(), classLoader, mSecret);

		// Clipboard manager
		hookAll(XClipboardManager.getInstances(null, true), classLoader, mSecret);

		// Connectivity manager
		hookAll(XConnectivityManager.getInstances(null, true), classLoader, mSecret);

		// Content resolver
		hookAll(XContentResolver.getInstances(null), classLoader, mSecret);

		// Context wrapper
		hookAll(XContextImpl.getInstances(), classLoader, mSecret);

		// Environment
		hookAll(XEnvironment.getInstances(), classLoader, mSecret);

		// Inet address
		hookAll(XInetAddress.getInstances(), classLoader, mSecret);

		// Input device
		hookAll(XInputDevice.getInstances(), classLoader, mSecret);

		// Intent firewall
		hookAll(XIntentFirewall.getInstances(), classLoader, mSecret);

		// IO bridge
		hookAll(XIoBridge.getInstances(), classLoader, mSecret);

		// IP prefix
		hookAll(XIpPrefix.getInstances(), classLoader, mSecret);

		// Link properties
		hookAll(XLinkProperties.getInstances(), classLoader, mSecret);

		// Location manager
		hookAll(XLocationManager.getInstances(null, true), classLoader, mSecret);

		// Media recorder
		hookAll(XMediaRecorder.getInstances(), classLoader, mSecret);

		// Network info
		hookAll(XNetworkInfo.getInstances(), classLoader, mSecret);

		// Network interface
		hookAll(XNetworkInterface.getInstances(), classLoader, mSecret);

		// NFC adapter
		hookAll(XNfcAdapter.getInstances(), classLoader, mSecret);

		// Package manager service
		hookAll(XPackageManager.getInstances(null, true), classLoader, mSecret);

		// Process
		hookAll(XProcess.getInstances(), classLoader, mSecret);

		// Process builder
		hookAll(XProcessBuilder.getInstances(), classLoader, mSecret);

		// Resources
		hookAll(XResources.getInstances(), classLoader, mSecret);

		// Runtime
		hookAll(XRuntime.getInstances(), classLoader, mSecret);

		// Sensor manager
		hookAll(XSensorManager.getInstances(null, true), classLoader, mSecret);

		// Settings secure
		hookAll(XSettingsSecure.getInstances(), classLoader, mSecret);

		// SIP manager
		hookAll(XSipManager.getInstances(), classLoader, mSecret);

		// SMS manager
		hookAll(XSmsManager.getInstances(), classLoader, mSecret);

		// System properties
		hookAll(XSystemProperties.getInstances(), classLoader, mSecret);

		// Telephone service
		hookAll(XTelephonyManager.getInstances(null, true), classLoader, mSecret);

		// Usage statistics manager
		hookAll(XUsageStatsManager.getInstances(), classLoader, mSecret);

		// USB device
		hookAll(XUsbDevice.getInstances(), classLoader, mSecret);

		// Web view
		hookAll(XWebView.getInstances(), classLoader, mSecret);

		// Window service
		hookAll(XWindowManager.getInstances(null, true), classLoader, mSecret);

		// Wi-Fi service
		hookAll(XWifiManager.getInstances(null, true), classLoader, mSecret);

		// Intent receive
		hookAll(XActivityThread.getInstances(), classLoader, mSecret);

		// Intent send
		hookAll(XActivity.getInstances(), classLoader, mSecret);
	}

	public static void handleGetSystemService(String name, String className, String secret) {
		if (PrivacyManager.getTransient(className, null) == null) {
			PrivacyManager.setTransient(className, Boolean.toString(true));

			if (name.equals(Context.ACCOUNT_SERVICE))
				hookAll(XAccountManager.getInstances(className, false), null, secret);
			else if (name.equals(Context.ACTIVITY_SERVICE))
				hookAll(XActivityManager.getInstances(className, false), null, secret);
			else if (name.equals(Context.CLIPBOARD_SERVICE))
				hookAll(XClipboardManager.getInstances(className, false), null, secret);
			else if (name.equals(Context.CONNECTIVITY_SERVICE))
				hookAll(XConnectivityManager.getInstances(className, false), null, secret);
			else if (name.equals(Context.LOCATION_SERVICE))
				hookAll(XLocationManager.getInstances(className, false), null, secret);
			else if (name.equals("PackageManager"))
				hookAll(XPackageManager.getInstances(className, false), null, secret);
			else if (name.equals(Context.SENSOR_SERVICE))
				hookAll(XSensorManager.getInstances(className, false), null, secret);
			else if (name.equals(Context.TELEPHONY_SERVICE))
				hookAll(XTelephonyManager.getInstances(className, false), null, secret);
			else if (name.equals(Context.WINDOW_SERVICE))
				hookAll(XWindowManager.getInstances(className, false), null, secret);
			else if (name.equals(Context.WIFI_SERVICE))
				hookAll(XWifiManager.getInstances(className, false), null, secret);
		}
	}

	public static void hookAll(List<XHook> listHook, ClassLoader classLoader, String secret) {
		for (XHook hook : listHook)
			if (hook.getRestrictionName() == null)
				hook(hook, classLoader, secret);
			else {
				CRestriction crestriction = new CRestriction(0, hook.getRestrictionName(), null, null);
				CRestriction mrestriction = new CRestriction(0, hook.getRestrictionName(), hook.getMethodName(), null);
				if (mListDisabled.contains(crestriction) || mListDisabled.contains(mrestriction))
					Util.log(hook, Log.WARN, "Skipping " + hook);
				else
					hook(hook, classLoader, secret);
			}
	}

	private static void hook(final XHook hook, ClassLoader classLoader, String secret) {
		// Get meta data
		Hook md = PrivacyManager.getHook(hook.getRestrictionName(), hook.getSpecifier());
		if (md == null) {
			String message = "Not found hook=" + hook;
			mListHookError.add(message);
			Util.log(hook, Log.ERROR, message);
		} else if (!md.isAvailable())
			return;

		// Provide secret
		if (secret == null)
			Util.log(hook, Log.ERROR, "Secret missing hook=" + hook);
		hook.setSecret(secret);

		try {
			// Find class
			Class<?> hookClass = null;
			try {
				hookClass = findClass(hook.getClassName(), classLoader);
			} catch (Throwable ex) {
				String message = "Class not found hook=" + hook;
				int level = (md != null && md.isOptional() ? Log.WARN : Log.ERROR);
				if ("isXposedEnabled".equals(hook.getMethodName()))
					level = Log.WARN;
				if (level == Log.ERROR)
					mListHookError.add(message);
				Util.log(hook, level, message);
				Util.logStack(hook, level);
				return;
			}

			// Get members
			List<Member> listMember = new ArrayList<Member>();
			// TODO: enable/disable superclass traversal
			Class<?> clazz = hookClass;
			while (clazz != null && !"android.content.ContentProvider".equals(clazz.getName()))
				try {
					if (hook.getMethodName() == null) {
						for (Constructor<?> constructor : clazz.getDeclaredConstructors())
							if (!Modifier.isAbstract(constructor.getModifiers())
									&& Modifier.isPublic(constructor.getModifiers()) ? hook.isVisible() : !hook
									.isVisible())
								listMember.add(constructor);
						break;
					} else {
						for (Method method : clazz.getDeclaredMethods())
							if (method.getName().equals(hook.getMethodName())
									&& !Modifier.isAbstract(method.getModifiers())
									&& (Modifier.isPublic(method.getModifiers()) ? hook.isVisible() : !hook.isVisible()))
								listMember.add(method);
					}
					clazz = clazz.getSuperclass();
				} catch (Throwable ex) {
					if (ex.getClass().equals(ClassNotFoundException.class))
						break;
					else
						throw ex;
				}

			// Hook members
			for (Member member : listMember)
				try {
					XposedBridge.hookMethod(member, new XMethodHook(hook));
				} catch (NoSuchFieldError ex) {
					Util.log(hook, Log.WARN, ex.toString());
				} catch (Throwable ex) {
					mListHookError.add(ex.toString());
					Util.bug(hook, ex);
				}

			// Check if members found
			if (listMember.isEmpty() && !hook.getClassName().startsWith("com.google.android.gms")) {
				String message = "Method not found hook=" + hook;
				int level = (md != null && md.isOptional() ? Log.WARN : Log.ERROR);
				if ("isXposedEnabled".equals(hook.getMethodName()))
					level = Log.WARN;
				if (level == Log.ERROR)
					mListHookError.add(message);
				Util.log(hook, level, message);
				Util.logStack(hook, level);
			}
		} catch (Throwable ex) {
			mListHookError.add(ex.toString());
			Util.bug(hook, ex);
		}
	}

	// Helper classes

	private static class XMethodHook extends XC_MethodHook {
		private XHook mHook;

		public XMethodHook(XHook hook) {
			mHook = hook;
		}

		@Override
		protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
			try {
				// Do not restrict Zygote
				if (Process.myUid() <= 0)
					return;

				// Pre processing
				XParam xparam = XParam.fromXposed(param);

				long start = System.currentTimeMillis();

				// Execute hook
				mHook.before(xparam);

				long ms = System.currentTimeMillis() - start;
				if (ms > PrivacyManager.cWarnHookDelayMs)
					Util.log(mHook, Log.WARN, String.format("%s %d ms", param.method.getName(), ms));

				// Post processing
				if (xparam.hasResult())
					param.setResult(xparam.getResult());
				if (xparam.hasThrowable())
					param.setThrowable(xparam.getThrowable());
				param.setObjectExtra("xextra", xparam.getExtras());
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		@Override
		protected void afterHookedMethod(MethodHookParam param) throws Throwable {
			if (!param.hasThrowable())
				try {
					// Do not restrict Zygote
					if (Process.myUid() <= 0)
						return;

					// Pre processing
					XParam xparam = XParam.fromXposed(param);
					xparam.setExtras(param.getObjectExtra("xextra"));

					long start = System.currentTimeMillis();

					// Execute hook
					mHook.after(xparam);

					long ms = System.currentTimeMillis() - start;
					if (ms > PrivacyManager.cWarnHookDelayMs)
						Util.log(mHook, Log.WARN, String.format("%s %d ms", param.method.getName(), ms));

					// Post processing
					if (xparam.hasResult())
						param.setResult(xparam.getResult());
					if (xparam.hasThrowable())
						param.setThrowable(xparam.getThrowable());
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}
		}
	};
}

package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import com.saurik.substrate.MS;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Binder;
import android.os.Build;
import android.os.Process;
import android.util.Log;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.callbacks.XC_LoadPackage.LoadPackageParam;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers;
import de.robv.android.xposed.XC_MethodHook;
import static de.robv.android.xposed.XposedHelpers.findClass;

@SuppressLint("DefaultLocale")
public class XPrivacy implements IXposedHookLoadPackage, IXposedHookZygoteInit {
	private static boolean cydia = false;
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

	// Cydia substrate
	public static void initialize() {
		cydia = true;

		hook();

		// Self
		MS.hookClassLoad(Util.class.getName(), new MS.ClassLoadHook() {
			@Override
			public void classLoaded(Class<?> clazz) {
				hookAll(XUtilHook.getInstances(), clazz.getClassLoader(), mSecret);
			}
		});

		// TODO: Cydia: Build.SERIAL

		// Advertising Id
		MS.hookClassLoad("com.google.android.gms.ads.identifier.AdvertisingIdClient$Info", new MS.ClassLoadHook() {
			@Override
			public void classLoaded(Class<?> clazz) {
				hookAll(XAdvertisingIdClientInfo.getInstances(), clazz.getClassLoader(), mSecret);
			}
		});

		// User activity
		MS.hookClassLoad("com.google.android.gms.location.ActivityRecognitionClient", new MS.ClassLoadHook() {
			@Override
			public void classLoaded(Class<?> clazz) {
				hookAll(XActivityRecognitionClient.getInstances(), clazz.getClassLoader(), mSecret);
			}
		});

		// Google auth
		MS.hookClassLoad("com.google.android.gms.auth.GoogleAuthUtil", new MS.ClassLoadHook() {
			@Override
			public void classLoaded(Class<?> clazz) {
				hookAll(XGoogleAuthUtil.getInstances(), clazz.getClassLoader(), mSecret);
			}
		});

		// Location client
		MS.hookClassLoad("com.google.android.gms.location.LocationClient", new MS.ClassLoadHook() {
			@Override
			public void classLoaded(Class<?> clazz) {
				hookAll(XLocationClient.getInstances(), clazz.getClassLoader(), mSecret);
			}
		});
	}

	@SuppressLint("InlinedApi")
	public void initZygote(StartupParam startupParam) throws Throwable {
		Util.log(null, Log.WARN, String.format("Load %s", startupParam.modulePath));

		// Check for LBE security master
		if (Util.hasLBE()) {
			Util.log(null, Log.ERROR, "LBE installed");
			return;
		}

		hook();
	}

	private static void hook() {
		// Generate secret
		mSecret = Long.toHexString(new Random().nextLong());

		// System server
		try {
			// frameworks/base/services/java/com/android/server/SystemServer.java
			Class<?> cSystemServer = Class.forName("com.android.server.SystemServer");
			Method mMain = cSystemServer.getDeclaredMethod("main", String[].class);
			if (cydia)
				MS.hookMethod(cSystemServer, mMain, new MS.MethodAlteration<Object, Void>() {
					@Override
					public Void invoked(Object thiz, Object... args) throws Throwable {
						PrivacyService.register(mListHookError, mSecret);
						return invoke(thiz, args);
					}
				});
			else
				XposedBridge.hookMethod(mMain, new XC_MethodHook() {
					@Override
					protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
						PrivacyService.register(mListHookError, mSecret);
					}
				});
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		// Activity manager service
		hookAll(XActivityManagerService.getInstances(), mSecret);

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

		// User activity
		try {
			Class.forName("com.google.android.gms.location.ActivityRecognitionClient", false, lpparam.classLoader);
			hookAll(XActivityRecognitionClient.getInstances(), lpparam.classLoader, mSecret);
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

		if ("android.telephony.MSimTelephonyManager".equals(instance.getClass().getName())) {
			Util.log(hook, Log.WARN, "Telephone service=" + Context.TELEPHONY_SERVICE);
			Class<?> clazz = instance.getClass();
			while (clazz != null) {
				Util.log(hook, Log.WARN, "Class " + clazz);
				for (Method method : clazz.getDeclaredMethods())
					Util.log(hook, Log.WARN, "Declared " + method);
				clazz = clazz.getSuperclass();
			}
		}

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
		String message = null;
		if (hook.getRestrictionName() == null) {
			if (hook.getSdk() == 0)
				message = "No SDK specified for " + hook;
		} else {
			md = PrivacyManager.getHook(hook.getRestrictionName(), hook.getSpecifier());
			if (md == null)
				message = "Hook not found " + hook;
			else if (hook.getSdk() != 0)
				message = "SDK not expected for " + hook;
		}
		if (message != null) {
			mListHookError.add(message);
			Util.log(hook, Log.ERROR, message);
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
						XParam xparam = XParam.fromXposed(param);
						hook.before(xparam);
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
							if (Process.myUid() <= 0)
								return;
							XParam xparam = XParam.fromXposed(param);
							xparam.setExtras(param.getObjectExtra("xextra"));
							hook.after(xparam);
							if (xparam.hasResult())
								param.setResult(xparam.getResult());
							if (xparam.hasThrowable())
								param.setThrowable(xparam.getThrowable());
						} catch (Throwable ex) {
							Util.bug(null, ex);
						}
				}
			};

			// Find class
			Class<?> hookClass = null;
			try {
				if (cydia)
					if (classLoader == null)
						hookClass = Class.forName(hook.getClassName());
					else
						hookClass = Class.forName(hook.getClassName(), true, classLoader);
				else
					hookClass = findClass(hook.getClassName(), classLoader);
			} catch (Throwable ex) {
				message = String.format("Class not found for %s", hook);
				mListHookError.add(message);
				Util.log(hook, hook.isOptional() ? Log.WARN : Log.ERROR, message);
			}

			// Get members
			List<Member> listMember = new ArrayList<Member>();
			Class<?> clazz = hookClass;
			while (clazz != null) {
				if (hook.getMethodName() == null) {
					for (Constructor<?> constructor : clazz.getDeclaredConstructors())
						if (Modifier.isPublic(constructor.getModifiers()) ? hook.isVisible() : !hook.isVisible())
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
			}

			// Hook members
			for (final Member member : listMember)
				try {
					if (Modifier.isAbstract(member.getModifiers()))
						Util.log(hook, Log.ERROR, String.format("Abstract: %s", member));
					else {
						if (cydia)
							if (member instanceof Method)
								MS.hookMethod(member.getDeclaringClass(), (Method) member,
										new MS.MethodAlteration<Object, Object>() {
											@Override
											public Object invoked(Object thiz, Object... args) throws Throwable {
												if (Process.myUid() <= 0)
													return invoke(thiz, args);

												XParam xparam = XParam.fromCydia(member, thiz, args);
												hook.before(xparam);

												if (!xparam.hasResult() || xparam.hasThrowable()) {
													try {
														Object result = invoke(thiz, args);
														xparam.setResult(result);
													} catch (Throwable ex) {
														xparam.setThrowable(ex);
													}

													hook.after(xparam);
												}

												if (xparam.hasThrowable())
													throw xparam.getThrowable();
												return xparam.getResult();
											}
										});
							else
								MS.hookMethod(member.getDeclaringClass(), (Constructor<?>) member,
										new MS.MethodAlteration<Object, Object>() {
											@Override
											public Object invoked(Object thiz, Object... args) throws Throwable {
												if (Process.myUid() <= 0)
													return invoke(thiz, args);

												XParam xparam = XParam.fromCydia(member, thiz, args);
												hook.before(xparam);

												if (!xparam.hasResult() || xparam.hasThrowable()) {
													try {
														Object result = invoke(thiz, args);
														xparam.setResult(result);
													} catch (Throwable ex) {
														xparam.setThrowable(ex);
													}

													hook.after(xparam);
												}

												if (xparam.hasThrowable())
													throw xparam.getThrowable();
												return xparam.getResult();
											}
										});
						else
							XposedBridge.hookMethod(member, methodHook);
					}
				} catch (NoSuchFieldError ex) {
					Util.log(hook, Log.WARN, ex.toString());
				} catch (Throwable ex) {
					mListHookError.add(ex.toString());
					Util.bug(hook, ex);
				}

			// Check if members found
			if (listMember.isEmpty() && !hook.getClassName().startsWith("com.google.android.gms")) {
				message = String.format("Method not found for %s", hook);
				if (!hook.isOptional())
					mListHookError.add(message);
				Util.log(hook, hook.isOptional() ? Log.WARN : Log.ERROR, message);
			}
		} catch (Throwable ex) {
			mListHookError.add(ex.toString());
			Util.bug(hook, ex);
		}
	}

	// WORKAROUND: when a native lib is loaded after hooking, the hook is undone

	private static List<XC_MethodHook.Unhook> mUnhookNativeMethod = new ArrayList<XC_MethodHook.Unhook>();

	@SuppressWarnings("unused")
	private static void registerNativeMethod(final XHook hook, Method method, XC_MethodHook.Unhook unhook) {
		if (Process.myUid() > 0) {
			synchronized (mUnhookNativeMethod) {
				mUnhookNativeMethod.add(unhook);
				Util.log(hook, Log.INFO, "Native " + method + " uid=" + Process.myUid());
			}
		}
	}

	@SuppressWarnings("unused")
	private static void hookCheckNative() {
		try {
			XC_MethodHook hook = new XC_MethodHook() {
				@Override
				protected void afterHookedMethod(MethodHookParam param) throws Throwable {
					if (Process.myUid() > 0)
						try {
							synchronized (mUnhookNativeMethod) {
								Util.log(null, Log.INFO, "Loading " + param.args[0] + " uid=" + Process.myUid()
										+ " count=" + mUnhookNativeMethod.size());
								for (XC_MethodHook.Unhook unhook : mUnhookNativeMethod) {
									XposedBridge.hookMethod(unhook.getHookedMethod(), unhook.getCallback());
									unhook.unhook();
								}
							}
						} catch (Throwable ex) {
							Util.bug(null, ex);
						}
				}
			};

			Class<?> runtimeClass = Class.forName("java.lang.Runtime");
			for (Method method : runtimeClass.getDeclaredMethods())
				if (method.getName().equals("load") || method.getName().equals("loadLibrary")) {
					XposedBridge.hookMethod(method, hook);
					Util.log(null, Log.WARN, "Hooked " + method);
				}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}
}

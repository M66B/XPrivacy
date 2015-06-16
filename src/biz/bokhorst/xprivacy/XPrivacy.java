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
import android.os.Process;
import android.util.Log;
import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.callbacks.XC_LoadPackage.LoadPackageParam;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XC_MethodHook;
import static de.robv.android.xposed.XposedHelpers.findClass;

public class XPrivacy implements IXposedHookLoadPackage, IXposedHookZygoteInit {
	private static String mSecret = null;
	private static List<String> mListHookError = new ArrayList<String>();
	private static List<CRestriction> mListDisabled = new ArrayList<CRestriction>();

	public void initZygote(StartupParam startupParam) throws Throwable {
		Util.log(null, Log.WARN, "Init path=" + startupParam.modulePath);

		// Check for LBE security master
		if (Util.hasLBE()) {
			Util.log(null, Log.ERROR, "LBE installed");
			return;
		}

		// Generate secret
		mSecret = Long.toHexString(new Random().nextLong());

		// Reading files with SELinux enabled can result in bootloops
		boolean selinux = Util.isSELinuxEnforced();
		if ("true".equals(Util.getXOption("ignoreselinux"))) {
			selinux = false;
			Log.w("Xprivacy", "Ignoring SELinux");
		}

		// Read list of disabled hooks
		if (mListDisabled.size() == 0 && !selinux) {
			File disabled = new File("/data/system/xprivacy/disabled");
			if (disabled.exists() && disabled.canRead())
				try {
					Log.w("XPrivacy", "Reading " + disabled.getAbsolutePath());
					FileInputStream fis = new FileInputStream(disabled);
					InputStreamReader ir = new InputStreamReader(fis);
					BufferedReader br = new BufferedReader(ir);
					String line;
					while ((line = br.readLine()) != null)
						if (line.length() > 0 && !line.startsWith("#")) {
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

		// AOSP mode override
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT && !selinux)
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

		/*
		 * ActivityManagerService is the beginning of the main "android"
		 * process. This is where the core java system is started, where the
		 * system context is created and so on. In pre-lollipop we can access
		 * this class directly, but in lollipop we have to visit ActivityThread
		 * first, since this class is now responsible for creating a class
		 * loader that can be used to access ActivityManagerService. It is no
		 * longer possible to do so via the normal boot class loader. Doing it
		 * like this will create a consistency between older and newer Android
		 * versions.
		 * 
		 * Note that there is no need to handle arguments in this case. And we
		 * don't need them so in case they change over time, we will simply use
		 * the hookAll feature.
		 */

		try {
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
				Class<?> at = Class.forName("android.app.ActivityThread");
				XposedBridge.hookAllMethods(at, "systemMain", new XC_MethodHook() {
					@Override
					protected void afterHookedMethod(MethodHookParam param) throws Throwable {
						try {
							final ClassLoader loader = Thread.currentThread().getContextClassLoader();
							Class<?> am = Class.forName("com.android.server.am.ActivityManagerService", false, loader);
							XposedBridge.hookAllConstructors(am, new XC_MethodHook() {
								@Override
								protected void afterHookedMethod(MethodHookParam param) throws Throwable {
									try {
										PrivacyService.register(mListHookError, loader, mSecret, param.thisObject);
										hookSystem(loader);
									} catch (Throwable ex) {
										Util.bug(null, ex);
									}
								}
							});
						} catch (Throwable ex) {
							Util.bug(null, ex);
						}
					}
				});

			} else {
				Class<?> cSystemServer = Class.forName("com.android.server.SystemServer");
				Method mMain = cSystemServer.getDeclaredMethod("main", String[].class);
				XposedBridge.hookMethod(mMain, new XC_MethodHook() {
					@Override
					protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
						try {
							PrivacyService.register(mListHookError, null, mSecret, null);
						} catch (Throwable ex) {
							Util.bug(null, ex);
						}
					}
				});
			}

			hookZygote();
			if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP)
				hookSystem(null);

		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		// Check for LBE security master
		if (Util.hasLBE())
			return;

		hookPackage(lpparam.packageName, lpparam.classLoader);
	}

	private void hookZygote() throws Throwable {
		Log.w("XPrivacy", "Hooking Zygote");

		/*
		 * Add nixed User Space / System Server hooks
		 */

		// Account manager
		hookAll(XAccountManager.getInstances(null, false), null, mSecret, false);

		// Activity manager
		hookAll(XActivityManager.getInstances(null, false), null, mSecret, false);

		// App widget manager
		hookAll(XAppWidgetManager.getInstances(false), null, mSecret, false);

		// Bluetooth adapater
		hookAll(XBluetoothAdapter.getInstances(false), null, mSecret, false);

		// Clipboard manager
		hookAll(XClipboardManager.getInstances(null, false), null, mSecret, false);

		// Content resolver
		hookAll(XContentResolver.getInstances(false), null, mSecret, false);

		// Package manager service
		hookAll(XPackageManager.getInstances(null, false), null, mSecret, false);

		// SMS manager
		hookAll(XSmsManager.getInstances(false), null, mSecret, false);

		// Telephone service
		hookAll(XTelephonyManager.getInstances(null, false), null, mSecret, false);

		// Usage statistics manager
		hookAll(XUsageStatsManager.getInstances(false), null, mSecret, false);

		/*
		 * Add pure user space hooks
		 */

		// Intent receive
		hookAll(XActivityThread.getInstances(), null, mSecret, false);

		// Runtime
		hookAll(XRuntime.getInstances(), null, mSecret, false);

		// Application
		hookAll(XApplication.getInstances(), null, mSecret, false);

		// Audio record
		hookAll(XAudioRecord.getInstances(), null, mSecret, false);

		// Binder device
		hookAll(XBinder.getInstances(), null, mSecret, false);

		// Bluetooth device
		hookAll(XBluetoothDevice.getInstances(), null, mSecret, false);

		// Camera
		hookAll(XCamera.getInstances(), null, mSecret, false);

		// Camera2 device
		hookAll(XCameraDevice2.getInstances(), null, mSecret, false);

		// Connectivity manager
		hookAll(XConnectivityManager.getInstances(null, false), null, mSecret, false);

		// Context wrapper
		hookAll(XContextImpl.getInstances(), null, mSecret, false);

		// Environment
		hookAll(XEnvironment.getInstances(), null, mSecret, false);

		// Inet address
		hookAll(XInetAddress.getInstances(), null, mSecret, false);

		// Input device
		hookAll(XInputDevice.getInstances(), null, mSecret, false);

		// IO bridge
		hookAll(XIoBridge.getInstances(), null, mSecret, false);

		// IP prefix
		hookAll(XIpPrefix.getInstances(), null, mSecret, false);

		// Link properties
		hookAll(XLinkProperties.getInstances(), null, mSecret, false);

		// Location manager
		hookAll(XLocationManager.getInstances(null, false), null, mSecret, false);

		// Media recorder
		hookAll(XMediaRecorder.getInstances(), null, mSecret, false);

		// Network info
		hookAll(XNetworkInfo.getInstances(), null, mSecret, false);

		// Network interface
		hookAll(XNetworkInterface.getInstances(), null, mSecret, false);

		// NFC adapter
		hookAll(XNfcAdapter.getInstances(), null, mSecret, false);

		// Process
		hookAll(XProcess.getInstances(), null, mSecret, false);

		// Process builder
		hookAll(XProcessBuilder.getInstances(), null, mSecret, false);

		// Resources
		hookAll(XResources.getInstances(), null, mSecret, false);

		// Sensor manager
		hookAll(XSensorManager.getInstances(null, false), null, mSecret, false);

		// Settings secure
		hookAll(XSettingsSecure.getInstances(), null, mSecret, false);

		// SIP manager
		hookAll(XSipManager.getInstances(), null, mSecret, false);

		// System properties
		hookAll(XSystemProperties.getInstances(), null, mSecret, false);

		// USB device
		hookAll(XUsbDevice.getInstances(), null, mSecret, false);

		// Web view
		hookAll(XWebView.getInstances(), null, mSecret, false);

		// Window service
		hookAll(XWindowManager.getInstances(null, false), null, mSecret, false);

		// Wi-Fi service
		hookAll(XWifiManager.getInstances(null, false), null, mSecret, false);

		// Intent send
		hookAll(XActivity.getInstances(), null, mSecret, false);
	}

	private void hookSystem(ClassLoader classLoader) throws Throwable {
		Log.w("XPrivacy", "Hooking system");

		/*
		 * Add nixed User Space / System Server hooks
		 */

		// Account manager
		hookAll(XAccountManager.getInstances(null, true), classLoader, mSecret, false);

		// Activity manager
		hookAll(XActivityManager.getInstances(null, true), classLoader, mSecret, false);

		// App widget manager
		hookAll(XAppWidgetManager.getInstances(true), classLoader, mSecret, false);

		// Bluetooth adapater
		hookAll(XBluetoothAdapter.getInstances(true), classLoader, mSecret, false);

		// Clipboard manager
		hookAll(XClipboardManager.getInstances(null, true), classLoader, mSecret, false);

		// Content resolver
		hookAll(XContentResolver.getInstances(true), classLoader, mSecret, false);

		// Location manager service
		hookAll(XLocationManager.getInstances(null, true), classLoader, mSecret, false);

		// Package manager service
		hookAll(XPackageManager.getInstances(null, true), classLoader, mSecret, false);

		// SMS manager
		hookAll(XSmsManager.getInstances(true), classLoader, mSecret, false);

		// Telephone service
		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP)
			hookAll(XTelephonyManager.getInstances(null, true), classLoader, mSecret, false);
		hookAll(XTelephonyManager.getRegistryInstances(), classLoader, mSecret, false);

		// Usage statistics manager
		hookAll(XUsageStatsManager.getInstances(true), classLoader, mSecret, false);

		// Wi-Fi service
		hookAll(XWifiManager.getInstances(null, true), classLoader, mSecret, false);

		/*
		 * Add pure system server hooks
		 */

		// Activity manager service
		hookAll(XActivityManagerService.getInstances(), classLoader, mSecret, false);

		// Intent firewall
		hookAll(XIntentFirewall.getInstances(), classLoader, mSecret, false);
	}

	private void hookPackage(String packageName, ClassLoader classLoader) {
		Log.w("XPrivacy", "Hooking package=" + packageName);

		// Skip hooking self
		String self = XPrivacy.class.getPackage().getName();
		if (packageName.equals(self)) {
			hookAll(XUtilHook.getInstances(), classLoader, mSecret, false);
			return;
		}

		// Build SERIAL
		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP || Process.myUid() != Process.SYSTEM_UID)
			if (PrivacyManager.getRestrictionExtra(null, Process.myUid(), PrivacyManager.cIdentification, "SERIAL",
					null, Build.SERIAL, mSecret))
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
			hookAll(XActivityRecognitionClient.getInstances(), classLoader, mSecret, false);
		} catch (Throwable ignored) {
		}

		// Advertising Id
		try {
			Class.forName("com.google.android.gms.ads.identifier.AdvertisingIdClient$Info", false, classLoader);
			hookAll(XAdvertisingIdClientInfo.getInstances(), classLoader, mSecret, false);
		} catch (Throwable ignored) {
		}

		// Cast device
		try {
			Class.forName("com.google.android.gms.cast.CastDevice", false, classLoader);
			hookAll(XCastDevice.getInstances(), classLoader, mSecret, false);
		} catch (Throwable ignored) {
		}

		// Google auth
		try {
			Class.forName("com.google.android.gms.auth.GoogleAuthUtil", false, classLoader);
			hookAll(XGoogleAuthUtil.getInstances(), classLoader, mSecret, false);
		} catch (Throwable ignored) {
		}

		// GoogleApiClient.Builder
		try {
			Class.forName("com.google.android.gms.common.api.GoogleApiClient$Builder", false, classLoader);
			hookAll(XGoogleApiClient.getInstances(), classLoader, mSecret, false);
		} catch (Throwable ignored) {
		}

		// Google Map V1
		try {
			Class.forName("com.google.android.maps.GeoPoint", false, classLoader);
			hookAll(XGoogleMapV1.getInstances(), classLoader, mSecret, false);
		} catch (Throwable ignored) {
		}

		// Google Map V2
		try {
			Class.forName("com.google.android.gms.maps.GoogleMap", false, classLoader);
			hookAll(XGoogleMapV2.getInstances(), classLoader, mSecret, false);
		} catch (Throwable ignored) {
		}

		// Location client
		try {
			Class.forName("com.google.android.gms.location.LocationClient", false, classLoader);
			hookAll(XLocationClient.getInstances(), classLoader, mSecret, false);
		} catch (Throwable ignored) {
		}

		// Phone interface manager
		if ("com.android.phone".equals(packageName)) {
			hookAll(XTelephonyManager.getPhoneInstances(), classLoader, mSecret, false);
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
				hookAll(XTelephonyManager.getInstances(null, true), classLoader, mSecret, false);
		}

		// Providers
		hookAll(XContentResolver.getPackageInstances(packageName, classLoader), classLoader, mSecret, false);
	}

	public static void handleGetSystemService(String name, String className, String secret) {
		if (PrivacyManager.getTransient(className, null) == null) {
			PrivacyManager.setTransient(className, Boolean.toString(true));

			if (name.equals(Context.ACCOUNT_SERVICE))
				hookAll(XAccountManager.getInstances(className, false), null, secret, true);
			else if (name.equals(Context.ACTIVITY_SERVICE))
				hookAll(XActivityManager.getInstances(className, false), null, secret, true);
			else if (name.equals(Context.CLIPBOARD_SERVICE))
				hookAll(XClipboardManager.getInstances(className, false), null, secret, true);
			else if (name.equals(Context.CONNECTIVITY_SERVICE))
				hookAll(XConnectivityManager.getInstances(className, false), null, secret, true);
			else if (name.equals(Context.LOCATION_SERVICE))
				hookAll(XLocationManager.getInstances(className, false), null, secret, true);
			else if (name.equals("PackageManager"))
				hookAll(XPackageManager.getInstances(className, false), null, secret, true);
			else if (name.equals(Context.SENSOR_SERVICE))
				hookAll(XSensorManager.getInstances(className, false), null, secret, true);
			else if (name.equals(Context.TELEPHONY_SERVICE))
				hookAll(XTelephonyManager.getInstances(className, false), null, secret, true);
			else if (name.equals(Context.WINDOW_SERVICE))
				hookAll(XWindowManager.getInstances(className, false), null, secret, true);
			else if (name.equals(Context.WIFI_SERVICE))
				hookAll(XWifiManager.getInstances(className, false), null, secret, true);
		}
	}

	public static void hookAll(List<XHook> listHook, ClassLoader classLoader, String secret, boolean dynamic) {
		for (XHook hook : listHook)
			if (hook.getRestrictionName() == null)
				hook(hook, classLoader, secret);
			else {
				CRestriction crestriction = new CRestriction(0, hook.getRestrictionName(), null, null);
				CRestriction mrestriction = new CRestriction(0, hook.getRestrictionName(), hook.getMethodName(), null);
				if (mListDisabled.contains(crestriction) || mListDisabled.contains(mrestriction))
					Util.log(hook, Log.WARN, "Skipping disabled hook " + hook);
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
				return;
			}

			// Get members
			List<Member> listMember = new ArrayList<Member>();
			List<Class<?>[]> listParameters = new ArrayList<Class<?>[]>();
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
									&& (Modifier.isPublic(method.getModifiers()) ? hook.isVisible() : !hook.isVisible())) {

								// Check for same function in sub class
								boolean different = true;
								for (Class<?>[] parameters : listParameters) {
									boolean same = (parameters.length == method.getParameterTypes().length);
									for (int p = 0; same && p < parameters.length; p++)
										if (!parameters[p].equals(method.getParameterTypes()[p])) {
											same = false;
											break;
										}
									if (same) {
										different = false;
										break;
									}
								}

								if (different) {
									listMember.add(method);
									listParameters.add(method.getParameterTypes());
								}
							}
					}
					clazz = clazz.getSuperclass();
				} catch (Throwable ex) {
					if (ex.getClass().equals(ClassNotFoundException.class)
							|| ex.getClass().equals(NoClassDefFoundError.class))
						break;
					else
						throw ex;
				}

			// Hook members
			for (Member member : listMember)
				try {
					if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
						if ((member.getModifiers() & Modifier.NATIVE) != 0)
							Util.log(hook, Log.WARN, "Native method=" + member);
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

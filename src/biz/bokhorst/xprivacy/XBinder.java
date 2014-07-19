package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.content.Context;
import android.os.Binder;
import android.os.IBinder;
import android.os.Parcel;
import android.os.Process;
import android.util.Log;
import android.util.SparseArray;

public class XBinder extends XHook {
	private Methods mMethod;
	private static long mToken = 0;
	private static Map<String, Boolean> mMapClassSystem = new HashMap<String, Boolean>();
	private static Map<String, SparseArray<String>> mMapCodeName = new HashMap<String, SparseArray<String>>();

	private static final int BITS_TOKEN = 16;
	private static final int FLAG_ALL = 0xFFFF;
	private static final int MASK_TOKEN = 0xFFFF;

	private static final int PING_TRANSACTION = ('_' << 24) | ('P' << 16) | ('N' << 8) | 'G';
	private static final int DUMP_TRANSACTION = ('_' << 24) | ('D' << 16) | ('M' << 8) | 'P';
	private static final int INTERFACE_TRANSACTION = ('_' << 24) | ('N' << 16) | ('T' << 8) | 'F';
	private static final int TWEET_TRANSACTION = ('_' << 24) | ('T' << 16) | ('W' << 8) | 'T';
	private static final int LIKE_TRANSACTION = ('_' << 24) | ('L' << 16) | ('I' << 8) | 'K';
	private static final int SYSPROPS_TRANSACTION = ('_' << 24) | ('S' << 16) | ('P' << 8) | 'R';

	// Service name should one-to-one correspond to the other lists

	// @formatter:off
	public static List<String> cServiceName = Arrays.asList(new String[] {
		"account",
		"activity",
		"clipboard",
		"connectivity",
		"content",
		"location",
		"telephony.registry",
		"telephony.msim.registry",
		"package",
		"iphonesubinfo",
		"iphonesubinfo_msim",
		"window",
		"wifi",
		"sip",
		"isms",
		"nfc",
		"appwidget",
		"bluetooth",
		"bluetooth_manager",
		"input",
		"sensorservice",
		"usb",
		"media.camera"
	});
	// @formatter:on

	// @formatter:off
	public static List<String> cServiceOptional = Arrays.asList(new String[] {
		"iphonesubinfo",
		"iphonesubinfo_msim",
		"sip",
		"isms",
		"nfc",
		"bluetooth",
		"bluetooth_manager"
	});
	// @formatter:on

	// @formatter:off
	public static List<String> cServiceDescriptor = Arrays.asList(new String[] {
		"android.accounts.IAccountManager",
		"android.app.IActivityManager",
		"android.content.IClipboard",
		"android.net.IConnectivityManager",
		"android.content.IContentService",
		"android.location.ILocationManager",
		"com.android.internal.telephony.ITelephonyRegistry",
		"com.android.internal.telephony.ITelephonyRegistryMSim",
		"android.content.pm.IPackageManager",
		"com.android.internal.telephony.IPhoneSubInfo",
		"com.android.internal.telephony.msim.IPhoneSubInfoMSim",
		"android.view.IWindowManager",
		"android.net.wifi.IWifiManager",
		"android.net.sip.ISipService",
		"com.android.internal.telephony.ISms",
		"android.nfc.INfcAdapter",
		"com.android.internal.appwidget.IAppWidgetService",
		"android.bluetooth.IBluetooth",
		"android.bluetooth.IBluetoothManager",
		"android.hardware.input.IInputManager",
		"android.gui.SensorServer",
		"android.hardware.usb.IUsbManager",
		"android.hardware.ICameraService"
	});
	// @formatter:on

	private XBinder(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return (mMethod == Methods.transact ? "android.os.BinderProxy" : "android.os.Binder");
	}

	public boolean isVisible() {
		return (mMethod != Methods.execTransact);
	}

	@Override
	public void setSecret(String secret) {
		super.setSecret(secret);
		mToken = (secret.hashCode() & MASK_TOKEN);
	}

	// @formatter:off

	// private boolean execTransact(int code, int dataObj, int replyObj, int flags)
	// public final boolean transact(int code, Parcel data, Parcel reply, int flags)
	// public native boolean transact(int code, Parcel data, Parcel reply, int flags)
	// frameworks/base/core/java/android/os/Binder.java
	// http://developer.android.com/reference/android/os/Binder.html

	// @formatter:on

	private enum Methods {
		execTransact, transact
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XBinder(Methods.execTransact, null, 1)); // Binder
		listHook.add(new XBinder(Methods.transact, null, 1)); // BinderProxy
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.execTransact)
			checkIPC(param);

		else if (mMethod == Methods.transact)
			markIPC(param);

		else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}

	private void markIPC(XParam param) throws Throwable {
		// Allow management transaction
		int code = (Integer) param.args[0];
		if (isManagementTransaction(code))
			return;

		// Only for applications
		int uid = Binder.getCallingUid();
		if (!PrivacyManager.isApplication(uid))
			return;

		// Get interface name
		IBinder binder = (IBinder) param.thisObject;
		String descriptor = (binder == null ? null : binder.getInterfaceDescriptor());

		// Check if listed descriptor
		int idx = cServiceDescriptor.indexOf(descriptor);
		if (idx >= 0) {
			// Search this object in call stack
			boolean ok = false;
			boolean found = false;
			if (PrivacyManager.getSettingBool(0, PrivacyManager.cSettingNoJavaIPC, false))
				ok = true;
			else {
				StackTraceElement[] ste = Thread.currentThread().getStackTrace();
				for (int i = 0; i < ste.length; i++)
					if (ste[i].getClassName().equals(param.thisObject.getClass().getName())) {
						found = true;

						// Check if caller class in user space
						String callerClassName = (i + 2 < ste.length ? ste[i + 2].getClassName() : null);
						if (callerClassName != null && !callerClassName.startsWith("java.lang.reflect."))
							synchronized (mMapClassSystem) {
								if (!mMapClassSystem.containsKey(callerClassName))
									try {
										ClassLoader loader = Thread.currentThread().getContextClassLoader();
										Class<?> clazz = Class.forName(callerClassName, false, loader);
										boolean boot = Context.class.getClassLoader().equals(clazz.getClassLoader());
										mMapClassSystem.put(callerClassName, boot);
									} catch (ClassNotFoundException ignored) {
										mMapClassSystem.put(callerClassName, true);
									}
								ok = mMapClassSystem.get(callerClassName);
							}

						break;
					}
			}

			// Conditionally mark
			if (ok) {
				int flags = (Integer) param.args[3];
				if ((flags & ~FLAG_ALL) != 0)
					Util.log(this, Log.ERROR, "Unknown flags=" + Integer.toHexString(flags) + " descriptor="
							+ descriptor + " code=" + code + " uid=" + Binder.getCallingUid());
				flags |= (mToken << BITS_TOKEN);
				param.args[3] = flags;
			} else {
				int level = (found ? Log.WARN : Log.ERROR);
				Util.log(this, level, "Unmarked descriptor=" + descriptor + " found=" + found + " code=" + code
						+ " uid=" + Binder.getCallingUid());
				Util.logStack(this, level, true);
			}
		}
	}

	// Entry point from android_util_Binder.cpp's onTransact
	private void checkIPC(XParam param) throws Throwable {
		// Allow management transaction
		int code = (Integer) param.args[0];
		if (isManagementTransaction(code))
			return;

		// Only for applications
		int uid = Binder.getCallingUid();
		if (!PrivacyManager.isApplication(uid))
			return;

		// Get interface name
		IBinder binder = (IBinder) param.thisObject;
		String descriptor = (binder == null ? null : binder.getInterfaceDescriptor());
		if (!cServiceDescriptor.contains(descriptor))
			return;

		// Get token
		int flags = (Integer) param.args[3];
		long token = (flags >> BITS_TOKEN) & MASK_TOKEN;
		flags &= FLAG_ALL;
		param.args[3] = flags;

		// Check token
		if (token != mToken) {
			String[] name = descriptor.split("\\.");
			String methodName = name[name.length - 1];

			// Get transaction code name
			String codeName;
			synchronized (mMapCodeName) {
				if (!mMapCodeName.containsKey(descriptor)) {
					SparseArray<String> sa = new SparseArray<String>();
					mMapCodeName.put(descriptor, sa);
					try {
						Class<?> superClass;
						if ("android.app.IActivityManager".equals(descriptor))
							superClass = Class.forName("android.app.IActivityManager");
						else
							superClass = param.thisObject.getClass().getSuperclass();
						if (superClass != null)
							for (Field field : superClass.getDeclaredFields())
								try {
									if (field.getName().startsWith("TRANSACTION_")
											|| field.getName().endsWith("_TRANSACTION")) {
										Integer txCode = (Integer) field.get(param.thisObject);
										String txName = field.getName().replace("TRANSACTION_", "")
												.replace("_TRANSACTION", "");
										sa.put(txCode, txName);
									}
								} catch (Throwable ignore) {
								}
					} catch (Throwable ignored) {
					}
				}

				codeName = mMapCodeName.get(descriptor).get(code);
			}
			if (codeName == null) {
				codeName = Integer.toString(code);
				Util.log(this, Log.INFO,
						"Unknown transaction=" + descriptor + ":" + code + " uid=" + Binder.getCallingUid());
				Util.logStack(this, Log.INFO);
			}

			Util.log(this, Log.INFO, "can restrict transaction=" + methodName + ":" + codeName + " flags=" + flags
					+ " uid=" + uid + " my=" + Process.myUid());

			if (isRestrictedExtra(uid, PrivacyManager.cIPC, "Binder", methodName + ":" + codeName)) {
				// Get reply parcel
				Parcel reply = null;
				try {
					// static protected final Parcel obtain(int obj)
					// frameworks/base/core/java/android/os/Parcel.java
					Method methodObtain = Parcel.class.getDeclaredMethod("obtain", int.class);
					methodObtain.setAccessible(true);
					reply = (Parcel) methodObtain.invoke(null, param.args[2]);
				} catch (NoSuchMethodException ex) {
					Util.bug(this, ex);
				}

				// Block IPC
				if (reply == null)
					Util.log(this, Log.ERROR, "reply is null uid=" + uid);
				else {
					reply.setDataPosition(0);
					reply.writeException(new SecurityException("XPrivacy"));
				}
				param.setResult(true);
			}
		}
	}

	private static boolean isManagementTransaction(int code) {
		return (code == PING_TRANSACTION || code == DUMP_TRANSACTION || code == INTERFACE_TRANSACTION
				|| code == TWEET_TRANSACTION || code == LIKE_TRANSACTION || code == SYSPROPS_TRANSACTION);

	}
}

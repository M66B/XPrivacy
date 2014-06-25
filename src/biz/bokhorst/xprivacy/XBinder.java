package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.os.Binder;
import android.os.IBinder;
import android.os.Parcel;
import android.os.Process;
import android.util.Log;

public class XBinder extends XHook {
	private Methods mMethod;

	private static long mToken = 0;
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
	// TODO: service list

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
		"usb"
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
		"android.hardware.usb.IUsbManager"
	});
	// @formatter:on

	// @formatter:off
	public static List<String> cServiceClassName = Arrays.asList(new String[] {
		"android.accounts.AccountManager",
		"android.app.ActivityManager",
		"android.content.ClipboardManager",
		"android.net.ConnectivityManager",
		"android.content.ContentResolver,android.content.ContentProviderClient",
		"android.location.LocationManager",
		"android.telephony.TelephonyManager",
		"android.telephony.TelephonyManager",
		"android.app.ApplicationPackageManager",
		"android.telephony.TelephonyManager",
		"android.telephony.TelephonyManager",
		"android.view.WindowManagerImpl,android.view.ViewRootImpl,android.view.View,android.view.Display",
		"android.net.wifi.WifiManager",
		"android.net.sip.SipManager",
		"android.telephony.SmsManager",
		"android.nfc.NfcActivityManager,android.nfc.NfcAdapter",
		"android.appwidget.AppWidgetManager,android.appwidget.AppWidgetHost",
		"android.bluetooth.BluetoothAdapter,android.bluetooth.BluetoothDevice",
		"android.hardware.input.InputManager",
		"android.hardware.SystemSensorManager",
		"android.hardware.usb.UsbManager"
	});
	// @formatter:on

	// @formatter:off
	// Allow some common internal calls
	public static List<String[]> cExceptionClassName = Arrays.asList(new String[][] {
		new String[] { // AccountManager
		},
		new String[] { // ActivityManager
			"android.app.Activity",
			"android.app.ActivityThread",
			"android.app.ActivityThread$Idler",
			"android.app.ActivityThread$StopInfo",
			"android.app.ContextImpl",
			"android.app.Instrumentation",
			"android.app.LoadedApk",
			"android.app.PendingIntent",
			"android.app.Service",
			"android.content.ContentResolver",
			"android.content.BroadcastReceiver$PendingResult",
			"android.hardware.SensorManager",
			"android.os.StrictMode$AndroidBlockGuardPolicy",
			"com.android.internal.os.RuntimeInit$UncaughtHandler",
		},
		new String[] { // ClipboardManager
		},
		new String[] { // ConnectivityManager
			"android.app.ActivityThread",
		},
		new String[] { // ContentProvider
		},
		new String[] { // LocationManager
			"android.location.Geocoder",
		},
		new String[] { // TelephonyManager
		},
		new String[] { // TelephonyManager
		},
		new String[] { // PackageManager
			"android.app.ActivityThread",
			"android.app.LoadedApk",
			"android.app.ResourcesManager",
			"android.content.res.Resources",
			"android.nfc.NfcAdapter",
		},
		new String[] { // TelephonyManager
		},
		new String[] { // TelephonyManager
		},
		new String[] { // WindowManager
			"android.app.KeyguardManager",
			"android.hardware.LegacySensorManager",
		},
		new String[] { // WifiManager
		},
		new String[] { // SipManager
		},
		new String[] { // SmsManager
		},
		new String[] { // NfcManager
		},
		new String[] { // AppWidgetManager
		},
		new String[] { // BluetoothManager
		},
		new String[] { // InputManager
		},
		new String[] { // SensorManager
		},
		new String[] { // UsbManager
		},
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

		int uid = Binder.getCallingUid();
		if (PrivacyManager.isApplication(uid)) {
			// Get interface name
			IBinder binder = (IBinder) param.thisObject;
			String descriptor = (binder == null ? null : binder.getInterfaceDescriptor());

			// Check if listed descriptor
			int idx = cServiceDescriptor.indexOf(descriptor);
			if (idx >= 0) {
				// Search class in call stack
				boolean ok = false;
				boolean white = false;
				boolean found = false;
				String proxy = descriptor.replace(".I", ".") + "Proxy";
				String[] serviceClassName = cServiceClassName.get(idx).split(",");
				StackTraceElement[] ste = Thread.currentThread().getStackTrace();
				for (int i = 0; i < ste.length; i++)
					if (ste[i].getClassName().startsWith(descriptor) || ste[i].getClassName().startsWith(proxy)) {
						found = true;

						// Check exceptions
						if (i + 1 < ste.length)
							if (Arrays.asList(cExceptionClassName.get(idx)).contains(ste[i + 1].getClassName())) {
								white = true;
								break;
							}

						// Check manager class name
						for (int j = i + 1; j < ste.length; j++) {
							for (String name : serviceClassName)
								if (ste[j].getClassName().startsWith(name)) {
									ok = true;
									break;
								}
							if (ok)
								break;
						}
						break;
					}

				// Internal checks
				if (!found) {
					Util.log(this, Log.ERROR,
							"Missing descriptor=" + descriptor + " code=" + code + " uid=" + Binder.getCallingUid());
					Util.logStack(this, Log.ERROR);
				}
				if (white)
					if (ok) {
						Util.log(this, Log.ERROR, "Whitelisted descriptor=" + descriptor + " code=" + code + " uid="
								+ Binder.getCallingUid());
						Util.logStack(this, Log.ERROR);
					} else
						ok = true;

				// Conditionally mark
				if (ok) {
					int flags = (Integer) param.args[3];
					if ((flags & ~FLAG_ALL) != 0)
						Util.log(this, Log.ERROR, "Unknown flags=" + Integer.toHexString(flags) + " descriptor="
								+ descriptor + " code=" + code + " uid=" + Binder.getCallingUid());
					flags |= (mToken << BITS_TOKEN);
					param.args[3] = flags;
				}

				if (!ok && !PrivacyService.getClient().isSystemApp(uid)) {
					Util.log(this, Log.ERROR,
							"Unmarked descriptor=" + descriptor + " code=" + code + " uid=" + Binder.getCallingUid());
					Util.logStack(this, Log.ERROR);
				}
			}
		}
	}

	private void checkIPC(XParam param) throws Throwable {
		// Entry point from android_util_Binder.cpp's onTransact
		int code = (Integer) param.args[0];
		int flags = (Integer) param.args[3];
		long token = (flags >> BITS_TOKEN) & MASK_TOKEN;
		flags &= FLAG_ALL;
		param.args[3] = flags;

		// Allow management transaction
		if (isManagementTransaction(code))
			return;

		int uid = Binder.getCallingUid();
		if (token != mToken && PrivacyManager.isApplication(uid)) {
			// Get interface name
			IBinder binder = (IBinder) param.thisObject;
			String descriptor = (binder == null ? null : binder.getInterfaceDescriptor());
			if (cServiceDescriptor.contains(descriptor)) {
				String[] name = descriptor.split("\\.");
				String methodName = name[name.length - 1];
				Util.log(this, Log.INFO, "can restrict method=" + methodName + " code=" + code + " flags=" + flags
						+ " uid=" + uid + " my=" + Process.myUid());
				if (isRestrictedExtra(uid, PrivacyManager.cIPC, methodName, Integer.toString(code))) {
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
	}

	private static boolean isManagementTransaction(int code) {
		return (code == PING_TRANSACTION || code == DUMP_TRANSACTION || code == INTERFACE_TRANSACTION
				|| code == TWEET_TRANSACTION || code == LIKE_TRANSACTION || code == SYSPROPS_TRANSACTION);

	}
}

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
	private static int BITS_TOKEN = 16;
	private static int FLAG_ALL = 0xFFFF;
	private static int MASK_TOKEN = 0xFFFF;

	int PING_TRANSACTION = ('_' << 24) | ('P' << 16) | ('N' << 8) | 'G';
	int DUMP_TRANSACTION = ('_' << 24) | ('D' << 16) | ('M' << 8) | 'P';
	int INTERFACE_TRANSACTION = ('_' << 24) | ('N' << 16) | ('T' << 8) | 'F';
	int TWEET_TRANSACTION = ('_' << 24) | ('T' << 16) | ('W' << 8) | 'T';
	int LIKE_TRANSACTION = ('_' << 24) | ('L' << 16) | ('I' << 8) | 'K';
	int SYSPROPS_TRANSACTION = ('_' << 24) | ('S' << 16) | ('P' << 8) | 'R';

	// Service name should one-to-one correspond to a service descriptor
	// TODO: sensor interface

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
		"wifi"
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
		"android.net.wifi.IWifiManager"
	});
	// @formatter:on

	// @formatter:off
	public static List<String> cServiceClassName = Arrays.asList(new String[] {
		"android.accounts.AccountManager",
		"android.app.ActivityManager",
		"android.text.ClipboardManager,android.content.ClipboardManager",
		"android.net.ConnectivityManager",
		"android.content.ContentResolver,android.content.ContentProviderClient",
		"android.location.LocationManager",
		"android.telephony.TelephonyManager",
		"android.telephony.TelephonyManager",
		"android.content.pm.PackageManager,android.app.ApplicationPackageManager",
		"android.telephony.TelephonyManager",
		"android.telephony.TelephonyManager",
		"android.view.WindowManager",
		"android.net.wifi.WifiManager"
	});
	// @formatter:on

	// @formatter:off
	public static List<String> cWhiteList = Arrays.asList(new String[] {
		"android.app.Activity",
		"android.app.ActivityThread",
		"android.app.ActivityThread$Idler",
		"android.app.ActivityThread$StopInfo",
		"android.app.ContextImpl",
		"android.app.Instrumentation",
		"android.app.LoadedApk",
		"android.app.PendingIntent",
		"android.app.Service",

		"android.content.BroadcastReceiver$PendingResult",
		"com.android.providers.contacts.ContactsProvider2",
		"com.android.location.provider.LocationProviderBase",

		"android.view.ViewConfiguration",

		"android.nfc.NfcAdapter",

		"com.android.systemui.statusbar.phone.NavigationBarView",
		"com.android.systemui.statusbar.phone.PhoneStatusBar",
		"com.android.systemui.statusbar.phone.QuickSettings",

		"android.app.KeyguardManager",
		"com.android.keyguard.KeyguardUpdateMonitor",
		"com.android.keyguard.KeyguardViewMediator",

		"com.android.internal.widget.LockPatternUtils",

		//"com.google.android.partnersetup.MccFallback",
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
				String[] className = cServiceClassName.get(idx).split(",");
				StackTraceElement[] ste = Thread.currentThread().getStackTrace();
				for (int i = 0; i < ste.length; i++)
					if (ste[i].getClassName().startsWith(descriptor) || ste[i].getClassName().startsWith(proxy)) {
						found = true;

						// Check white list
						if (i + 1 < ste.length)
							for (String whiteListed : cWhiteList)
								if (ste[i + 1].getClassName().equals(whiteListed)) {
									white = true;
									// TODO: set OK
									break;
								}

						// Check manager class name
						if (!ok)
							for (int j = i + 1; j < ste.length; j++) {
								for (String cname : className)
									if (ste[j].getClassName().startsWith(cname)) {
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
					Util.log(this, Log.ERROR, "Missing descriptor=" + descriptor);
					Util.logStack(this, Log.ERROR);
				}
				if (white)
					if (ok) {
						Util.log(this, Log.ERROR, "Whitelisted " + descriptor);
						Util.logStack(this, Log.ERROR);
					} else
						ok = true;

				// Conditionally mark
				if (ok) {
					int flags = (Integer) param.args[3];
					if ((flags & ~FLAG_ALL) != 0)
						Util.log(this, Log.ERROR, "Unknown flags=" + Integer.toHexString(flags));
					flags |= (mToken << BITS_TOKEN);
					param.args[3] = flags;
				} else {
					Util.log(this, Log.WARN, "Unmarked descriptor=" + descriptor);
					Util.logStack(this, Log.WARN);
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
		if (code == PING_TRANSACTION || code == DUMP_TRANSACTION || code == INTERFACE_TRANSACTION
				|| code == TWEET_TRANSACTION || code == LIKE_TRANSACTION || code == SYSPROPS_TRANSACTION)
			return;

		int uid = Binder.getCallingUid();
		if (token != mToken && PrivacyManager.isApplication(uid)) {
			// Get interface name
			IBinder binder = (IBinder) param.thisObject;
			String descriptor = (binder == null ? null : binder.getInterfaceDescriptor());
			if (cServiceDescriptor.contains(descriptor)) {
				Util.log(this, Log.WARN, "can restrict name=" + descriptor + " code=" + code + " flags=" + flags
						+ " uid=" + uid + " my=" + Process.myUid());
				String[] name = descriptor.split("\\.");
				if (getRestricted(uid, PrivacyManager.cIPC, name[name.length - 1])) {
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
}

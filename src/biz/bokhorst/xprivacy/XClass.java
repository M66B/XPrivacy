package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.util.Log;

public class XClass extends XHook {
	private Methods mMethod;

	private XClass(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	private XClass(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "java.lang.Class";
	}

	// public Field getDeclaredField(String name)
	// public Field[] getDeclaredFields()
	// public Method getDeclaredMethod(String name, Class...<?> parameterTypes)
	// public Method getDeclaredMethods()
	// public Field getField(String name)
	// public Field[] getFields()
	// public Method getMethod(String name, Class...<?> parameterTypes)
	// public Method[] getMethods()
	// http://developer.android.com/reference/java/lang/Class.html

	private enum Methods {
		getDeclaredField, getDeclaredFields, getDeclaredMethod, getDeclaredMethods, getField, getFields, getMethod, getMethods
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XClass(Methods.getDeclaredField, PrivacyManager.cIPC));
		listHook.add(new XClass(Methods.getDeclaredFields, PrivacyManager.cIPC));
		listHook.add(new XClass(Methods.getDeclaredMethod, PrivacyManager.cIPC));
		listHook.add(new XClass(Methods.getDeclaredMethods, PrivacyManager.cIPC));
		listHook.add(new XClass(Methods.getField, PrivacyManager.cIPC));
		listHook.add(new XClass(Methods.getFields, PrivacyManager.cIPC));
		listHook.add(new XClass(Methods.getMethod, PrivacyManager.cIPC));
		listHook.add(new XClass(Methods.getMethods, PrivacyManager.cIPC));
		return listHook;
	}

	// @formatter:off
	public static List<String> cClassName = Arrays.asList(new String[] {
		"android.accounts.AccountManager",
		"android.app.Activity",
		"android.app.ActivityManager",
		// "com.android.server.am.ActivityManagerService",
		"com.google.android.gms.location.ActivityRecognitionClient",
		"com.google.android.gms.ads.identifier.AdvertisingIdClient$Info",
		"android.app.Application",
		"android.appwidget.AppWidgetManager",
		"android.media.AudioRecord",
		// "android.os.BinderProxy",
		// "android.os.Binder",
		"android.bluetooth.BluetoothAdapter",
		"android.bluetooth.BluetoothDevice",
		"android.hardware.Camera",
		"android.content.ClipboardManager",
		"android.net.ConnectivityManager",
		"android.content.ContentProviderClient",
		"android.content.ContentResolver",
		"android.app.ContextImpl",
		"android.os.Environment",
		"com.google.android.gms.auth.GoogleAuthUtil",
		"java.net.InetAddress",
		"android.view.InputDevice",
		// "libcore.io.IoBridge",
		"com.google.android.gms.location.LocationClient",
		"android.location.LocationManager",
		"android.media.MediaRecorder",
		"android.net.NetworkInfo",
		"java.net.NetworkInterface",
		"android.nfc.NfcAdapter",
		"android.app.ApplicationPackageManager",
		"android.os.Process",
		"java.lang.ProcessBuilder",
		"android.content.res.Resources",
		"android.content.res.Configuration",
		"java.lang.Runtime",
		"android.hardware.SystemSensorManager",
		// "android.provider.Settings.Secure",
		"android.net.sip.SipManager",
		"android.telephony.SmsManager",
		"android.os.SystemProperties",
		"android.telephony.TelephonyManager",
		// WebSettings
		"android.webkit.WebView",
		"android.net.wifi.WifiManager",
		"android.view.WindowManagerImpl"
	});
	// @formatter:on

	@Override
	protected void before(XParam param) throws Throwable {
		String className = ((Class<?>) param.thisObject).getName();
		if (!className.contains(className))
			return;

		if (mMethod == Methods.getDeclaredField || mMethod == Methods.getField) {
			if (param.args.length > 0)
				if (isRestrictedExtra(param, className + "." + param.args[0]))
					param.setThrowable(new NoSuchFieldException("XPrivacy"));

		} else if (mMethod == Methods.getDeclaredFields || mMethod == Methods.getFields) {
			if (isRestricted(param))
				param.setResult(new Field[0]);

		} else if (mMethod == Methods.getDeclaredMethod || mMethod == Methods.getMethod) {
			if (param.args.length > 0)
				if (isRestrictedExtra(param, className + "." + param.args[0]))
					param.setThrowable(new NoSuchMethodException("XPrivacy"));

		} else if (mMethod == Methods.getDeclaredMethods || mMethod == Methods.getMethods) {
			if (isRestricted(param))
				param.setResult(new Method[0]);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

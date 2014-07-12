package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.os.Binder;

public class XField extends XHook {
	@SuppressWarnings("unused")
	private Methods mMethod;

	private XField(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "Reflection");
		mMethod = method;
	}

	private XField(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), "Reflection", sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "java.lang.reflect.Field";
	}

	// public Object get(Object obj)
	// public boolean getBoolean(Object obj)
	// public byte getByte(Object obj)
	// public char getChar(Object obj)
	// public short getShort(Object obj)
	// public int getInt(Object obj)
	// public long getLong(Object obj)
	// public float getFloat(Object obj)
	// public double getDouble(Object obj)
	// http://developer.android.com/reference/java/lang/reflect/Field.html

	private enum Methods {
		get, getBoolean, getByte, getChar, getShort, getInt, getLong, getFloat, getDouble
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XField(Methods.get, PrivacyManager.cIPC));
		listHook.add(new XField(Methods.getBoolean, PrivacyManager.cIPC));
		listHook.add(new XField(Methods.getByte, PrivacyManager.cIPC));
		listHook.add(new XField(Methods.getChar, PrivacyManager.cIPC));
		listHook.add(new XField(Methods.getShort, PrivacyManager.cIPC));
		listHook.add(new XField(Methods.getInt, PrivacyManager.cIPC));
		listHook.add(new XField(Methods.getLong, PrivacyManager.cIPC));
		listHook.add(new XField(Methods.getFloat, PrivacyManager.cIPC));
		listHook.add(new XField(Methods.getDouble, PrivacyManager.cIPC));
		return listHook;
	}

	// @formatter:off
	public static List<String> cClassName = Arrays.asList(new String[] {
		"android.accounts.AccountManager",
		"android.app.Activity",
		"android.app.ActivityManager",
		"android.app.ActivityManagerEx",
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
		"android.net.MultiSimConnectivityManager",
		"android.content.ContentProviderClient",
		"android.content.ContentResolver",
		"android.app.ContextImpl",
		"android.os.Environment",
		"com.google.android.gms.auth.GoogleAuthUtil",
		"java.net.InetAddress",
		"android.view.InputDevice",
		// "libcore.io.IoBridge",
		"android.net.LinkAddress",
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
		"android.hardware.SensorManager",
		"android.hardware.SystemSensorManager",
		// "android.provider.Settings.Secure",
		"android.net.sip.SipManager",
		"android.telephony.SmsManager",
		"android.os.SystemProperties",
		"android.telephony.TelephonyManager",
		"android.telephony.MSimTelephonyManager",
		"android.telephony.MultiSimTelephonyManager",
		// WebSettings
		"android.webkit.WebView",
		"android.net.wifi.WifiManager",
		"android.view.WindowManagerImpl",
		"android.view.Window$LocalWindowManager",

		"com.google.android.maps.GeoPoint",
		"com.google.android.gms.maps.model.Marker",
		"com.google.android.gms.maps.GoogleMap"
	});
	// @formatter:on

	@Override
	protected void before(XParam param) throws Throwable {
		Field field = (Field) param.thisObject;
		String className = field.getDeclaringClass().getName();

		// Make accessible: Xposed will invoke original method
		param.setObjectExtra("accessible", (Boolean) field.isAccessible());
		field.setAccessible(true);

		// Check if Android
		if (Binder.getCallingUid() == android.os.Process.SYSTEM_UID)
			return;

		// Check if class listed
		if (!cClassName.contains(className))
			return;

		// Check exceptions
		String fieldName = field.getName();
		if ("CREATOR".equals(fieldName))
			return;
		if ("java.net.Inet4Address".equals(className) && "ANY".equals(fieldName))
			return;
		if ("android.net.wifi.WifiInfo".equals(className) && "mWifiSsid".equals(fieldName))
			return;

		// Check if restricted
		if (isRestrictedExtra(param, className + "." + fieldName))
			param.setThrowable(new IllegalAccessException("XPrivacy"));
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Restore accessibility
		Field field = (Field) param.thisObject;
		field.setAccessible((Boolean) param.getObjectExtra("accessible"));
	}
}

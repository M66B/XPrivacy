package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.content.Context;
import android.os.Binder;
import android.os.Bundle;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Process;
import android.telephony.CellInfo;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XTelephonyRegistry extends XHook {
	private Methods mMethod;
	private static boolean mHookedLocation = false;
	private static boolean mHookedPhone = false;
	private static Map<IBinder, Integer> mListenerUid = new WeakHashMap<IBinder, Integer>();

	private XTelephonyRegistry(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()));
		mMethod = method;
	}

	private XTelephonyRegistry(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()), sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.server.TelephonyRegistry";
	}

	// @formatter:off

	// public void listen(String pkgForDebug, IPhoneStateListener callback, int events, boolean notifyNow)
	// frameworks/base/services/java/com/android/server/TelephonyRegistry.java
	// http://developer.android.com/reference/android/telephony/TelephonyManager.html

	// @formatter:on

	private enum Methods {
		listen
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XTelephonyRegistry(Methods.listen, PrivacyManager.cLocation));
		listHook.add(new XTelephonyRegistry(Methods.listen, PrivacyManager.cPhone));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.listen) {
			if (param.args.length > 2 && param.args[1] != null)
				if (param.args[1].getClass().getName().startsWith("com.android.internal.telephony.IPhoneStateListener")) {
					IBinder binder = ((IInterface) param.args[1]).asBinder();
					int event = (Integer) param.args[2];
					if (event == android.telephony.PhoneStateListener.LISTEN_NONE) {
						// Remove listener
						synchronized (mListenerUid) {
							if (mListenerUid.containsKey(binder))
								mListenerUid.remove(binder);
							else
								Util.log(this, Log.WARN, "Remove: listener not found");
						}
					} else {
						// Add listener
						int uid = Binder.getCallingUid();
						synchronized (mListenerUid) {
							mListenerUid.put(binder, uid);
						}

						// Hook
						if (!mHookedLocation && "location".equals(getRestrictionName())) {
							hookOn(param, "onCellLocationChanged");
							hookOn(param, "onCellInfoChanged");
							mHookedLocation = true;
						}

						if (!mHookedPhone && "phone".equals(getRestrictionName())) {
							hookOn(param, "onCallStateChanged");
							mHookedPhone = true;
						}
					}
				}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	private void hookOn(final MethodHookParam param, final String methodName) {
		try {
			// void onCallStateChanged(int state, String incomingNumber)
			// void onCellLocationChanged(Bundle location)
			// void onCellInfoChanged(List<CellInfo> cellInfo)
			Method on = null;
			if ("onCallStateChanged".equals(methodName))
				on = param.args[1].getClass().getDeclaredMethod(methodName, int.class, String.class);
			if ("onCellLocationChanged".equals(methodName))
				on = param.args[1].getClass().getDeclaredMethod(methodName, Bundle.class);
			if ("onCellInfoChanged".equals(methodName))
				on = param.args[1].getClass().getDeclaredMethod(methodName, List.class);
			XposedBridge.hookMethod(on, new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam onparam) throws Throwable {
					// Get uid
					int uid = 0;
					IBinder binder = ((IInterface) onparam.thisObject).asBinder();
					synchronized (mListenerUid) {
						if (mListenerUid.containsKey(binder))
							uid = mListenerUid.get(binder);
						else
							Util.log(XTelephonyRegistry.this, Log.WARN, "Get: listener not found");
					}

					// Restrict
					if (uid > 0) {
						if (methodName.equals("onCallStateChanged")) {
							if (onparam.args.length > 1 && onparam.args[1] != null)
								if (isRestricted(param, uid))
									onparam.args[1] = PrivacyManager.getDefacedProp(uid, "PhoneNumber");
						} else if (methodName.equals("onCellLocationChanged")) {
							if (onparam.args.length > 0 && onparam.args[0] != null)
								if (isRestricted(param, uid))
									onparam.args[0] = new Bundle();
						} else if (methodName.equals("onCellInfoChanged")) {
							if (onparam.args.length > 0 && onparam.args[0] != null)
								if (isRestricted(param, uid))
									onparam.args[0] = new ArrayList<CellInfo>();
						}
					}
				}
			});
			Util.log(this, Log.WARN, "Hooked " + on + " uid=" + Process.myUid());
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		int uid = Binder.getCallingUid();
		return isRestricted(param, uid);
	}

	private boolean isRestricted(MethodHookParam param, int uid) throws Throwable {
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
		return getRestricted(context, uid, true);
	}
}

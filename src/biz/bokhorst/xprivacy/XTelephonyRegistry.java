package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.content.Context;
import android.os.Binder;
import android.os.Bundle;
import android.os.IBinder;
import android.telephony.CellInfo;
import android.telephony.ServiceState;
import android.telephony.SignalStrength;
import android.util.Log;

import com.android.internal.telephony.IPhoneStateListener;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XTelephonyRegistry extends XHook {
	private Methods mMethod;
	private static final Map<IPhoneStateListener, XIPhoneStateListener> mListener = new WeakHashMap<IPhoneStateListener, XIPhoneStateListener>();

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
		if (mMethod == Methods.listen)
			replacePhoneStateListener(param);
		else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	private void replacePhoneStateListener(MethodHookParam param) throws Throwable {
		if (param.args.length > 2 && param.args[1] != null)
			if (!(param.args[1] instanceof XIPhoneStateListener))
				if (param.args[1] instanceof IPhoneStateListener) {
					if (isRestricted(param)) {
						int event = (Integer) param.args[2];
						IPhoneStateListener listener = (IPhoneStateListener) param.args[1];
						if (event == android.telephony.PhoneStateListener.LISTEN_NONE) {
							// Remove
							synchronized (mListener) {
								XIPhoneStateListener xlistener = mListener.get(listener);
								if (xlistener == null)
									Util.log(this, Log.WARN, "Not found count=" + mListener.size());
								else {
									param.args[1] = xlistener;
									mListener.remove(listener);
								}
							}
						} else {
							// Replace
							XIPhoneStateListener xListener = new XIPhoneStateListener(listener);
							synchronized (mListener) {
								mListener.put(listener, xListener);
								Util.log(this, Log.INFO, "Added count=" + mListener.size());
							}
							param.args[1] = xListener;
						}
					}
				} else
					Util.log(this, Log.WARN, "method=" + param.method);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;

		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (NoSuchFieldError ex) {
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}

		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	private class XIPhoneStateListener implements IPhoneStateListener {
		private IPhoneStateListener mListener;

		public XIPhoneStateListener(IPhoneStateListener listener) {
			mListener = listener;
		}

		@Override
		public void onCallForwardingIndicatorChanged(boolean cfi) {
			mListener.onCallForwardingIndicatorChanged(cfi);
		}

		@Override
		public void onCallStateChanged(int state, String incomingNumber) {
			mListener.onCallStateChanged(state,
					(String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "PhoneNumber"));
		}

		@Override
		public void onCellInfoChanged(List<CellInfo> cellInfo) {
			mListener.onCellInfoChanged(new ArrayList<CellInfo>());
		}

		@Override
		public void onCellLocationChanged(Bundle location) {
			mListener.onCellLocationChanged(new Bundle());
		}

		@Override
		public void onDataActivity(int direction) {
			mListener.onDataActivity(direction);
		}

		@Override
		public void onDataConnectionStateChanged(int state, int networkType) {
			mListener.onDataConnectionStateChanged(state, networkType);
		}

		@Override
		public void onMessageWaitingIndicatorChanged(boolean mwi) {
			mListener.onMessageWaitingIndicatorChanged(mwi);
		}

		@Override
		public void onServiceStateChanged(ServiceState serviceState) {
			mListener.onServiceStateChanged(serviceState);
		}

		@Override
		public void onSignalStrengthChanged(int asu) {
			mListener.onSignalStrengthChanged(asu);
		}

		@Override
		public void onSignalStrengthsChanged(SignalStrength signalStrength) {
			mListener.onSignalStrengthsChanged(signalStrength);
		}

		@Override
		public void onOtaspChanged(int otaspMode) {
			mListener.onOtaspChanged(otaspMode);
		}

		@Override
		public IBinder asBinder() {
			return mListener.asBinder();
		}
	}
}

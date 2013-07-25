package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.content.Context;
import android.os.Binder;
import android.telephony.CellLocation;
import android.telephony.NeighboringCellInfo;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.SignalStrength;
import android.telephony.CellInfo;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XTelephonyManager extends XHook {
	private static final Map<PhoneStateListener, XPhoneStateListener> mListener = new WeakHashMap<PhoneStateListener, XPhoneStateListener>();

	public XTelephonyManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public void disableLocationUpdates()
	// public void enableLocationUpdates()
	// public List<CellInfo> getAllCellInfo()
	// public CellLocation getCellLocation()
	// public String getDeviceId()
	// public String getGroupIdLevel1()
	// public String getIsimDomain()
	// public String getIsimImpi()
	// public String[] getIsimImpu()
	// public String getLine1AlphaTag()
	// public String getLine1Number()
	// public String getMsisdn()
	// public List<NeighboringCellInfo> getNeighboringCellInfo()
	// public String getNetworkCountryIso()
	// public String getNetworkOperator()
	// public String getNetworkOperatorName()
	// public String getSimCountryIso()
	// public String getSimOperator()
	// public String getSimOperatorName()
	// public String getSimSerialNumber()
	// public String getSubscriberId()
	// public String getVoiceMailAlphaTag()
	// public String getVoiceMailNumber()
	// public void listen(PhoneStateListener listener, int events)
	// frameworks/base/telephony/java/android/telephony/TelephonyManager.java
	// http://developer.android.com/reference/android/telephony/TelephonyManager.html

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("listen")) {
			if (param.args.length > 1) {
				PhoneStateListener listener = (PhoneStateListener) param.args[0];
				int event = (Integer) param.args[1];
				if (listener != null)
					if (isRestricted(param)) {
						if (event == PhoneStateListener.LISTEN_NONE) {
							// Remove
							synchronized (mListener) {
								XPhoneStateListener xlistener = mListener.get(listener);
								if (xlistener == null)
									Util.log(this, Log.WARN, "Not found count=" + mListener.size());
								else {
									param.args[0] = xlistener;
									mListener.remove(listener);
								}
							}
						} else {
							// Replace
							XPhoneStateListener xListener = new XPhoneStateListener(listener);
							synchronized (mListener) {
								mListener.put(listener, xListener);
								Util.log(this, Log.INFO, "Added count=" + mListener.size());
							}
							param.args[0] = xListener;
						}
					}
			}
		} else if (methodName.equals("disableLocationUpdates") || methodName.equals("enableLocationUpdates"))
			if (isRestricted(param))
				param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (!methodName.equals("listen") && !methodName.equals("disableLocationUpdates")
				&& !methodName.equals("enableLocationUpdates"))
			if (param.getResult() != null)
				if (isRestricted(param))
					if (methodName.equals("getAllCellInfo"))
						param.setResult(new ArrayList<CellInfo>());
					else if (methodName.equals("getCellLocation"))
						param.setResult(CellLocation.getEmpty());
					else if (methodName.equals("getIsimImpu"))
						param.setResult(PrivacyManager.getDefacedProp(methodName));
					else if (methodName.equals("getNeighboringCellInfo"))
						param.setResult(new ArrayList<NeighboringCellInfo>());
					else
						param.setResult(PrivacyManager.getDefacedProp(methodName));
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;

		// TelephonyManager
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "sContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}

		// MultiSimTelephonyManager
		if (context == null)
			try {
				Field fieldContext = findField(param.thisObject.getClass(), "mContext");
				context = (Context) fieldContext.get(param.thisObject);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}

		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	private class XPhoneStateListener extends PhoneStateListener {
		private PhoneStateListener mListener;

		public XPhoneStateListener(PhoneStateListener listener) {
			mListener = listener;
		}

		@Override
		public void onCallForwardingIndicatorChanged(boolean cfi) {
			mListener.onCallForwardingIndicatorChanged(cfi);
		}

		@Override
		public void onCallStateChanged(int state, String incomingNumber) {
			mListener.onCallStateChanged(state, (String) PrivacyManager.getDefacedProp("PhoneNumber"));
		}

		@Override
		public void onCellInfoChanged(List<CellInfo> cellInfo) {
			mListener.onCellInfoChanged(new ArrayList<CellInfo>());
		}

		@Override
		public void onCellLocationChanged(CellLocation location) {
			mListener.onCellLocationChanged(CellLocation.getEmpty());
		}

		@Override
		public void onDataActivity(int direction) {
			mListener.onDataActivity(direction);
		}

		@Override
		public void onDataConnectionStateChanged(int state) {
			mListener.onDataConnectionStateChanged(state);
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
		@SuppressWarnings("deprecation")
		public void onSignalStrengthChanged(int asu) {
			mListener.onSignalStrengthChanged(asu);
		}

		@Override
		public void onSignalStrengthsChanged(SignalStrength signalStrength) {
			mListener.onSignalStrengthsChanged(signalStrength);
		}
	}
}

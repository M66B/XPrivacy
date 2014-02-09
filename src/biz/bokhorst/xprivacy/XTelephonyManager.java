package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.os.Binder;
import android.telephony.CellLocation;
import android.telephony.NeighboringCellInfo;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.SignalStrength;
import android.telephony.CellInfo;
import android.telephony.TelephonyManager;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XTelephonyManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final Map<PhoneStateListener, XPhoneStateListener> mListener = new WeakHashMap<PhoneStateListener, XPhoneStateListener>();

	private XTelephonyManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
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
	// public int getNetworkType()
	// public int getPhoneType()
	// public String getSimCountryIso()
	// public String getSimOperator()
	// public String getSimOperatorName()
	// public static int getPhoneType(int networkMode)
	// public String getSimSerialNumber()
	// public String getSubscriberId()
	// public String getVoiceMailAlphaTag()
	// public String getVoiceMailNumber()
	// public void listen(PhoneStateListener listener, int events)
	// frameworks/base/telephony/java/android/telephony/TelephonyManager.java
	// http://developer.android.com/reference/android/telephony/TelephonyManager.html

	// @formatter:off
	private enum Methods {
		disableLocationUpdates, enableLocationUpdates,
		getAllCellInfo, getCellLocation,
		getDeviceId, getGroupIdLevel1,
		getIsimDomain, getIsimImpi, getIsimImpu,
		getLine1AlphaTag, getLine1Number, getMsisdn,
		getNeighboringCellInfo,
		getNetworkCountryIso, getNetworkOperator, getNetworkOperatorName,
		getNetworkType, getPhoneType,
		getSimCountryIso, getSimOperator, getSimOperatorName, getSimSerialNumber,
		getSubscriberId,
		getVoiceMailAlphaTag, getVoiceMailNumber,
		listen
	};
	// @formatter:on

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();

		List<XHook> listHook = new ArrayList<XHook>();

		listHook.add(new XTelephonyManager(Methods.disableLocationUpdates, PrivacyManager.cLocation, className));
		listHook.add(new XTelephonyManager(Methods.enableLocationUpdates, PrivacyManager.cLocation, className));
		listHook.add(new XTelephonyManager(Methods.getAllCellInfo, PrivacyManager.cLocation, className));
		listHook.add(new XTelephonyManager(Methods.getCellLocation, PrivacyManager.cLocation, className));

		listHook.add(new XTelephonyManager(Methods.getDeviceId, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getGroupIdLevel1, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getIsimDomain, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getIsimImpi, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getIsimImpu, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getLine1AlphaTag, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getLine1Number, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getMsisdn, PrivacyManager.cPhone, className));

		listHook.add(new XTelephonyManager(Methods.getNeighboringCellInfo, PrivacyManager.cLocation, className));

		listHook.add(new XTelephonyManager(Methods.getSimSerialNumber, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getSubscriberId, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getVoiceMailAlphaTag, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getVoiceMailNumber, PrivacyManager.cPhone, className));

		listHook.add(new XTelephonyManager(Methods.listen, PrivacyManager.cLocation, className));

		listHook.add(new XTelephonyManager(Methods.listen, PrivacyManager.cPhone, className));

		// No permissions required
		listHook.add(new XTelephonyManager(Methods.getNetworkCountryIso, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getNetworkOperator, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getNetworkOperatorName, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getNetworkType, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getPhoneType, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getSimCountryIso, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getSimOperator, PrivacyManager.cPhone, className));
		listHook.add(new XTelephonyManager(Methods.getSimOperatorName, PrivacyManager.cPhone, className));

		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.listen) {
			if (param.args.length > 1) {
				PhoneStateListener listener = (PhoneStateListener) param.args[0];
				int event = (Integer) param.args[1];
				if (listener != null)
					if (event == PhoneStateListener.LISTEN_NONE) {
						// Remove
						synchronized (mListener) {
							XPhoneStateListener xlistener = mListener.get(listener);
							if (xlistener != null) {
								param.args[0] = xlistener;
								mListener.remove(listener);
							}
						}
					} else if (isRestricted(param))
						try {
							// Replace
							XPhoneStateListener xListener = new XPhoneStateListener(listener);
							synchronized (mListener) {
								mListener.put(listener, xListener);
								Util.log(this, Log.INFO, "Added count=" + mListener.size());
							}
							param.args[0] = xListener;
						} catch (Throwable ignored) {
							// Some implementations require a looper
							// which is not according to the documentation
							// and stock source code
						}
			}
		} else if (mMethod == Methods.disableLocationUpdates || mMethod == Methods.enableLocationUpdates)
			if (isRestricted(param))
				param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod != Methods.listen && mMethod != Methods.disableLocationUpdates
				&& mMethod != Methods.enableLocationUpdates)
			if (mMethod == Methods.getAllCellInfo) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<CellInfo>());

			} else if (mMethod == Methods.getCellLocation) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(CellLocation.getEmpty());

			} else if (mMethod == Methods.getIsimImpu) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), mMethod.name()));

			} else if (mMethod == Methods.getNeighboringCellInfo) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<NeighboringCellInfo>());

			} else if (mMethod == Methods.getNetworkType) {
				if (isRestricted(param))
					param.setResult(TelephonyManager.NETWORK_TYPE_UNKNOWN);

			} else if (mMethod == Methods.getPhoneType) {
				if (isRestricted(param))
					param.setResult(TelephonyManager.PHONE_TYPE_GSM); // IMEI

			} else {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), mMethod.name()));
			}
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
			mListener.onCallStateChanged(state,
					(String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "PhoneNumber"));
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

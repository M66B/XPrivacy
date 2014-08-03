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
import android.telephony.gsm.GsmCellLocation;
import android.util.Log;

public class XTelephonyManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.telephony.TelephonyManager";
	private static final Map<PhoneStateListener, XPhoneStateListener> mListener = new WeakHashMap<PhoneStateListener, XPhoneStateListener>();

	private enum Srv {
		SubInfo, Registry
	};

	private XTelephonyManager(Methods method, String restrictionName, Srv srv) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
		if (srv == Srv.SubInfo)
			mClassName = "com.android.internal.telephony.PhoneSubInfo";
		else if (srv == Srv.Registry)
			mClassName = "com.android.server.TelephonyRegistry";
		else
			Util.log(null, Log.ERROR, "Unknown srv=" + srv.name());
	}

	private XTelephonyManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	private XTelephonyManager(Methods method, String restrictionName, String className, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

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
	// public static int getPhoneType(int networkMode)
	// public String getSimSerialNumber()
	// public String getSubscriberId()
	// public String getVoiceMailAlphaTag()
	// public String getVoiceMailNumber()
	// public void listen(PhoneStateListener listener, int events)
	// frameworks/base/telephony/java/android/telephony/TelephonyManager.java
	// http://developer.android.com/reference/android/telephony/TelephonyManager.html

	// public java.lang.String getDeviceId()
	// public java.lang.String getDeviceSvn()
	// public java.lang.String getSubscriberId()
	// public java.lang.String getGroupIdLevel1()
	// public java.lang.String getIccSerialNumber()
	// public java.lang.String getLine1Number()
	// public java.lang.String getLine1AlphaTag()
	// public java.lang.String getMsisdn()
	// public java.lang.String getVoiceMailNumber()
	// public java.lang.String getCompleteVoiceMailNumber()
	// public java.lang.String getVoiceMailAlphaTag()
	// public java.lang.String getIsimImpi()
	// public java.lang.String getIsimDomain()
	// public java.lang.String[] getIsimImpu()
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.4_r1/com/android/internal/telephony/PhoneSubInfo.java

	// public void listen(java.lang.String pkg, IPhoneStateListener callback, int events, boolean notifyNow)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.4_r1/com/android/server/TelephonyRegistry.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		disableLocationUpdates, enableLocationUpdates,
		getAllCellInfo, getCellLocation,
		getDeviceId, getGroupIdLevel1,
		getIsimDomain, getIsimImpi, getIsimImpu,
		getLine1AlphaTag, getLine1Number, getMsisdn,
		getNeighboringCellInfo,
		getNetworkCountryIso, getNetworkOperator, getNetworkOperatorName,
		getSimCountryIso, getSimOperator, getSimOperatorName, getSimSerialNumber,
		getSubscriberId,
		getVoiceMailAlphaTag, getVoiceMailNumber,
		listen,

		Srv_getDeviceId, Srv_getGroupIdLevel1,
		Srv_getIccSerialNumber,
		Srv_getIsimDomain, Srv_getIsimImpi, Srv_getIsimImpu,
		Srv_getLine1AlphaTag, Srv_getLine1Number,
		Srv_getMsisdn,
		Srv_getSubscriberId,
		Srv_getCompleteVoiceMailNumber, Srv_getVoiceMailNumber, Srv_getVoiceMailAlphaTag,

		Srv_listen
	};
	// @formatter:on

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			listHook.add(new XTelephonyManager(Methods.disableLocationUpdates, null, className, 10));
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
			listHook.add(new XTelephonyManager(Methods.getSimCountryIso, PrivacyManager.cPhone, className));
			listHook.add(new XTelephonyManager(Methods.getSimOperator, PrivacyManager.cPhone, className));
			listHook.add(new XTelephonyManager(Methods.getSimOperatorName, PrivacyManager.cPhone, className));

			// PhoneSubInfo
			listHook.add(new XTelephonyManager(Methods.Srv_getDeviceId, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getGroupIdLevel1, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getIccSerialNumber, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getIsimDomain, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getIsimImpi, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getIsimImpu, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getLine1AlphaTag, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getLine1Number, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getMsisdn, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getSubscriberId, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getCompleteVoiceMailNumber, PrivacyManager.cPhone,
					Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getVoiceMailAlphaTag, PrivacyManager.cPhone, Srv.SubInfo));
			listHook.add(new XTelephonyManager(Methods.Srv_getVoiceMailNumber, PrivacyManager.cPhone, Srv.SubInfo));

			listHook.add(new XTelephonyManager(Methods.Srv_listen, PrivacyManager.cLocation, Srv.Registry));
			listHook.add(new XTelephonyManager(Methods.Srv_listen, PrivacyManager.cPhone, Srv.Registry));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case disableLocationUpdates:
			if (isRestricted(param, PrivacyManager.cLocation, "enableLocationUpdates"))
				param.setResult(null);
			break;

		case enableLocationUpdates:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case getAllCellInfo:
		case getCellLocation:
		case getDeviceId:
		case getGroupIdLevel1:
		case getIsimDomain:
		case getIsimImpi:
		case getIsimImpu:
		case getLine1AlphaTag:
		case getLine1Number:
		case getMsisdn:
		case getNeighboringCellInfo:
		case getNetworkCountryIso:
		case getNetworkOperator:
		case getNetworkOperatorName:
		case getSimCountryIso:
		case getSimOperator:
		case getSimOperatorName:
		case getSimSerialNumber:
		case getSubscriberId:
		case getVoiceMailAlphaTag:
		case getVoiceMailNumber:
			break;

		case listen:
			if (param.args.length > 1 && param.args[0] instanceof PhoneStateListener
					&& param.args[1] instanceof Integer) {
				PhoneStateListener listener = (PhoneStateListener) param.args[0];
				int event = (Integer) param.args[1];
				if (event == PhoneStateListener.LISTEN_NONE) {
					// Remove
					synchronized (mListener) {
						XPhoneStateListener xListener = mListener.get(listener);
						if (xListener != null) {
							param.args[0] = xListener;
							Util.log(this, Log.WARN,
									"Removed count=" + mListener.size() + " uid=" + Binder.getCallingUid());
						}
					}
				} else if (isRestricted(param))
					try {
						// Replace
						XPhoneStateListener xListener;
						synchronized (mListener) {
							xListener = mListener.get(listener);
							if (xListener == null) {
								xListener = new XPhoneStateListener(listener);
								mListener.put(listener, xListener);
								Util.log(this, Log.WARN,
										"Added count=" + mListener.size() + " uid=" + Binder.getCallingUid());
							}
						}
						param.args[0] = xListener;
					} catch (Throwable ignored) {
						// Some implementations require a looper
						// which is not according to the documentation
						// and stock source code
					}
			}
			break;

		case Srv_listen:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case Srv_getDeviceId:
		case Srv_getGroupIdLevel1:
		case Srv_getIccSerialNumber:
		case Srv_getIsimDomain:
		case Srv_getIsimImpi:
		case Srv_getIsimImpu:
		case Srv_getLine1AlphaTag:
		case Srv_getLine1Number:
		case Srv_getMsisdn:
		case Srv_getSubscriberId:
		case Srv_getCompleteVoiceMailNumber:
		case Srv_getVoiceMailNumber:
		case Srv_getVoiceMailAlphaTag:
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		int uid = Binder.getCallingUid();

		switch (mMethod) {
		case disableLocationUpdates:
		case enableLocationUpdates:
			break;

		case getAllCellInfo:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList<CellInfo>());
			break;

		case getCellLocation:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(getDefacedCellLocation(uid));
			break;

		case getNeighboringCellInfo:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<NeighboringCellInfo>());
			break;

		case listen:
		case Srv_listen:
			break;

		case getDeviceId:
		case getGroupIdLevel1:
		case getIsimDomain:
		case getIsimImpi:
		case getIsimImpu:
		case getLine1AlphaTag:
		case getLine1Number:
		case getMsisdn:
		case getNetworkCountryIso:
		case getNetworkOperator:
		case getNetworkOperatorName:
		case getSimCountryIso:
		case getSimOperator:
		case getSimOperatorName:
		case getSimSerialNumber:
		case getSubscriberId:
		case getVoiceMailAlphaTag:
		case getVoiceMailNumber:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp(uid, mMethod.name()));
			break;

		case Srv_getDeviceId:
		case Srv_getGroupIdLevel1:
		case Srv_getIccSerialNumber:
		case Srv_getIsimDomain:
		case Srv_getIsimImpi:
		case Srv_getIsimImpu:
		case Srv_getLine1AlphaTag:
		case Srv_getLine1Number:
		case Srv_getMsisdn:
		case Srv_getSubscriberId:
		case Srv_getCompleteVoiceMailNumber:
		case Srv_getVoiceMailNumber:
		case Srv_getVoiceMailAlphaTag:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp(uid, mMethod.name().replace("Srv_", "")));
			break;
		}
	}

	private static CellLocation getDefacedCellLocation(int uid) {
		int cid = (Integer) PrivacyManager.getDefacedProp(uid, "CID");
		int lac = (Integer) PrivacyManager.getDefacedProp(uid, "LAC");
		if (cid > 0 && lac > 0) {
			GsmCellLocation cellLocation = new GsmCellLocation();
			cellLocation.setLacAndCid(lac, cid);
			return cellLocation;
		} else
			return CellLocation.getEmpty();
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
			mListener.onCellLocationChanged(getDefacedCellLocation(Binder.getCallingUid()));
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

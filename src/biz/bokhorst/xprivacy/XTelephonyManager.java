package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.content.Context;
import android.os.Binder;
import android.os.Build;
import android.telephony.CellLocation;
import android.telephony.NeighboringCellInfo;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.SignalStrength;
import android.telephony.CellInfo;
import android.telephony.TelephonyManager;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XTelephonyManager extends XHook {
	private Methods mMethod;
	private static final Map<PhoneStateListener, XPhoneStateListener> mListener = new WeakHashMap<PhoneStateListener, XPhoneStateListener>();

	private XTelephonyManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	private XTelephonyManager(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "android.telephony.TelephonyManager";
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
	// public static int getPhoneType(int networkMode)
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

	private enum Methods {
		disableLocationUpdates, enableLocationUpdates, getAllCellInfo, getCellLocation, getDeviceId, getGroupIdLevel1, getIsimDomain, getIsimImpi, getIsimImpu, getLine1AlphaTag, getLine1Number, getMsisdn, getNeighboringCellInfo, getNetworkCountryIso, getNetworkOperator, getNetworkOperatorName, getNetworkType, getPhoneType, getSimCountryIso, getSimOperator, getSimOperatorName, getSimSerialNumber, getSubscriberId, getVoiceMailAlphaTag, getVoiceMailNumber, listen
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XTelephonyManager(Methods.enableLocationUpdates, PrivacyManager.cLocation));
		listHook.add(new XTelephonyManager(Methods.getCellLocation, PrivacyManager.cLocation));
		listHook.add(new XTelephonyManager(Methods.getNeighboringCellInfo, PrivacyManager.cLocation));

		listHook.add(new XTelephonyManager(Methods.getDeviceId, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getIsimDomain, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getIsimImpi, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getIsimImpu, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getLine1AlphaTag, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getLine1Number, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getMsisdn, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getSimSerialNumber, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getSubscriberId, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getVoiceMailAlphaTag, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getVoiceMailNumber, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.listen, PrivacyManager.cPhone));

		// No permissions required
		listHook.add(new XTelephonyManager(Methods.getNetworkCountryIso, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getNetworkOperator, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getNetworkOperatorName, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getNetworkType, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getPhoneType, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getSimCountryIso, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getSimOperator, PrivacyManager.cPhone));
		listHook.add(new XTelephonyManager(Methods.getSimOperatorName, PrivacyManager.cPhone));

		listHook.add(new XTelephonyManager(Methods.getAllCellInfo, PrivacyManager.cLocation,
				Build.VERSION_CODES.JELLY_BEAN_MR1));
		listHook.add(new XTelephonyManager(Methods.getGroupIdLevel1, PrivacyManager.cPhone,
				Build.VERSION_CODES.JELLY_BEAN_MR2));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.listen) {
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
					param.setResult(PrivacyManager.getDefacedProp(mMethod.name()));
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
					param.setResult(PrivacyManager.getDefacedProp(mMethod.name()));
			}
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;

		// No context for static methods
		if (param.thisObject != null) {
			// TelephonyManager
			boolean found = false;
			try {
				Field fieldContext = findField(param.thisObject.getClass(), "sContext");
				context = (Context) fieldContext.get(param.thisObject);
				found = true;
			} catch (NoSuchFieldError ex) {
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}

			// MultiSimTelephonyManager
			if (!found)
				try {
					Field fieldContext = findField(param.thisObject.getClass(), "mContext");
					context = (Context) fieldContext.get(param.thisObject);
					found = true;
				} catch (NoSuchFieldError ex) {
				} catch (Throwable ex) {
					Util.bug(this, ex);
				}

			// Duos
			if (!found)
				try {
					Field fieldContext = findField(param.thisObject.getClass(), "sContextDuos");
					context = (Context) fieldContext.get(param.thisObject);
					found = true;
				} catch (Throwable ex) {
					Util.bug(this, ex);
				}
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

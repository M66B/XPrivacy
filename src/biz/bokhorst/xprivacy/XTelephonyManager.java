package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.telephony.CellLocation;
import android.telephony.PhoneStateListener;
import android.telephony.ServiceState;
import android.telephony.SignalStrength;
import android.telephony.CellInfo;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XTelephonyManager extends XHook {

	public XTelephonyManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// public String getDeviceId()
	// public String getLine1Number()
	// public String getMsisdn()
	// public String getSimSerialNumber()
	// public String getSubscriberId()
	// public void listen(PhoneStateListener listener, int events)
	// frameworks/base/telephony/java/android/telephony/TelephonyManager.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("listen") || param.method.getName().equals("_listen")) {
			PhoneStateListener listener = (PhoneStateListener) param.args[0];
			if (listener != null)
				if (isRestricted(param)) {
					XUtil.log(this, Log.INFO, "Replacing listener uid=" + Binder.getCallingUid());
					param.args[0] = new XPhoneStateListener(listener);
				}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (!param.method.getName().equals("listen") && !param.method.getName().equals("_listen"))
			if (param.getResultOrThrowable() != null)
				if (isRestricted(param)) {
					XUtil.log(this, Log.INFO, this.getMethodName() + " uid=" + Binder.getCallingUid());
					if (param.method.getName().equals("getIsimImpu"))
						param.setResult(new String[] { XRestriction.cDefaceString });
					else
						param.setResult(XRestriction.cDefaceString);
				}
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "sContext");
		Context context = (Context) fieldContext.get(param.thisObject);
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
			try {
				XUtil.log(XTelephonyManager.this, Log.INFO, mListener.getClass().getPackage().getName()
						+ ": onCallStateChanged");
			} catch (Throwable ex) {
			}
			mListener.onCallStateChanged(state, XRestriction.cDefaceString);
		}

		@Override
		public void onCellInfoChanged(List<CellInfo> cellInfo) {
			mListener.onCellInfoChanged(cellInfo);
		}

		@Override
		public void onCellLocationChanged(CellLocation location) {
			mListener.onCellLocationChanged(location);
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

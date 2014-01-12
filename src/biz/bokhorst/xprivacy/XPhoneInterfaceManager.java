package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.os.Build;
import android.os.Bundle;
import android.telephony.NeighboringCellInfo;
import android.telephony.CellInfo;
import android.telephony.TelephonyManager;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

import static de.robv.android.xposed.XposedHelpers.findField;

public class XPhoneInterfaceManager extends XHook {
	private Methods mMethod;

	private XPhoneInterfaceManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()));
		mMethod = method;
	}

	private XPhoneInterfaceManager(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()), sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.phone.PhoneInterfaceManager";
	}

	// @formatter:off

	// public void disableLocationUpdates()
	// public void enableLocationUpdates()
	// public List<CellInfo> getAllCellInfo()
	// public Bundle getCellLocation()
	// public List<NeighboringCellInfo> getNeighboringCellInfo(String callingPackage)
	// public int getDataNetworkType()
	// public int getActivePhoneType()

	// TODO:
	// * IPhoneSubInfo public String getDeviceId()
	// * IPhoneSubInfo public String getGroupIdLevel1()
	// * IPhoneSubInfo public public String getIsimDomain()
	// * IPhoneSubInfo public public String getIsimImpi()
	// * IPhoneSubInfo public public String[] getIsimImpu()
	// * IPhoneSubInfo public public String getLine1AlphaTag()
	// * IPhoneSubInfo public public String getLine1Number()
	// * IPhoneSubInfo public public String getMsisdn()
	// * TelephonyProperties public String getNetworkCountryIso()
	// * TelephonyProperties public String getNetworkOperator()
	// * TelephonyProperties public String getNetworkOperatorName()
	// * TelephonyProperties public static int getPhoneType(int networkMode)
	// * TelephonyProperties public String getSimCountryIso()
	// * TelephonyProperties public String getSimOperator()
	// * TelephonyProperties public String getSimOperatorName()
	// * IPhoneSubInfo public String getSimSerialNumber()
	// * IPhoneSubInfo public String getSubscriberId()
	// * IPhoneSubInfo public String getVoiceMailAlphaTag()
	// * IPhoneSubInfo public String getVoiceMailNumber()
	// * ITelephonyRegistry public void listen(PhoneStateListener listener, int events)
	
	// platform/packages/services/Telephony/src/com/android/phone/PhoneInterfaceManager.java
	// http://developer.android.com/reference/android/telephony/TelephonyManager.html

	// @formatter:on

	// @formatter:off
	private enum Methods {
		disableLocationUpdates, enableLocationUpdates,
		getAllCellInfo, getCellLocation, getNeighboringCellInfo,
		getDataNetworkType, getActivePhoneType
	};
	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();

		listHook.add(new XPhoneInterfaceManager(Methods.disableLocationUpdates, PrivacyManager.cLocation));
		listHook.add(new XPhoneInterfaceManager(Methods.enableLocationUpdates, PrivacyManager.cLocation));
		listHook.add(new XPhoneInterfaceManager(Methods.getAllCellInfo, PrivacyManager.cLocation,
				Build.VERSION_CODES.JELLY_BEAN_MR1));
		listHook.add(new XPhoneInterfaceManager(Methods.getCellLocation, PrivacyManager.cLocation));
		listHook.add(new XPhoneInterfaceManager(Methods.getNeighboringCellInfo, PrivacyManager.cLocation));

		// No permissions required
		listHook.add(new XPhoneInterfaceManager(Methods.getDataNetworkType, PrivacyManager.cPhone));
		listHook.add(new XPhoneInterfaceManager(Methods.getActivePhoneType, PrivacyManager.cPhone));

		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.disableLocationUpdates || mMethod == Methods.enableLocationUpdates)
			if (isRestricted(param))
				param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod != Methods.disableLocationUpdates && mMethod != Methods.enableLocationUpdates)
			if (mMethod == Methods.getAllCellInfo) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<CellInfo>());
			} else if (mMethod == Methods.getCellLocation) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new Bundle());
			} else if (mMethod == Methods.getNeighboringCellInfo) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<NeighboringCellInfo>());
			} else if (mMethod == Methods.getDataNetworkType) {
				if (isRestricted(param))
					param.setResult(TelephonyManager.NETWORK_TYPE_UNKNOWN);
			} else if (mMethod == Methods.getActivePhoneType) {
				if (isRestricted(param))
					param.setResult(TelephonyManager.PHONE_TYPE_GSM); // IMEI
			} else
				Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;

		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mApp");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (NoSuchFieldError ex) {
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}

		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

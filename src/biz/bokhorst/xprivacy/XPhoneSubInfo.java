package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

import static de.robv.android.xposed.XposedHelpers.findField;

public class XPhoneSubInfo extends XHook {
	private Methods mMethod;

	private XPhoneSubInfo(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()));
		mMethod = method;
	}

	private XPhoneSubInfo(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()), sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.internal.telephony.PhoneSubInfo";
	}

	// @formatter:off

	// public String getDeviceId()
	// public String getGroupIdLevel1()
	// public String getIccSerialNumber()
	// public String getIsimDomain()
	// public String getIsimImpi()
	// public String[] getIsimImpu()
	// public String getLine1AlphaTag()
	// public String getLine1Number()
	// public String getMsisdn()
	// public String getSubscriberId()
	// public String getVoiceMailAlphaTag()
	// public String getVoiceMailNumber()
	
	// telephony/src/java/com/android/internal/telephony/PhoneSubInfo.java
	// http://developer.android.com/reference/android/telephony/TelephonyManager.html

	// @formatter:on

	// @formatter:off
	private enum Methods {
		getDeviceId,
		getGroupIdLevel1,
		getIccSerialNumber,
		getIsimDomain, getIsimImpi, getIsimImpu,
		getLine1AlphaTag, getLine1Number,
		getMsisdn,
		getSubscriberId,
		getVoiceMailAlphaTag, getVoiceMailNumber
	};
	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();

		listHook.add(new XPhoneSubInfo(Methods.getDeviceId, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getGroupIdLevel1, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getIccSerialNumber, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getIsimDomain, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getIsimImpi, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getIsimImpu, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getLine1AlphaTag, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getLine1Number, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getMsisdn, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getSubscriberId, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getVoiceMailAlphaTag, PrivacyManager.cPhone));
		listHook.add(new XPhoneSubInfo(Methods.getVoiceMailNumber, PrivacyManager.cPhone));

		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResult() != null && isRestricted(param))
			param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), mMethod.name()));
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
}

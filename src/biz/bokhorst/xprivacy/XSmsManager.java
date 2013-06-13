package biz.bokhorst.xprivacy;

import java.util.ArrayList;

import android.telephony.SmsMessage;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XSmsManager extends XHook {

	public XSmsManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// public static ArrayList<SmsMessage> getAllMessagesFromIcc()
	// frameworks/base/telephony/java/android/telephony/SmsManager.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (!methodName.equals("getAllMessagesFromIcc"))
			if (isRestricted(param))
				param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResult() != null)
			if (isRestricted(param))
				param.setResult(new ArrayList<SmsMessage>());
	}
}

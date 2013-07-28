package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.telephony.SmsMessage;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XSmsManager extends XHook {

	private XSmsManager(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "android.telephony.SmsManager";
	}

	// @formatter:off

	// public static ArrayList<SmsMessage> getAllMessagesFromIcc()
	// public void sendDataMessage(String destinationAddress, String scAddress, short destinationPort, byte[] data, PendingIntent sentIntent, PendingIntent deliveryIntent)
	// public void sendMultipartTextMessage(String destinationAddress, String scAddress, ArrayList<String> parts, ArrayList<PendingIntent> sentIntents, ArrayList<PendingIntent> deliveryIntents)
	// public void sendTextMessage(String destinationAddress, String scAddress, String text, PendingIntent sentIntent, PendingIntent deliveryIntent)
	// frameworks/base/telephony/java/android/telephony/SmsManager.java
	// http://developer.android.com/reference/android/telephony/gsm/SmsManager.html

	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XSmsManager("getAllMessagesFromIcc", PrivacyManager.cMessages));
		String[] smses = new String[] { "sendDataMessage", "sendMultipartTextMessage", "sendTextMessage" };
		for (String sms : smses)
			listHook.add(new XSmsManager(sms, PrivacyManager.cCalling));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("sendDataMessage") || methodName.equals("sendMultipartTextMessage")
				|| methodName.equals("sendTextMessage"))
			if (isRestricted(param))
				param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (!methodName.equals("sendDataMessage") && !methodName.equals("sendMultipartTextMessage")
				&& !methodName.equals("sendTextMessage"))
			if (methodName.equals("getAllMessagesFromIcc")) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<SmsMessage>());
			} else
				Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}
}

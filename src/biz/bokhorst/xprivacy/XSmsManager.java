package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.telephony.SmsMessage;
import android.util.Log;

public class XSmsManager extends XHook {
	private Methods mMethod;

	private XSmsManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
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
	// http://developer.android.com/reference/android/telephony/SmsManager.html

	// @formatter:on

	private enum Methods {
		getAllMessagesFromIcc, sendDataMessage, sendMultipartTextMessage, sendTextMessage
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XSmsManager(Methods.getAllMessagesFromIcc, PrivacyManager.cMessages));
		listHook.add(new XSmsManager(Methods.sendDataMessage, PrivacyManager.cCalling));
		listHook.add(new XSmsManager(Methods.sendMultipartTextMessage, PrivacyManager.cCalling));
		listHook.add(new XSmsManager(Methods.sendTextMessage, PrivacyManager.cCalling));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.sendDataMessage || mMethod == Methods.sendMultipartTextMessage
				|| mMethod == Methods.sendTextMessage)
			if (param.args.length > 0 && param.args[0] instanceof String)
				if (isRestrictedExtra(param, (String) param.args[0]))
					param.setResult(null);
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod != Methods.sendDataMessage && mMethod != Methods.sendMultipartTextMessage
				&& mMethod != Methods.sendTextMessage)
			if (mMethod == Methods.getAllMessagesFromIcc) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<SmsMessage>());
			} else
				Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

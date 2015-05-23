package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.telephony.SmsMessage;

public class XSmsManager extends XHook {
	private Methods mMethod;

	private XSmsManager(Methods method, String restrictionName) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
	}

	public String getClassName() {
		if (mMethod.name().startsWith("Srv_"))
			return "com.android.internal.telephony.IccSmsInterfaceManager";
		else
			return "android.telephony.SmsManager";
	}

	// @formatter:off

	// public static ArrayList<SmsMessage> getAllMessagesFromIcc()
	// public Bundle getCarrierConfigValues()
	// public void sendDataMessage(String destinationAddress, String scAddress, short destinationPort, byte[] data, PendingIntent sentIntent, PendingIntent deliveryIntent)
	// public void sendMultipartTextMessage(String destinationAddress, String scAddress, ArrayList<String> parts, ArrayList<PendingIntent> sentIntents, ArrayList<PendingIntent> deliveryIntents)
	// public void sendMultimediaMessage(Context context, Uri contentUri, String locationUrl, Bundle configOverrides, PendingIntent sentIntent)
	// public void sendTextMessage(String destinationAddress, String scAddress, String text, PendingIntent sentIntent, PendingIntent deliveryIntent)
	// frameworks/base/telephony/java/android/telephony/SmsManager.java
	// http://developer.android.com/reference/android/telephony/SmsManager.html

	// public List<SmsRawData> getAllMessagesFromIccEf(String callingPackage)
	// public void sendData(java.lang.String callingPkg, java.lang.String destAddr, java.lang.String scAddr, int destPort, byte[] data, android.app.PendingIntent sentIntent, android.app.PendingIntent deliveryIntent)
	// public void sendMultipartText(java.lang.String callingPkg, java.lang.String destinationAddress, java.lang.String scAddress, java.util.List<java.lang.String> parts, java.util.List<android.app.PendingIntent> sentIntents, java.util.List<android.app.PendingIntent> deliveryIntents)
	// public void sendText(java.lang.String callingPkg, java.lang.String destAddr, java.lang.String scAddr, java.lang.String text, android.app.PendingIntent sentIntent, android.app.PendingIntent deliveryIntent)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/internal/telephony/IccSmsInterfaceManager.java/

	// @formatter:on

	// @formatter:off
	private enum Methods {
		getAllMessagesFromIcc, getCarrierConfigValues, sendDataMessage, sendMultipartTextMessage, sendMultimediaMessage, sendTextMessage,
		Srv_getAllMessagesFromIccEf, Srv_sendData, Srv_sendMultipartText, Srv_sendText
	};
	// @formatter:on

	public static List<XHook> getInstances(boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (server) {
			listHook.add(new XSmsManager(Methods.Srv_getAllMessagesFromIccEf, PrivacyManager.cMessages));
			listHook.add(new XSmsManager(Methods.Srv_sendData, PrivacyManager.cCalling));
			listHook.add(new XSmsManager(Methods.Srv_sendMultipartText, PrivacyManager.cCalling));
			listHook.add(new XSmsManager(Methods.Srv_sendText, PrivacyManager.cCalling));
		} else {
			listHook.add(new XSmsManager(Methods.getAllMessagesFromIcc, PrivacyManager.cMessages));
			listHook.add(new XSmsManager(Methods.getCarrierConfigValues, PrivacyManager.cMessages));
			listHook.add(new XSmsManager(Methods.sendDataMessage, PrivacyManager.cCalling));
			listHook.add(new XSmsManager(Methods.sendMultimediaMessage, PrivacyManager.cCalling));
			listHook.add(new XSmsManager(Methods.sendMultipartTextMessage, PrivacyManager.cCalling));
			listHook.add(new XSmsManager(Methods.sendTextMessage, PrivacyManager.cCalling));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case getAllMessagesFromIcc:
		case getCarrierConfigValues:
			// Do nothing
			break;

		case sendDataMessage:
		case sendMultipartTextMessage:
		case sendTextMessage:
			if (param.args.length > 0 && param.args[0] instanceof String)
				if (isRestrictedExtra(param, (String) param.args[0]))
					param.setResult(null);
			break;

		case sendMultimediaMessage:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case Srv_getAllMessagesFromIccEf:
			// Do nothing
			break;

		case Srv_sendData:
		case Srv_sendText:
		case Srv_sendMultipartText:
			if (param.args.length > 1 && (param.args[1] == null || param.args[1] instanceof String))
				if (isRestrictedExtra(param, (String) param.args[1]))
					param.setResult(null);
			break;
		}
	}

	@Override
	@SuppressWarnings("rawtypes")
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getAllMessagesFromIcc:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<SmsMessage>());
			break;

		case getCarrierConfigValues:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new Bundle());
			break;

		case sendDataMessage:
		case sendMultimediaMessage:
		case sendMultipartTextMessage:
		case sendTextMessage:
			// Do nothing
			break;

		case Srv_getAllMessagesFromIccEf:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList());
			break;

		case Srv_sendData:
		case Srv_sendText:
		case Srv_sendMultipartText:
			// Do nothing
			break;
		}
	}
}

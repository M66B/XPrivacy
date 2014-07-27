package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import biz.bokhorst.xprivacy.XHook;

public class XIccSmsInterfaceManager extends XHook {
	private Methods mMethod;

	private XIccSmsInterfaceManager(Methods method, String restrictionName) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.internal.telephony.IccSmsInterfaceManager";
	}

	// @formatter:off
	// public List<SmsRawData> getAllMessagesFromIccEf(String callingPackage)
	// public void sendData(java.lang.String callingPkg, java.lang.String destAddr, java.lang.String scAddr, int destPort, byte[] data, android.app.PendingIntent sentIntent, android.app.PendingIntent deliveryIntent)
	// public void sendMultipartText(java.lang.String callingPkg, java.lang.String destinationAddress, java.lang.String scAddress, java.util.List<java.lang.String> parts, java.util.List<android.app.PendingIntent> sentIntents, java.util.List<android.app.PendingIntent> deliveryIntents)
	// public void sendText(java.lang.String callingPkg, java.lang.String destAddr, java.lang.String scAddr, java.lang.String text, android.app.PendingIntent sentIntent, android.app.PendingIntent deliveryIntent)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/internal/telephony/IccSmsInterfaceManager.java/
	// @formatter:on

	private enum Methods {
		Srv_getAllMessagesFromIccEf, Srv_sendData, Srv_sendMultipartText, Srv_sendText
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XIccSmsInterfaceManager(Methods.Srv_getAllMessagesFromIccEf, PrivacyManager.cMessages));
		listHook.add(new XIccSmsInterfaceManager(Methods.Srv_sendData, PrivacyManager.cCalling));
		listHook.add(new XIccSmsInterfaceManager(Methods.Srv_sendMultipartText, PrivacyManager.cCalling));
		listHook.add(new XIccSmsInterfaceManager(Methods.Srv_sendText, PrivacyManager.cCalling));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case Srv_getAllMessagesFromIccEf:
			// Do nothing
			break;

		case Srv_sendData:
		case Srv_sendText:
		case Srv_sendMultipartText:
			if (param.args.length > 1 && param.args[1] instanceof String)
				if (isRestrictedExtra(param, (String) param.args[1]))
					param.setResult(null);
			break;
		}
	}

	@Override
	@SuppressWarnings("rawtypes")
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
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

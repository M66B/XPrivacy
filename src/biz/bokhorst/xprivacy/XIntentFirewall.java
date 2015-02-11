package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.nfc.NfcAdapter;
import android.provider.Telephony;
import android.service.notification.NotificationListenerService;
import android.telephony.TelephonyManager;

@SuppressLint("InlinedApi")
public class XIntentFirewall extends XHook {
	private Methods mMethod;
	private static Map<String, String> mapIntentRestriction = new HashMap<String, String>();

	static {
		// Intent receive: calling
		mapIntentRestriction.put(Intent.ACTION_NEW_OUTGOING_CALL, PrivacyManager.cCalling);
		mapIntentRestriction.put(TelephonyManager.ACTION_PHONE_STATE_CHANGED, PrivacyManager.cPhone);
		mapIntentRestriction.put(TelephonyManager.ACTION_RESPOND_VIA_MESSAGE, PrivacyManager.cCalling);

		// Intent receive: C2DM
		mapIntentRestriction.put("com.google.android.c2dm.intent.REGISTRATION", PrivacyManager.cNotifications);
		mapIntentRestriction.put("com.google.android.c2dm.intent.RECEIVE", PrivacyManager.cNotifications);

		// Intent receive: NFC
		mapIntentRestriction.put(NfcAdapter.ACTION_ADAPTER_STATE_CHANGED, PrivacyManager.cNfc);
		mapIntentRestriction.put(NfcAdapter.ACTION_NDEF_DISCOVERED, PrivacyManager.cNfc);
		mapIntentRestriction.put(NfcAdapter.ACTION_TAG_DISCOVERED, PrivacyManager.cNfc);
		mapIntentRestriction.put(NfcAdapter.ACTION_TECH_DISCOVERED, PrivacyManager.cNfc);

		// Intent receive: SMS
		mapIntentRestriction.put(Telephony.Sms.Intents.DATA_SMS_RECEIVED_ACTION, PrivacyManager.cMessages);
		mapIntentRestriction.put(Telephony.Sms.Intents.SMS_RECEIVED_ACTION, PrivacyManager.cMessages);
		mapIntentRestriction.put(Telephony.Sms.Intents.WAP_PUSH_RECEIVED_ACTION, PrivacyManager.cMessages);
		mapIntentRestriction.put(Telephony.Sms.Intents.SMS_DELIVER_ACTION, PrivacyManager.cMessages);
		mapIntentRestriction.put(Telephony.Sms.Intents.WAP_PUSH_DELIVER_ACTION, PrivacyManager.cMessages);

		// Intent receive: notifications
		mapIntentRestriction.put(NotificationListenerService.SERVICE_INTERFACE, PrivacyManager.cNotifications);

		// Intent receive: package changes
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_ADDED, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_REPLACED, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_RESTARTED, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_REMOVED, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_CHANGED, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_DATA_CLEARED, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_FIRST_LAUNCH, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_FULLY_REMOVED, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_NEEDS_VERIFICATION, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_PACKAGE_VERIFIED, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_EXTERNAL_APPLICATIONS_AVAILABLE, PrivacyManager.cSystem);
		mapIntentRestriction.put(Intent.ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE, PrivacyManager.cSystem);
	}

	private XIntentFirewall(Methods method) {
		super(null, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.server.firewall.IntentFirewall";
	}

	// @formatter:off

	// public boolean checkIntent(FirewallIntentResolver resolver, ComponentName resolvedComponent, int intentType, Intent intent, int callerUid, int callerPid, String resolvedType, int receivingUid)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.2_r1/com/android/server/firewall/IntentFirewall.java

	// @formatter:on

	private enum Methods {
		checkIntent
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XIntentFirewall(Methods.checkIntent));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case checkIntent:
			if (param.args.length > 7 && param.args[3] instanceof Intent && param.args[7] instanceof Integer) {
				Intent intent = (Intent) param.args[3];
				int receivingUid = (Integer) param.args[7];
				if (isIntentRestricted(receivingUid, intent))
					param.setResult(false);
			}
			break;
		}
	}

	private boolean isIntentRestricted(int uid, Intent intent) throws Throwable {
		String action = intent.getAction();
		String data = intent.getDataString();
		String actionData = (action == null ? "" : action) + (data == null ? "" : ":" + data);

		if (PrivacyManager.getSettingBool(0, PrivacyManager.cSettingIntentWall, false))
			if (isRestrictedExtra(uid, "system", "IntentFirewall", actionData))
				return true;

		if (mapIntentRestriction.containsKey(action)) {
			// Get restriction category
			String restrictionName = mapIntentRestriction.get(action);

			if (Intent.ACTION_NEW_OUTGOING_CALL.equals(action)) {
				// Outgoing call
				if (intent.hasExtra(Intent.EXTRA_PHONE_NUMBER)) {
					String phoneNumber = intent.getStringExtra(Intent.EXTRA_PHONE_NUMBER);
					if (phoneNumber != null)
						if (isRestrictedExtraValue(uid, restrictionName, action, phoneNumber, phoneNumber))
							return true;
				}

			} else if (TelephonyManager.ACTION_PHONE_STATE_CHANGED.equals(action)) {
				// Incoming call
				if (intent.hasExtra(TelephonyManager.EXTRA_INCOMING_NUMBER)) {
					String phoneNumber = intent.getStringExtra(TelephonyManager.EXTRA_INCOMING_NUMBER);
					if (phoneNumber != null)
						if (isRestrictedExtraValue(uid, restrictionName, action, phoneNumber, phoneNumber))
							return true;
				}

			} else if (PrivacyManager.cSystem.equals(restrictionName)) {
				// Package event
				if (isRestrictedExtra(uid, restrictionName, action, intent.getDataString())) {
					String[] packageNames;
					if (action.equals(Intent.ACTION_EXTERNAL_APPLICATIONS_AVAILABLE)
							|| action.equals(Intent.ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE))
						packageNames = intent.getStringArrayExtra(Intent.EXTRA_CHANGED_PACKAGE_LIST);
					else
						packageNames = new String[] { intent.getData().getSchemeSpecificPart() };
					for (String packageName : packageNames)
						if (!XPackageManager.isPackageAllowed(0, packageName))
							return true;
				}

			} else if (isRestrictedExtra(uid, restrictionName, action, intent.getDataString()))
				return true;

		}

		return false;
	}
}

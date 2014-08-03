package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.res.Configuration;
import android.nfc.NfcAdapter;
import android.os.Binder;
import android.os.Message;
import android.provider.Telephony;
import android.service.notification.NotificationListenerService;
import android.telephony.TelephonyManager;

@SuppressLint("InlinedApi")
public class XActivityThread extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static Map<String, String> mapActionRestriction = new HashMap<String, String>();

	static {
		// Intent receive: calling
		mapActionRestriction.put(Intent.ACTION_NEW_OUTGOING_CALL, PrivacyManager.cCalling);
		mapActionRestriction.put(TelephonyManager.ACTION_PHONE_STATE_CHANGED, PrivacyManager.cPhone);
		mapActionRestriction.put(TelephonyManager.ACTION_RESPOND_VIA_MESSAGE, PrivacyManager.cCalling);

		// Intent receive: C2DM
		mapActionRestriction.put("com.google.android.c2dm.intent.REGISTRATION", PrivacyManager.cNotifications);
		mapActionRestriction.put("com.google.android.c2dm.intent.RECEIVE", PrivacyManager.cNotifications);

		// Intent receive: NFC
		mapActionRestriction.put(NfcAdapter.ACTION_ADAPTER_STATE_CHANGED, PrivacyManager.cNfc);
		mapActionRestriction.put(NfcAdapter.ACTION_NDEF_DISCOVERED, PrivacyManager.cNfc);
		mapActionRestriction.put(NfcAdapter.ACTION_TAG_DISCOVERED, PrivacyManager.cNfc);
		mapActionRestriction.put(NfcAdapter.ACTION_TECH_DISCOVERED, PrivacyManager.cNfc);

		// Intent receive: SMS
		mapActionRestriction.put(Telephony.Sms.Intents.DATA_SMS_RECEIVED_ACTION, PrivacyManager.cMessages);
		mapActionRestriction.put(Telephony.Sms.Intents.SMS_RECEIVED_ACTION, PrivacyManager.cMessages);
		mapActionRestriction.put(Telephony.Sms.Intents.WAP_PUSH_RECEIVED_ACTION, PrivacyManager.cMessages);
		mapActionRestriction.put(Telephony.Sms.Intents.SMS_DELIVER_ACTION, PrivacyManager.cMessages);
		mapActionRestriction.put(Telephony.Sms.Intents.WAP_PUSH_DELIVER_ACTION, PrivacyManager.cMessages);

		// Intent receive: notifications
		mapActionRestriction.put(NotificationListenerService.SERVICE_INTERFACE, PrivacyManager.cNotifications);

		// Intent receive: package changes
		mapActionRestriction.put(Intent.ACTION_PACKAGE_ADDED, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_REPLACED, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_RESTARTED, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_REMOVED, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_CHANGED, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_DATA_CLEARED, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_FIRST_LAUNCH, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_FULLY_REMOVED, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_NEEDS_VERIFICATION, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_PACKAGE_VERIFIED, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_EXTERNAL_APPLICATIONS_AVAILABLE, PrivacyManager.cSystem);
		mapActionRestriction.put(Intent.ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE, PrivacyManager.cSystem);
	}

	private XActivityThread(Methods method, int sdk, String className) {
		super(null, method.name(), null, sdk);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	@Override
	public boolean isVisible() {
		return (mMethod == Methods.bindApplication);
	}

	private enum Methods {
		bindApplication, next, handleReceiver
	};

	// @formatter:off

	// public final void bindApplication(String processName, ApplicationInfo
	// 		appInfo, List<ProviderInfo> providers, ComponentName instrumentationName,
	// 		String profileFile, ParcelFileDescriptor profileFd, boolean
	// 		autoStopProfiler, Bundle instrumentationArgs, IInstrumentationWatcher
	// 		instrumentationWatcher, IUiAutomationConnection
	// 		instrumentationUiConnection, int debugMode, boolean enableOpenGlTrace,
	// 		boolean isRestrictedBackupMode, boolean persistent, Configuration config,
	// 		CompatibilityInfo compatInfo, Map<String, IBinder> services, Bundle
	// 		coreSettings)
	// private void handleReceiver(ReceiverData data)
	// http://grepcode.com/file_/repository.grepcode.com/java/ext/com.google.android/android/4.4.4_r1/android/app/ActivityThread.java

	// final Message next()
	// frameworks/base/core/java/android/android/os/MessageQueue.java

	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();

		if (Hook.isAOSP(19))
			listHook.add(new XActivityThread(Methods.bindApplication, 1, "android.app.ActivityThread$ApplicationThread"));
		else {
			listHook.add(new XActivityThread(Methods.next, 1, "android.os.MessageQueue"));
			listHook.add(new XActivityThread(Methods.handleReceiver, 1, "android.app.ActivityThread"));
		}

		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case bindApplication:
			if (param.args.length > 1 && param.args[1] instanceof ApplicationInfo) {
				ApplicationInfo appInfo = (ApplicationInfo) param.args[1];
				for (int i = 0; i < param.args.length; i++)
					if (param.args[i] instanceof Configuration) {
						boolean restricted = false;
						Configuration config = new Configuration((Configuration) param.args[i]);

						if (getRestricted(appInfo.uid, PrivacyManager.cPhone, "Configuration.MCC")) {
							restricted = true;
							try {
								config.mcc = Integer.parseInt((String) PrivacyManager
										.getDefacedProp(appInfo.uid, "MCC"));
							} catch (Throwable ignored) {
								config.mcc = 1;
							}
						}

						if (getRestricted(appInfo.uid, PrivacyManager.cPhone, "Configuration.MNC")) {
							restricted = true;
							try {
								config.mnc = Integer.parseInt((String) PrivacyManager
										.getDefacedProp(appInfo.uid, "MNC"));
							} catch (Throwable ignored) {
								config.mnc = 1;
							}
						}

						if (restricted)
							param.args[i] = config;

						break;
					}
			}
			break;

		case next:
			// Do nothing
			break;

		case handleReceiver:
			if (param.args.length > 0 && param.args[0] != null) {
				Field fieldIntent = param.args[0].getClass().getDeclaredField("intent");
				fieldIntent.setAccessible(true);
				Intent intent = (Intent) fieldIntent.get(param.args[0]);
				if (intent != null) {
					if (checkIntent(Binder.getCallingUid(), intent)) {
						finish(param);
						param.setResult(null);
					}
				}
			}
			break;

		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case bindApplication:
			// Do nothing
			break;

		case next:
			Message msg = (Message) param.getResult();
			if (msg != null) {
				if (msg.obj instanceof Intent) {
					Intent intent = (Intent) msg.obj;
					if (intent != null)
						if (checkIntent(Binder.getCallingUid(), intent))
							param.setResult(null);
				}
			}
			break;

		case handleReceiver:
			// Do nothing
			break;
		}
	}

	private boolean checkIntent(int uid, Intent intent) throws Throwable {
		String action = intent.getAction();
		if (mapActionRestriction.containsKey(action)) {
			// Get restriction category
			String restrictionName = mapActionRestriction.get(action);

			if (Intent.ACTION_NEW_OUTGOING_CALL.equals(action)) {
				// Outgoing call
				if (intent.hasExtra(Intent.EXTRA_PHONE_NUMBER)) {
					String phoneNumber = intent.getStringExtra(Intent.EXTRA_PHONE_NUMBER);
					if (phoneNumber != null)
						if (isRestrictedExtra(uid, restrictionName, action, phoneNumber))
							intent.putExtra(Intent.EXTRA_PHONE_NUMBER,
									(String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "PhoneNumber"));
				}

			} else if (TelephonyManager.ACTION_PHONE_STATE_CHANGED.equals(action)) {
				// Incoming call
				if (intent.hasExtra(TelephonyManager.EXTRA_INCOMING_NUMBER)) {
					String phoneNumber = intent.getStringExtra(TelephonyManager.EXTRA_INCOMING_NUMBER);
					if (phoneNumber != null) {
						if (isRestrictedExtra(uid, restrictionName, action, phoneNumber))
							intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER,
									(String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "PhoneNumber"));
					}
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
						if (!XPackageManager.isPackageAllowed(packageName))
							return true;
				}

			} else if (isRestrictedExtra(uid, restrictionName, action, intent.getDataString()))
				return true;

		}

		return false;
	}

	private void finish(XParam param) {
		// unscheduleGcIdler
		if (param.thisObject != null)
			try {
				Method unschedule = param.thisObject.getClass().getDeclaredMethod("unscheduleGcIdler");
				unschedule.setAccessible(true);
				unschedule.invoke(param.thisObject);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}

		// data.finish
		if (param.args[0] instanceof BroadcastReceiver.PendingResult)
			try {
				BroadcastReceiver.PendingResult pr = (BroadcastReceiver.PendingResult) param.args[0];
				pr.finish();
			} catch (IllegalStateException ignored) {
				// No receivers for action ...
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
	}
}

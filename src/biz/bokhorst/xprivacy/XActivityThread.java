package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;
import static de.robv.android.xposed.XposedHelpers.findMethodExact;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import android.content.Context;
import android.content.Intent;
import android.os.Binder;
import android.os.Bundle;
import android.telephony.TelephonyManager;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityThread extends XHook {

	private String mActionName;

	public XActivityThread(String methodName, String permissionName, String actionName) {
		super(methodName, permissionName);
		mActionName = actionName;
	}

	// @formatter:off
	/*
		W/System.err(  451): 	at biz.bokhorst.xprivacy.XIntentFilter.before(XIntentFilter.java:26)
		W/System.err(  451): 	at biz.bokhorst.xprivacy.XPrivacy$1.beforeHookedMethod(XPrivacy.java:123)
		W/System.err(  451): 	at de.robv.android.xposed.XposedBridge.handleHookedMethod(XposedBridge.java:432)
		W/System.err(  451): 	at android.content.IntentFilter.match(Native Method)
		W/System.err(  451): 	at com.android.server.IntentResolver.buildResolveList(IntentResolver.java:546)
		W/System.err(  451): 	at com.android.server.IntentResolver.queryIntentFromList(IntentResolver.java:214)
		W/System.err(  451): 	at com.android.server.pm.PackageManagerService$ActivityIntentResolver.queryIntentForPackage(PackageManagerService.java:4796)
		W/System.err(  451): 	at com.android.server.pm.PackageManagerService.queryIntentReceivers(PackageManagerService.java:2704)
		W/System.err(  451): 	at com.android.server.am.ActivityManagerService.broadcastIntentLocked(ActivityManagerService.java:13036)
		W/System.err(  451): 	at com.android.server.am.ActivityManagerService.broadcastIntent(ActivityManagerService.java:13211)
		W/System.err(  451): 	at android.app.ActivityManagerNative.onTransact(ActivityManagerNative.java:318)
		W/System.err(  451): 	at com.android.server.am.ActivityManagerService.onTransact(ActivityManagerService.java:1616)
		W/System.err(  451): 	at android.os.Binder.execTransact(Binder.java:367)
		W/System.err(  451): 	at dalvik.system.NativeStart.run(Native Method)

	    private void buildResolveList(Intent intent, FastImmutableArraySet<String> categories,
	            boolean debug, boolean defaultOnly,
	            String resolvedType, String scheme, List<F> src, List<R> dest, int userId) {

	    public List<ResolveInfo> queryIntentReceivers(Intent intent, String resolvedType, int flags,
	            int userId) {
   	*/
	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		try {
			if (param.args[0] != null) {
				// Get intent
				Field fieldIntent = findField(param.args[0].getClass(), "intent");
				Intent intent = (Intent) fieldIntent.get(param.args[0]);

				// Process intent
				if (intent != null && mActionName.equals(intent.getAction())) {
					Bundle bundle = intent.getExtras();
					if (bundle == null)
						return;
					if (!isAllowed(param)) {
						if (intent.getAction().equals(Intent.ACTION_NEW_OUTGOING_CALL)) {
							String phoneNumber = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
							if (phoneNumber != null)
								intent.putExtra(Intent.EXTRA_PHONE_NUMBER, XPermissions.cDefaceString);
						} else if (intent.getAction().equals(TelephonyManager.ACTION_PHONE_STATE_CHANGED)) {
							String phoneNumber = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
							if (phoneNumber != null)
								intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER, XPermissions.cDefaceString);
						}
					}
				}
			}
		} catch (Throwable ex) {
			XUtil.bug(this, ex);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
	}

	@Override
	protected boolean isAllowed(MethodHookParam param) throws Throwable {
		Method method = findMethodExact(param.thisObject.getClass(), "getSystemContext", new Object[0]);
		Context context = (Context) method.invoke(param.thisObject, new Object[0]);
		int uid = Binder.getCallingUid();
		return getAllowed(context, uid, true);
	}
}

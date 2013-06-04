package biz.bokhorst.xprivacy;

import android.content.Context;
import android.content.Intent;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivity extends XHook {

	private String mActionName;

	public XActivity(String methodName, String restrictionName, String[] permissions, String actionName) {
		super(methodName, restrictionName, permissions);
		mActionName = actionName;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		try {
			// Get intent
			Intent intent = (Intent) param.args[0];
			if (intent != null && intent.getAction() != null)
				XUtil.log(this, Log.INFO, "Start action=" + intent.getAction() + " uid=" + Binder.getCallingUid());
			XUtil.dumpIntent(intent);

			// Process intent
			if (intent != null && mActionName.equals(intent.getAction()))
				if (isRestricted(param))
					param.setResult(null);
		} catch (Throwable ex) {
			XUtil.bug(this, ex);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = (Context) param.thisObject;
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

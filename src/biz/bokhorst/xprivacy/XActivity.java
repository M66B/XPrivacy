package biz.bokhorst.xprivacy;

import android.content.Context;
import android.content.Intent;
import android.os.Binder;
import android.os.Process;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivity extends XHook {

	private String mActionName;

	public XActivity(String methodName, String restrictionName, String actionName) {
		super(methodName, restrictionName);
		mActionName = actionName;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		try {
			// Get intent
			Intent intent = (Intent) param.args[0];
			if (intent != null && intent.getAction() != null)
				XUtil.log(this, Log.INFO, "Send action=" + intent.getAction());

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
		uid = Process.myUid();
		return getRestricted(context, uid, true);
	}
}

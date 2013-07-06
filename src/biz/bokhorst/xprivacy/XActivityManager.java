package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;

import android.app.ActivityManager;
import android.content.Context;
import android.os.Binder;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityManager extends XHook {

	public XActivityManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public List<RecentTaskInfo> getRecentTasks(int maxNum, int flags)
	// public List<RunningAppProcessInfo> getRunningAppProcesses()
	// public List<RunningServiceInfo> getRunningServices(int maxNum)
	// public List<RunningTaskInfo> getRunningTasks(int maxNum)
	// frameworks/base/core/java/android/app/ActivityManager.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			String methodName = param.method.getName();
			if (methodName.equals("getRecentTasks"))
				param.setResult(new ArrayList<ActivityManager.RecentTaskInfo>());
			else if (methodName.equals("getRunningAppProcesses"))
				param.setResult(new ArrayList<ActivityManager.RunningAppProcessInfo>());
			else if (methodName.equals("getRunningServices"))
				param.setResult(new ArrayList<ActivityManager.RunningServiceInfo>());
			else if (methodName.equals("getRunningTasks"))
				param.setResult(new ArrayList<ActivityManager.RunningTaskInfo>());
			else
				Util.log(this, Log.WARN, "Unknown method=" + methodName);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

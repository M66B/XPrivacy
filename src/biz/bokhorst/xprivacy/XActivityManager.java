package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.app.ActivityManager;
import android.content.Context;
import android.os.Binder;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityManager extends XHook {

	private XActivityManager(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "android.app.ActivityManager";
	}

	// public List<RecentTaskInfo> getRecentTasks(int maxNum, int flags)
	// public List<RunningAppProcessInfo> getRunningAppProcesses()
	// public List<RunningServiceInfo> getRunningServices(int maxNum)
	// public List<RunningTaskInfo> getRunningTasks(int maxNum)
	// frameworks/base/core/java/android/app/ActivityManager.java
	// http://developer.android.com/reference/android/app/ActivityManager.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		String[] acts = new String[] { "getRecentTasks", "getRunningAppProcesses", "getRunningServices",
				"getRunningTasks" };
		for (String act : acts)
			listHook.add(new XActivityManager(act, PrivacyManager.cSystem));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("getRecentTasks")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RecentTaskInfo>());
		} else if (methodName.equals("getRunningAppProcesses")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningAppProcessInfo>());
		} else if (methodName.equals("getRunningServices")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningServiceInfo>());
		} else if (methodName.equals("getRunningTasks")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningTaskInfo>());
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
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

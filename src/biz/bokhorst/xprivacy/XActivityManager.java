package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.ActivityManager;
import android.util.Log;

public class XActivityManager extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XActivityManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// public List<RecentTaskInfo> getRecentTasks(int maxNum, int flags)
	// public List<RunningAppProcessInfo> getRunningAppProcesses()
	// public List<RunningServiceInfo> getRunningServices(int maxNum)
	// public List<RunningTaskInfo> getRunningTasks(int maxNum)
	// frameworks/base/core/java/android/app/ActivityManager.java
	// http://developer.android.com/reference/android/app/ActivityManager.html

	private enum Methods {
		getRecentTasks, getRunningAppProcesses, getRunningServices, getRunningTasks
	};

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods act : Methods.values())
			listHook.add(new XActivityManager(act, PrivacyManager.cSystem, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getRecentTasks) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RecentTaskInfo>());

		} else if (mMethod == Methods.getRunningAppProcesses) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningAppProcessInfo>());

		} else if (mMethod == Methods.getRunningServices) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningServiceInfo>());

		} else if (mMethod == Methods.getRunningTasks) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningTaskInfo>());

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

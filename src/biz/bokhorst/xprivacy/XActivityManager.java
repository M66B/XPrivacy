package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.ActivityManager;

public class XActivityManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.app.ActivityManager";

	private XActivityManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public List<RecentTaskInfo> getRecentTasks(int maxNum, int flags)
	// public List<RunningAppProcessInfo> getRunningAppProcesses()
	// public List<RunningServiceInfo> getRunningServices(int maxNum)
	// public List<RunningTaskInfo> getRunningTasks(int maxNum)
	// frameworks/base/core/java/android/app/ActivityManager.java
	// http://developer.android.com/reference/android/app/ActivityManager.html

	// public List<ActivityManager.RecentTaskInfo> getRecentTasks(int maxNum, int flags, int userId)
	// public List<ActivityManager.RunningAppProcessInfo> getRunningAppProcesses()
	// public List<ActivityManager.RunningServiceInfo> getServices(int maxNum, int flags)
	// public List<RunningTaskInfo> getTasks(int maxNum, int flags, IThumbnailReceiver receiver)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/server/am/ActivityManagerService.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		getRecentTasks, getRunningAppProcesses, getRunningServices, getRunningTasks,
		Srv_getRecentTasks, Srv_getRunningAppProcesses, Srv_getServices, Srv_getTasks
	};
	// @formatter:on

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			for (Methods act : Methods.values())
				listHook.add(new XActivityManager(act, PrivacyManager.cSystem, className));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getRecentTasks:
		case Srv_getRecentTasks:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RecentTaskInfo>());
			break;

		case getRunningAppProcesses:
		case Srv_getRunningAppProcesses:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningAppProcessInfo>());
			break;

		case getRunningServices:
		case Srv_getServices:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningServiceInfo>());
			break;

		case getRunningTasks:
		case Srv_getTasks:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ActivityManager.RunningTaskInfo>());
			break;
		}
	}
}

package biz.bokhorst.xprivacy;

import java.util.List;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.content.Context;
import de.robv.android.xposed.XposedBridge;

public class XUtil {
	public static final String LOG_VERBOSE = "Verbose";
	public static final String LOG_INFO = "Info";
	public static final String LOG_WARNING = "Warning";
	public static final String LOG_ERROR = "Error";

	public static String getProcessNameByUid(Context context, int uid) {
		ActivityManager manager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
		if (manager != null) {
			List<RunningAppProcessInfo> lstProcess = manager.getRunningAppProcesses();
			if (lstProcess != null)
				for (RunningAppProcessInfo processInfo : lstProcess)
					if (processInfo.uid == uid)
						return processInfo.processName;
		}
		return "";
	}

	public static void log(XHook hook, String prefix, String message) {
		if (prefix != LOG_VERBOSE)
			XposedBridge.log(String.format("XPrivacy(%s) %s %s", (hook == null ? "" : hook.getClass().getSimpleName()),
					prefix, message));
	}
}

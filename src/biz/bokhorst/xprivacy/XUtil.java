package biz.bokhorst.xprivacy;

import android.util.Log;

public class XUtil {
	public static final int LOG_DEBUG = Log.DEBUG;
	public static final int LOG_INFO = Log.INFO;
	public static final int LOG_WARNING = Log.WARN;
	public static final int LOG_ERROR = Log.ERROR;

	public static void log(XHook hook, int priority, String msg) {
		if (priority != LOG_DEBUG)
			if (hook == null)
				Log.println(priority, "XPrivacy", msg);
			else
				Log.println(priority, String.format("XPrivacy/%s", hook.getClass().getSimpleName()), msg);
	}
}

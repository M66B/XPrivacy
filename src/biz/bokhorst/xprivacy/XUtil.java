package biz.bokhorst.xprivacy;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.content.Context;
import android.content.pm.PackageManager;
import android.content.res.Resources;
import android.util.Log;

public class XUtil {

	public static void log(XHook hook, int priority, String msg) {
		if (priority != Log.DEBUG)
			if (hook == null)
				Log.println(priority, "XPrivacy", msg);
			else
				Log.println(priority, String.format("XPrivacy/%s", hook.getClass().getSimpleName()), msg);
	}

	public static void bug(XHook hook, Throwable ex) {
		log(hook, Log.ERROR, ex.toString());
		ex.printStackTrace();
	}

	public static void logStack(XHook hook) {
		log(hook, Log.INFO, Log.getStackTraceString(new Exception("StackTrace")));
	}

	public static int getXposedVersion() {
		final Pattern PATTERN_APP_PROCESS_VERSION = Pattern.compile(".*with Xposed support \\(version (.+)\\).*");
		try {
			InputStream is = new FileInputStream("/system/bin/app_process");
			BufferedReader br = new BufferedReader(new InputStreamReader(is));
			String line;
			while ((line = br.readLine()) != null) {
				if (!line.contains("Xposed"))
					continue;
				Matcher m = PATTERN_APP_PROCESS_VERSION.matcher(line);
				if (m.find()) {
					br.close();
					is.close();
					return Integer.parseInt(m.group(1));
				}
			}
			br.close();
			is.close();
		} catch (Throwable ex) {
		}
		return -1;
	}

	public static boolean isXposedEnabled() {
		// Will be hooked to return true
		log(null, Log.WARN, "XPrivacy not enabled");
		return false;
	}

	public static Context getXContext(Context context) throws Throwable {
		String xPackageName = XUtil.class.getPackage().getName();
		return context.createPackageContext(xPackageName, 0);
	}

	public static Resources getXResources(Context context) throws Throwable {
		String xPackageName = XUtil.class.getPackage().getName();
		PackageManager pm = context.getPackageManager();
		return pm.getResourcesForApplication(xPackageName);
	}

	public static String getXString(Context context, int id) throws Throwable {
		return getXResources(context).getString(id);
	}
}

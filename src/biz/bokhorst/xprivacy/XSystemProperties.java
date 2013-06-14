package biz.bokhorst.xprivacy;

import java.util.Arrays;
import java.util.List;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XSystemProperties extends XHook {

	private List<String> mKeyList;

	public XSystemProperties(String methodName, String restrictionName, String[] permissions, String[] keys) {
		super(methodName, restrictionName, permissions);
		mKeyList = Arrays.asList(keys);
	}

	// public static String get(String key)
	// public static String get(String key, String def)
	// public static boolean getBoolean(String key, boolean def)
	// public static int getInt(String key, int def)
	// public static long getLong(String key, long def)
	// public static String getLongString(String key, String def)
	// frameworks/base/core/java/android/os/SystemProperties.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String key = (String) param.args[0];
		if (key != null && mKeyList.contains(key))
			if (isRestricted(param))
				if (param.method.getName().equals("get"))
					param.setResult(XRestriction.cDefaceString);
				else
					param.setResult(param.args[1]);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

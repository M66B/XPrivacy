package biz.bokhorst.xprivacy;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XSystemProperties extends XHook {

	private String mPropertyName;

	public XSystemProperties(String methodName, String restrictionName, String[] permissions, String propertyName) {
		super(methodName, restrictionName, permissions, propertyName);
		mPropertyName = propertyName;
	}

	// public static String get(String key)
	// public static String get(String key, String def)
	// public static boolean getBoolean(String key, boolean def)
	// public static int getInt(String key, int def)
	// public static long getLong(String key, long def)
	// frameworks/base/core/java/android/os/SystemProperties.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String key = (String) param.args[0];
		if (mPropertyName.equals(key))
			if (isRestricted(param, mPropertyName))
				if (param.method.getName().equals("get"))
					param.setResult(PrivacyManager.getDefacedProp(mPropertyName));
				else
					param.setResult(param.args[1]);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

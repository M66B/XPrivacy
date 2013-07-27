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
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String key = (param.args.length > 0 ? (String) param.args[0] : null);
		if (key != null)
			if (mPropertyName.startsWith("%") ? key.contains(mPropertyName.substring(1)) : key.equals(mPropertyName))
				if (isRestricted(param, mPropertyName))
					if (param.method.getName().equals("get"))
						param.setResult(PrivacyManager.getDefacedProp(mPropertyName));
					else if (param.args.length > 1)
						param.setResult(param.args[1]);
					else
						param.setResult(null);
	}
}

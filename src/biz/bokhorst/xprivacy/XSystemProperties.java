package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XSystemProperties extends XHook {
	private Methods mMethod;
	private String mPropertyName;

	private XSystemProperties(Methods method, String restrictionName, String propertyName) {
		super(restrictionName, method.name(), propertyName);
		mMethod = method;
		mPropertyName = propertyName;
	}

	public String getClassName() {
		return "android.os.SystemProperties";
	}

	// public static String get(String key)
	// public static String get(String key, String def)
	// public static boolean getBoolean(String key, boolean def)
	// public static int getInt(String key, int def)
	// public static long getLong(String key, long def)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/android/os/SystemProperties.java/

	private enum Methods {
		get, getBoolean, getInt, getLong
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		String[] props = new String[] { "%imei", "%hostname", "%serialno", "%macaddr", "%cid" };
		for (String prop : props)
			for (Methods getter : Methods.values())
				listHook.add(new XSystemProperties(getter, PrivacyManager.cIdentification, prop));

		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		String key = (param.args.length > 0 ? (String) param.args[0] : null);
		if (key != null)
			if (mPropertyName.startsWith("%") ? key.contains(mPropertyName.substring(1)) : key.equals(mPropertyName))
				if (mMethod == Methods.get) {
					if (param.getResult() != null && isRestrictedExtra(param, mPropertyName, key))
						param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), mPropertyName));

				} else if (param.args.length > 1) {
					if (isRestrictedExtra(param, mPropertyName, key))
						param.setResult(param.args[1]);

				} else
					Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.res.Configuration;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XResources extends XHook {
	private Methods mMethod;

	private XResources(Methods method) {
		super(null, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.content.res.Resources";
	}

	// public Configuration getConfiguration()
	// frameworks/base/core/java/android/content/res/Resources.java
	// http://developer.android.com/reference/android/content/res/Resources.html
	// http://developer.android.com/reference/android/content/res/Configuration.html

	private enum Methods {
		getConfiguration
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XResources(Methods.getConfiguration));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getConfiguration) {
			boolean restricted = false;
			int uid = Binder.getCallingUid();
			Configuration config = new Configuration((Configuration) param.getResult());

			if (getRestricted(uid, PrivacyManager.cPhone, "Configuration.MCC", false)) {
				restricted = true;
				try {
					config.mcc = Integer.parseInt((String) PrivacyManager.getDefacedProp(uid, "MCC"));
				} catch (Throwable ex) {
					config.mcc = 1;
				}
			}

			if (getRestricted(uid, PrivacyManager.cPhone, "Configuration.MNC", false)) {
				restricted = true;
				try {
					config.mnc = Integer.parseInt((String) PrivacyManager.getDefacedProp(uid, "MNC"));
				} catch (Throwable ex) {
					config.mnc = 1;
				}
			}

			if (restricted)
				param.setResult(config);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

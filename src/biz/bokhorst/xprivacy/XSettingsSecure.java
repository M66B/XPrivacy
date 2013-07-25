package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.content.ContentResolver;
import android.content.Context;
import android.os.Binder;
import android.provider.Settings;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XSettingsSecure extends XHook {

	public XSettingsSecure(String methodName, String restrictionName) {
		super(methodName, restrictionName, new String[] {}, null);
	}

	// @formatter:off

	// public synchronized static String getString(ContentResolver resolver, String name)
	// frameworks/base/core/java/android/provider/Settings.java
	// frameworks/base/core/java/android/content/ContentResolver.java
	// http://developer.android.com/reference/android/provider/Settings.Secure.html

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResult() != null) {
			String name = (param.args.length > 1 ? (String) param.args[1] : null);
			if (Settings.Secure.ANDROID_ID.equals(name))
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp("ANDROID_ID"));
		}
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		if (param.args.length > 0) {
			ContentResolver contentResolver = (ContentResolver) param.args[0];
			try {
				Field fieldContext = findField(ContentResolver.class, "mContext");
				context = (Context) fieldContext.get(contentResolver);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

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
		super(methodName, restrictionName, new String[] {});
	}

	// @formatter:off

	// public synchronized static String getString(ContentResolver resolver, String name)
	// frameworks/base/core/java/android/provider/Settings.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResultOrThrowable() != null)
			try {
				String name = (String) param.args[1];
				if (Settings.Secure.ANDROID_ID.equals(name))
					if (param.getResult() != null)
						if (isRestricted(param))
							param.setResult(Long.toHexString(XRestriction.cDefaceHex));
			} catch (IllegalArgumentException ex) {
				// Android ID is requested before system initialization
				// "Attempt to launch content provider before system ready"
				XUtil.bug(this, ex);
			}
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		ContentResolver contentResolver = (ContentResolver) param.args[0];
		// CM10/CM10.1
		Field fieldContext = findField(contentResolver.getClass(), "mContext");
		Context context = (Context) fieldContext.get(contentResolver);
		int uid = Binder.getCallingUid();
		return (uid != XRestriction.cUidAndroid && getRestricted(context, uid, true));
	}
}

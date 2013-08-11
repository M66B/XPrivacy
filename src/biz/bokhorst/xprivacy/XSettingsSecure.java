package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.ContentResolver;
import android.content.Context;
import android.os.Binder;
import android.provider.Settings;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XSettingsSecure extends XHook {

	private XSettingsSecure(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "android.provider.Settings.Secure";
	}

	// @formatter:off

	// public synchronized static String getString(ContentResolver resolver, String name)
	// frameworks/base/core/java/android/provider/Settings.java
	// frameworks/base/core/java/android/content/ContentResolver.java
	// http://developer.android.com/reference/android/provider/Settings.Secure.html

	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XSettingsSecure("getString", PrivacyManager.cIdentification));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String name = (param.args.length > 1 ? (String) param.args[1] : null);
		if (Settings.Secure.ANDROID_ID.equals(name))
			if (param.getResult() != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp("ANDROID_ID"));
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
		if (context != null && context.getPackageName().equals(this.getClass().getPackage().getName()))
			return false;
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

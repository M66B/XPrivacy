package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.content.ContentResolver;
import android.content.Context;
import android.os.Binder;
import android.provider.Settings;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XSettingsSecure extends XHook {

	public XSettingsSecure(String methodName, String permissionName) {
		super(methodName, permissionName);
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		try {
			if (Settings.Secure.ANDROID_ID.equals(param.args[1])) {
				ContentResolver contentResolver = (ContentResolver) param.args[0];
				Field fieldContext = findField(contentResolver.getClass(), "mContext");
				Context context = (Context) fieldContext.get(contentResolver);
				if (!isAllowed(context, Binder.getCallingUid(), true))
					param.setResult(Long.toHexString(0xDEFACEL));
			}
		} catch (Throwable ex) {
			XUtil.bug(this, ex);
			// Exceptions happen because Android ID is requested before the system is completely initialized
		}
	}
}

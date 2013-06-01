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
		param.getResultOrThrowable();
		try {
			String name = (String) param.args[1];
			if (Settings.Secure.ANDROID_ID.equals(name))
				if (param.getResult() != null)
					if (!isAllowed(param))
						param.setResult(Long.toHexString(XPermissions.cDefaceHex));
		} catch (IllegalArgumentException ex) {
			// Android ID is requested before system initialization
			XUtil.bug(this, ex);
		}
	}

	@Override
	protected boolean isAllowed(MethodHookParam param) throws Throwable {
		ContentResolver contentResolver = (ContentResolver) param.args[0];
		Field fieldContext = findField(contentResolver.getClass(), "mContext");
		Context context = (Context) fieldContext.get(contentResolver);
		int uid = Binder.getCallingUid();
		return getAllowed(context, uid, true);
	}

}

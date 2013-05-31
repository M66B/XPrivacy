package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.content.Context;
import android.os.Binder;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XTelephonyManager extends XHook {

	public XTelephonyManager(String methodName, String permissionName) {
		super(methodName, permissionName);
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		super.after(param);
		Field fieldContext = findField(param.thisObject.getClass(), "sContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		if (!getAllowed(context, uid, true))
			param.setResult("PRIVATE");
	}
}

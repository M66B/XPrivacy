package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;

import android.content.Context;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XLocationManager extends XHook {

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		isAllowed(context, uid, "location");
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
	}
}

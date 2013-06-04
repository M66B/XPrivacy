package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;

import android.content.Context;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XTelephonyRegistry extends XHook {

	public XTelephonyRegistry(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// void notifyCallState(int state, String incomingNumber)
		String number = (String) param.args[1];
		XUtil.log(this, Log.INFO, "Number=" + number);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import android.content.Context;
import android.location.LocationManager;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XContextImpl extends XHook {

	@Override
	protected void before(MethodHookParam param) throws Throwable {
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {

		// Create hook
		XC_MethodHook methodHook = new XC_MethodHook() {
			@Override
			protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
				Field fieldContext = findField(param.thisObject.getClass(), "mContext");
				Context context = (Context) fieldContext.get(param.thisObject);
				int uid = Binder.getCallingUid();
				if (!isAllowed(context, uid, "location")) {

				}
			}

			@Override
			protected void afterHookedMethod(MethodHookParam param) throws Throwable {
			}
		};

		// Hook requestLocationUpdates
		for (Method method : LocationManager.class.getDeclaredMethods())
			if (method.getName().equals("requestLocationUpdates") && Modifier.isPublic(method.getModifiers()))
				XposedBridge.hookMethod(method, methodHook);
	}
}

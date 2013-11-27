package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import de.robv.android.xposed.XposedBridge;

public class XContextWrapper extends XHook {
	private Methods mMethod;
	private static boolean mWindowManagerHooked = false;

	private XContextWrapper(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.content.ContextWrapper";
	}

	// public Context getApplicationContext()
	// public Context getBaseContext()
	// public Object getSystemService(String name)
	// frameworks/base/core/java/android/content/ContextWrapper.java

	private enum Methods {
		getApplicationContext, getBaseContext, getSystemService
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XContextWrapper(Methods.getApplicationContext, null));
		listHook.add(new XContextWrapper(Methods.getBaseContext, null));
		listHook.add(new XContextWrapper(Methods.getSystemService, PrivacyManager.cView));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
		if (mMethod == Methods.getApplicationContext || mMethod == Methods.getBaseContext) {
			int uid = Binder.getCallingUid();
			Context context = (Context) param.getResult();
			if (context != null && PrivacyManager.isExtraUsageDataEnabled(uid))
				PrivacyManager.sendUsageData(this, context);
		} else if (mMethod == Methods.getSystemService) {
			if (!mWindowManagerHooked) {
				String name = (String) param.args[0];
				if (name != null && name.equals(Context.WINDOW_SERVICE)) {

					// @formatter:off

					// public void addView(View view, ViewGroup.LayoutParams params)
					// public void removeView(View view)
					// public void updateViewLayout(View view, ViewGroup.LayoutParams params)
					// http://developer.android.com/reference/android/view/ViewManager.html
					// http://developer.android.com/reference/android/view/WindowManager.html

					// @formatter:on

					Object windowManager = param.getResultOrThrowable();
					if (windowManager != null) {
						{
							Class<?> clazz = windowManager.getClass();
							hook(clazz, "addView", View.class, ViewGroup.LayoutParams.class);
							hook(clazz, "removeView", View.class);
							hook(clazz, "updateViewLayout", View.class, ViewGroup.LayoutParams.class);
							mWindowManagerHooked = true;
						}
					}
				}
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	private void hook(Class<?> clazz, final String methodName, Class<?>... types) {
		try {
			Util.log(this, Log.INFO, "Hooking " + clazz.getName() + "." + methodName);
			Method addView = clazz.getDeclaredMethod(methodName, types);
			XposedBridge.hookMethod(addView, new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
					if (isRestricted(param, methodName))
						param.setResult(null);
				}
			});
		} catch (NoSuchMethodException ex) {
			Util.bug(this, ex);
		}
	}
}

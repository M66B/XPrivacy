package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.Application;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XApplication extends XHook {
	private Methods mMethod;
	private static XUncaughtExceptionHandler mUncaughtExceptionHandler;

	public XApplication(Methods method) {
		super(null, method.name(), null);
		mMethod = method;
	}

	@Override
	public String getClassName() {
		return "android.app.Application";
	}

	// public void onCreate ()
	// frameworks/base/core/java/android/app/Application.java
	// http://developer.android.com/reference/android/app/Application.html

	private enum Methods {
		onCreate, attach
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XApplication(Methods.onCreate));
		listHook.add(new XApplication(Methods.attach));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.onCreate || mMethod == Methods.attach) {
			if (mUncaughtExceptionHandler == null) {
				mUncaughtExceptionHandler = new XUncaughtExceptionHandler((Application) param.thisObject);
			} else {
				Util.log(this, Log.WARN, "Application.onCreate has already been called.");
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

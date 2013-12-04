package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XThread extends XHook {
	private Methods mMethod;

	private XThread(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "java.lang.Thread";
	}

	// public void setUncaughtExceptionHandler(UncaughtExceptionHandler handler)
	// libcore/luni/src/main/java/java/lang/Thread.java
	// http://developer.android.com/reference/java/lang/Thread.html

	private enum Methods {
		setUncaughtExceptionHandler
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XThread(Methods.setUncaughtExceptionHandler, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.setUncaughtExceptionHandler) {
			if (param.args.length > 0) {
				Thread thread = (Thread) param.thisObject;
				Thread.UncaughtExceptionHandler oldHandler = thread.getUncaughtExceptionHandler();
				Thread.UncaughtExceptionHandler newHandler = (Thread.UncaughtExceptionHandler) param.args[0];
				if (oldHandler instanceof XApplication.XUncaughtExceptionHandler) {
					((XApplication.XUncaughtExceptionHandler) oldHandler).setDefaultHandler(newHandler);
					param.setResult(null);
				}
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

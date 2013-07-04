package biz.bokhorst.xprivacy;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import android.os.Environment;
import biz.bokhorst.xprivacy.XHook;

public class XEnvironment extends XHook {

	public XEnvironment(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public static String getExternalStorageState()
	// frameworks/base/core/java/android/os/Environment.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param))
			param.setResult(Environment.MEDIA_UNMOUNTED);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

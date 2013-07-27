package biz.bokhorst.xprivacy;

import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XMediaRecorder extends XHook {

	public XMediaRecorder(String methodName, String restrictionName, String[] permissions) {
		super(restrictionName, methodName, permissions, null);
	}

	// public void setOutputFile(FileDescriptor fd)
	// public void setOutputFile(String path)
	// frameworks/base/media/java/android/media/MediaRecorder.java
	// http://developer.android.com/reference/android/media/MediaRecorder.html

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("setOutputFile")) {
			if (isRestricted(param)) {
				param.setResult(null);
				notifyUser(this.getClass().getSimpleName());
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

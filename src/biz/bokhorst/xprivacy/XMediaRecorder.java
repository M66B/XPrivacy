package biz.bokhorst.xprivacy;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XMediaRecorder extends XHook {

	public XMediaRecorder(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public void setOutputFile(FileDescriptor fd)
	// public void setOutputFile(String path)
	// frameworks/base/media/java/android/media/MediaRecorder.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			param.setResult(null);
			notifyUser(this.getClass().getSimpleName());
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

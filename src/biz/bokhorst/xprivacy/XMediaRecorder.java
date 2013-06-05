package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XMediaRecorder extends XHook {

	public XMediaRecorder(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// public void setOutputFile(FileDescriptor fd)
	// public void setOutputFile(String path)
	// public void prepare()
	// frameworks/base/media/java/android/media/MediaRecorder.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			Field fieldPath = findField(param.thisObject.getClass(), "mPath");
			Field fieldFd = findField(param.thisObject.getClass(), "mFd");
			fieldPath.set(param.thisObject, null);
			fieldFd.set(param.thisObject, null);
			notifyUser();
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

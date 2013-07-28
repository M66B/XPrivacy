package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XMediaRecorder extends XHook {

	private XMediaRecorder(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "android.media.MediaRecorder";
	}

	// public void setOutputFile(FileDescriptor fd)
	// public void setOutputFile(String path)
	// frameworks/base/media/java/android/media/MediaRecorder.java
	// http://developer.android.com/reference/android/media/MediaRecorder.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XMediaRecorder("setOutputFile", PrivacyManager.cMedia));
		return listHook;
	}

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

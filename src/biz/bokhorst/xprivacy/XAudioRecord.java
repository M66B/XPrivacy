package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XAudioRecord extends XHook {

	private XAudioRecord(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "android.media.AudioRecord";
	}

	// public void startRecording()
	// public void startRecording(MediaSyncEvent syncEvent)
	// frameworks/base/media/java/android/media/AudioRecord.java
	// http://developer.android.com/reference/android/media/AudioRecord.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XAudioRecord("startRecording", PrivacyManager.cMedia));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("startRecording")) {
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

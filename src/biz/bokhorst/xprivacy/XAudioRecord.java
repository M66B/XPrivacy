package biz.bokhorst.xprivacy;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XAudioRecord extends XHook {

	public XAudioRecord(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// public void startRecording()
	// public void startRecording(MediaSyncEvent syncEvent)
	// frameworks/base/media/java/android/media/AudioRecord.java

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

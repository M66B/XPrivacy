package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

public class XAudioRecord extends XHook {
	private Methods mMethod;

	private XAudioRecord(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "Audio." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.media.AudioRecord";
	}

	// public void startRecording()
	// public void startRecording(MediaSyncEvent syncEvent)
	// public void stop()
	// frameworks/base/media/java/android/media/AudioRecord.java
	// http://developer.android.com/reference/android/media/AudioRecord.html

	private enum Methods {
		startRecording, stop
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XAudioRecord(Methods.startRecording, PrivacyManager.cMedia));
		listHook.add(new XAudioRecord(Methods.stop, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case startRecording:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case stop:
			if (isRestricted(param, PrivacyManager.cMedia, "Audio.startRecording"))
				param.setResult(null);
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

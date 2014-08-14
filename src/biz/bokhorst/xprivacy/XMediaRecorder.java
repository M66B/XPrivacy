package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

public class XMediaRecorder extends XHook {
	private Methods mMethod;

	private XMediaRecorder(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "MediaRecorder." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.media.MediaRecorder";
	}

	// void setOutputFile(FileDescriptor fd)
	// void setOutputFile(String path)
	// public prepare()
	// public native void start()
	// void stop()
	// frameworks/base/media/java/android/media/MediaRecorder.java
	// http://developer.android.com/reference/android/media/MediaRecorder.html

	private enum Methods {
		setOutputFile, prepare, start, stop
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XMediaRecorder(Methods.setOutputFile, PrivacyManager.cMedia));
		listHook.add(new XMediaRecorder(Methods.prepare, null));
		listHook.add(new XMediaRecorder(Methods.start, PrivacyManager.cMedia));
		listHook.add(new XMediaRecorder(Methods.stop, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case setOutputFile:
		case start:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case prepare:
		case stop:
			if (isRestricted(param, PrivacyManager.cMedia, "MediaRecorder.start"))
				param.setResult(null);
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

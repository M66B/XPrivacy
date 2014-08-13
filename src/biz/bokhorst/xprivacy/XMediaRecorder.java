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

	// public native void start()
	// frameworks/base/media/java/android/media/MediaRecorder.java
	// http://developer.android.com/reference/android/media/MediaRecorder.html

	private enum Methods {
		setOutputFile, start
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XMediaRecorder(Methods.setOutputFile, PrivacyManager.cMedia));
		listHook.add(new XMediaRecorder(Methods.start, PrivacyManager.cMedia));
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
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

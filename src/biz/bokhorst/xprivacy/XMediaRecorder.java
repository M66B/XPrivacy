package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

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
		start
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XMediaRecorder(Methods.start, PrivacyManager.cMedia));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.start) {
			if (isRestricted(param))
				param.setResult(null);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

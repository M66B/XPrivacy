package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XCamera extends XHook {
	private Methods mMethod;

	private XCamera(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "Camera." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.hardware.Camera";
	}

	// @formatter:off

	// public final void setPreviewCallback(PreviewCallback cb)
	// public final void setPreviewCallbackWithBuffer(PreviewCallback cb)
	// public final void setOneShotPreviewCallback(PreviewCallback cb)
	// public final void takePicture(ShutterCallback shutter, PictureCallback raw, PictureCallback jpeg)
	// public final void takePicture(ShutterCallback shutter, PictureCallback raw, PictureCallback postview, PictureCallback jpeg)
	// frameworks/base/core/java/android/hardware/Camera.java
	// http://developer.android.com/reference/android/hardware/Camera.html

	// @formatter:on

	private enum Methods {
		setPreviewCallback, setPreviewCallbackWithBuffer, setOneShotPreviewCallback, takePicture
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods cam : Methods.values())
			listHook.add(new XCamera(cam, PrivacyManager.cMedia));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.setPreviewCallback || mMethod == Methods.setPreviewCallbackWithBuffer
				|| mMethod == Methods.setOneShotPreviewCallback || mMethod == Methods.takePicture) {
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

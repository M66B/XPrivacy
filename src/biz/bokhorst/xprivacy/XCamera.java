package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

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

	// public void setPreviewCallback(Camera.PreviewCallback cb)
	// public void setPreviewCallbackWithBuffer(Camera.PreviewCallback cb)
	// public void setPreviewDisplay(SurfaceHolder holder)
	// public void setPreviewTexture(SurfaceTexture surfaceTexture)
	// public final void setOneShotPreviewCallback (Camera.PreviewCallback cb)
	// public native final void startPreview()
	// public void stopPreview()
	// public final void takePicture(ShutterCallback shutter, PictureCallback raw, PictureCallback jpeg)
	// public final void takePicture(ShutterCallback shutter, PictureCallback raw, PictureCallback postview, PictureCallback jpeg)
	// frameworks/base/core/java/android/hardware/Camera.java
	// http://developer.android.com/reference/android/hardware/Camera.html

	// @formatter:on

	private enum Methods {
		setPreviewCallback, setPreviewCallbackWithBuffer, setPreviewDisplay, setPreviewTexture, setOneShotPreviewCallback, startPreview, stopPreview, takePicture
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods cam : Methods.values())
			listHook.add(new XCamera(cam, cam == Methods.stopPreview ? null : PrivacyManager.cMedia));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case setPreviewCallback:
		case setPreviewCallbackWithBuffer:
		case setPreviewDisplay:
		case setPreviewTexture:
		case setOneShotPreviewCallback:
		case startPreview:
		case takePicture:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case stopPreview:
			if (isRestricted(param, PrivacyManager.cMedia, "Camera.startPreview"))
				param.setResult(null);
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

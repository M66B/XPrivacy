package biz.bokhorst.xprivacy;

import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XCamera extends XHook {

	public XCamera(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
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

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("setPreviewCallback") || methodName.equals("setPreviewCallbackWithBuffer")
				|| methodName.equals("setOneShotPreviewCallback") || methodName.equals("takePicture")) {
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

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

	// public native final void startPreview()
	// public final void takePicture(ShutterCallback shutter, PictureCallback raw, PictureCallback jpeg)
	// public final void takePicture(ShutterCallback shutter, PictureCallback raw, PictureCallback postview, PictureCallback jpeg)
	// frameworks/base/core/java/android/hardware/Camera.java
	// http://developer.android.com/reference/android/hardware/Camera.html

	// @formatter:on

	private enum Methods {
		startPreview, takePicture
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods cam : Methods.values())
			listHook.add(new XCamera(cam, PrivacyManager.cMedia));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case startPreview:
		case takePicture:
			if (isRestricted(param))
				param.setResult(null);
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

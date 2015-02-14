package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XCameraDevice2 extends XHook {
	private Methods mMethod;

	private XCameraDevice2(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "Camera2." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.hardware.camera2.impl.CameraDeviceImpl";
	}

	// @formatter:off

	// public int capture(CaptureRequest request, CaptureListener listener, Handler handler)
	// public int captureBurst(List<CaptureRequest> requests, CaptureListener listener, Handler handler)
	// public int setRepeatingRequest(CaptureRequest request, CaptureListener listener, Handler handler)
	// public int setRepeatingBurst(List<CaptureRequest> requests, CaptureListener listener, Handler handler)
	// frameworks/base/core/java/android/hardware/camera2/impl/CameraDevice.java
	// http://developer.android.com/reference/android/hardware/camera2/CameraDevice.html

	// @formatter:on

	private enum Methods {
		capture, captureBurst, setRepeatingRequest, setRepeatingBurst
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods cam : Methods.values())
			listHook.add(new XCameraDevice2(cam, PrivacyManager.cMedia));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.capture || mMethod == Methods.captureBurst) {
			if (isRestricted(param))
				param.setResult(0);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

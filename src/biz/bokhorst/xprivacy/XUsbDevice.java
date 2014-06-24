package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

import biz.bokhorst.xprivacy.XHook;

public class XUsbDevice extends XHook {
	private Methods mMethod;

	private XUsbDevice(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "USB." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.hardware.usb.UsbDevice";
	}

	// public static int getDeviceId(String name)
	// public int getDeviceId()
	// public String getDeviceName()
	// public static String getDeviceName(int id)
	// http://developer.android.com/reference/android/hardware/usb/UsbDevice.html

	private enum Methods {
		getDeviceId, getDeviceName
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XUsbDevice(Methods.getDeviceId, PrivacyManager.cIdentification));
		listHook.add(new XUsbDevice(Methods.getDeviceName, PrivacyManager.cIdentification));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.getDeviceId) {
			if (isRestricted(param))
				param.setResult(0);

		} else if (mMethod == Methods.getDeviceName) {
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

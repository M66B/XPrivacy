package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
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
	// public String getSerialNumber()
	// http://developer.android.com/reference/android/hardware/usb/UsbDevice.html

	private enum Methods {
		getDeviceId, getDeviceName, getSerialNumber
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XUsbDevice(Methods.getDeviceId, PrivacyManager.cIdentification));
		listHook.add(new XUsbDevice(Methods.getDeviceName, PrivacyManager.cIdentification));
		listHook.add(new XUsbDevice(Methods.getSerialNumber, PrivacyManager.cIdentification));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getDeviceId:
			if (param.args.length > 0 && param.args[0] instanceof String) {
				if (isRestrictedExtra(param, (String) param.args[0]))
					param.setResult(0);
			} else {
				if (isRestricted(param))
					param.setResult(0);
			}
			break;

		case getDeviceName:
		case getSerialNumber:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "USB"));
			break;
		}
	}
}

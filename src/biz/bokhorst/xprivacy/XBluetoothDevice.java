package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;
import biz.bokhorst.xprivacy.XHook;

public class XBluetoothDevice extends XHook {
	private Methods mMethod;

	private XBluetoothDevice(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "Bluetooth." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.bluetooth.BluetoothDevice";
	}

	// public String getAddress()
	// frameworks/base/core/java/android/bluetooth/BluetoothDevice.java
	// http://developer.android.com/reference/android/bluetooth/BluetoothDevice.html

	private enum Methods {
		getAddress
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XBluetoothDevice(Methods.getAddress, PrivacyManager.cNetwork));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getAddress) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "MAC"));

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

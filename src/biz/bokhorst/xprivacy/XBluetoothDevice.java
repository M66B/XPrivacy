package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.XHook;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XBluetoothDevice extends XHook {

	public XBluetoothDevice(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public String getAddress()
	// frameworks/base/core/java/android/bluetooth/BluetoothDevice.java
	// http://developer.android.com/reference/android/bluetooth/BluetoothDevice.html

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResult() != null)
			if (isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp("MAC"));
	}
}

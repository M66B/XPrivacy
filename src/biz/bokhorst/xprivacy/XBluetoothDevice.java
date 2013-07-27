package biz.bokhorst.xprivacy;

import android.util.Log;
import biz.bokhorst.xprivacy.XHook;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XBluetoothDevice extends XHook {

	public XBluetoothDevice(String methodName, String restrictionName, String[] permissions) {
		super(restrictionName, methodName, permissions, null);
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
		String methodName = param.method.getName();
		if (methodName.equals("getAddress")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp("MAC"));
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}
}

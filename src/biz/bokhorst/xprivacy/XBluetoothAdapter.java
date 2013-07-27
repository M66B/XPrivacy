package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.XHook;

import java.util.HashSet;

import android.bluetooth.BluetoothDevice;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XBluetoothAdapter extends XHook {

	public XBluetoothAdapter(String methodName, String restrictionName, String[] permissions) {
		super(restrictionName, methodName, permissions, null);
	}

	// public String getAddress()
	// public Set<BluetoothDevice> getBondedDevices()
	// frameworks/base/core/java/android/bluetooth/BluetoothAdapter.java
	// http://developer.android.com/reference/android/bluetooth/BluetoothAdapter.html

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
		} else if (methodName.equals("getBondedDevices")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new HashSet<BluetoothDevice>());
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}
}

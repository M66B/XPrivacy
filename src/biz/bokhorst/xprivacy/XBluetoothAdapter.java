package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.XHook;

import java.util.HashSet;

import android.bluetooth.BluetoothDevice;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XBluetoothAdapter extends XHook {

	public XBluetoothAdapter(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public String getAddress()
	// public Set<BluetoothDevice> getBondedDevices()
	// frameworks/base/core/java/android/bluetooth/BluetoothAdapter.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResult() != null)
			if (isRestricted(param)) {
				String methodName = param.method.getName();
				if (methodName.equals("getAddress"))
					param.setResult(XRestriction.getDefacedMac());
				else if (methodName.equals("getBondedDevices"))
					param.setResult(new HashSet<BluetoothDevice>());
				else
					XUtil.log(this, Log.WARN, "Unknown method=" + methodName);
			}
	}
}

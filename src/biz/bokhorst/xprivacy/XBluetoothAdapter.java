package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.XHook;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import android.bluetooth.BluetoothDevice;
import android.os.Binder;
import android.util.Log;

public class XBluetoothAdapter extends XHook {
	private Methods mMethod;

	private XBluetoothAdapter(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "Bluetooth." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.bluetooth.BluetoothAdapter";
	}

	// public String getAddress()
	// public Set<BluetoothDevice> getBondedDevices()
	// frameworks/base/core/java/android/bluetooth/BluetoothAdapter.java
	// http://developer.android.com/reference/android/bluetooth/BluetoothAdapter.html

	private enum Methods {
		getAddress, getBondedDevices
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XBluetoothAdapter(Methods.getAddress, PrivacyManager.cNetwork));
		listHook.add(new XBluetoothAdapter(Methods.getBondedDevices, PrivacyManager.cNetwork));
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

		} else if (mMethod == Methods.getBondedDevices) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new HashSet<BluetoothDevice>());

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

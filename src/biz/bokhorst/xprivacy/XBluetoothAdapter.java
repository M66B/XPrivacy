package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.XHook;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import android.bluetooth.BluetoothDevice;
import android.os.Binder;

public class XBluetoothAdapter extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XBluetoothAdapter(Methods method, String restrictionName) {
		super(restrictionName, method.name().replace("Srv_", ""), "Bluetooth." + method.name());
		mMethod = method;
		if (method.name().startsWith("Srv_"))
			mClassName = "com.android.server.BluetoothManagerService";
		else
			mClassName = "android.bluetooth.BluetoothAdapter";
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public String getAddress()
	// public Set<BluetoothDevice> getBondedDevices()
	// public String getName()
	// frameworks/base/core/java/android/bluetooth/BluetoothAdapter.java
	// http://developer.android.com/reference/android/bluetooth/BluetoothAdapter.html
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.4_r1/com/android/server/BluetoothManagerService.java

	// @formatter:on

	private enum Methods {
		getAddress, getBondedDevices, Srv_getAddress, Srv_getName
	};

	public static List<XHook> getInstances(boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (server) {
			listHook.add(new XBluetoothAdapter(Methods.Srv_getAddress, PrivacyManager.cNetwork));
			listHook.add(new XBluetoothAdapter(Methods.Srv_getName, PrivacyManager.cNetwork));
		} else {
			listHook.add(new XBluetoothAdapter(Methods.getAddress, PrivacyManager.cNetwork));
			listHook.add(new XBluetoothAdapter(Methods.getBondedDevices, PrivacyManager.cNetwork));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		int uid = Binder.getCallingUid();
		switch (mMethod) {
		case getAddress:
		case Srv_getAddress:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp(uid, "MAC"));
			break;

		case Srv_getName:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp(uid, "BTName"));
			break;

		case getBondedDevices:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new HashSet<BluetoothDevice>());
			break;
		}
	}
}

package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.XHook;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import android.bluetooth.BluetoothDevice;
import android.os.Binder;
import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XBluetoothAdapter extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XBluetoothAdapter(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// public String getAddress()
	// public Set<BluetoothDevice> getBondedDevices()
	// frameworks/base/core/java/android/bluetooth/BluetoothAdapter.java
	// http://developer.android.com/reference/android/bluetooth/BluetoothAdapter.html

	private enum Methods {
		getAddress, getBondedDevices
	};

	public static List<XHook> getInstances() {
		return getInstances(null);
	}

	public static List<XHook> getInstances(Object instance) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
			String className = (instance == null ? "android.bluetooth.BluetoothAdapter" : instance.getClass().getName());
			listHook.add(new XBluetoothAdapter(Methods.getAddress, PrivacyManager.cNetwork, className));
			listHook.add(new XBluetoothAdapter(Methods.getBondedDevices, PrivacyManager.cNetwork, className));
		}
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
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

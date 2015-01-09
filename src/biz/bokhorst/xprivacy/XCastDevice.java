package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import biz.bokhorst.xprivacy.XHook;

public class XCastDevice extends XHook {
	private Methods mMethod;

	private XCastDevice(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "Cast." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "com.google.android.gms.cast.CastDevice";
	}

	// public static getDeviceId()
	// public Inet4Address getIpAddress()
	// http://developer.android.com/reference/com/google/android/gms/cast/CastDevice.html

	private enum Methods {
		getDeviceId, getIpAddress
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XCastDevice(Methods.getDeviceId, PrivacyManager.cIdentification));
		listHook.add(new XCastDevice(Methods.getIpAddress, PrivacyManager.cIdentification));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
		switch (mMethod) {
		case getDeviceId:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "CastID"));
			break;

		case getIpAddress:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "InetAddress"));
			break;
		}
	}
}

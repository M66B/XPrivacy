package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XInputDevice extends XHook {
	private Methods mMethod;

	private XInputDevice(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "InputDevice." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.view.InputDevice";
	}

	// @formatter:off

	// public String getDescriptor()
	// public String getName()
	// frameworks/base/core/java/android/view/InputDevice.java
	// http://developer.android.com/reference/android/view/InputDevice.html
	
	// @formatter:on

	private enum Methods {
		getDescriptor, getName
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XInputDevice(Methods.getDescriptor, PrivacyManager.cIdentification));
		listHook.add(new XInputDevice(Methods.getName, PrivacyManager.cIdentification));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.getDescriptor || mMethod == Methods.getName) {
			if (isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "DeviceDescriptor"));

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

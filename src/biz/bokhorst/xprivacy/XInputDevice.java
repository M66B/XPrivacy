package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XInputDevice extends XHook {
	private Methods mMethod;

	private XInputDevice(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.view.InputDevice";
	}

	// @formatter:off

	// public String getDescriptor()
	// frameworks/base/core/java/android/view/InputDevice.java
	// http://developer.android.com/reference/android/view/InputDevice.html
	
	// @formatter:on

	private enum Methods {
		getDescriptor
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XInputDevice(Methods.getDescriptor, PrivacyManager.cIdentification));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getDescriptor) {
			if (isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "DeviceDescriptor"));

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

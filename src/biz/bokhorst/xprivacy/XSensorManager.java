package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.hardware.Sensor;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XSensorManager extends XHook {
	private Methods mMethod;

	private XSensorManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.hardware.SensorManager";
	}

	// @formatter:off

	// public Sensor getDefaultSensor(int type)
	// public List<Sensor> getSensorList(int type)
	// frameworks/base/core/java/android/hardware/SensorManager.java
	
	// @formatter:on

	private enum Methods {
		getDefaultSensor, getSensorList
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XSensorManager(Methods.getDefaultSensor, PrivacyManager.cSensors));
		listHook.add(new XSensorManager(Methods.getSensorList, PrivacyManager.cSensors));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getDefaultSensor) {
			if (isRestricted(param))
				param.setResult(null);
		} else if (mMethod == Methods.getSensorList) {
			if (isRestricted(param))
				param.setResult(new ArrayList<Sensor>());
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

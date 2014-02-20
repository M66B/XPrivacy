package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.hardware.Sensor;
import android.util.Log;

public class XSensorManager extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XSensorManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public Sensor getDefaultSensor(int type)
	// public List<Sensor> getSensorList(int type)
	// frameworks/base/core/java/android/hardware/SensorManager.java
	// http://developer.android.com/reference/android/hardware/SensorManager.html
	
	// @formatter:on

	private enum Methods {
		getDefaultSensor, getSensorList
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XSensorManager(Methods.getDefaultSensor, PrivacyManager.cSensors, className));
		listHook.add(new XSensorManager(Methods.getSensorList, PrivacyManager.cSensors, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
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
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

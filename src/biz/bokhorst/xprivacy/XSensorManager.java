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
			else if (param.args.length > 0)
				if (isRestricted(param, (Integer) param.args[0]))
					param.setResult(null);

		} else if (mMethod == Methods.getSensorList) {
			if (isRestricted(param))
				param.setResult(new ArrayList<Sensor>());
			else if (param.args.length > 0)
				if (isRestricted(param, (Integer) param.args[0]))
					param.setResult(new ArrayList<Sensor>());

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getSensorList)
			if (param.getResult() != null && param.args.length > 0 && (Integer) param.args[0] == Sensor.TYPE_ALL) {
				List<Sensor> listSensor = new ArrayList<Sensor>();
				for (Sensor sensor : (List<Sensor>) param.getResult())
					if (!isRestricted(param, sensor.getType()))
						listSensor.add(sensor);
				param.setResult(listSensor);
			}
	}

	private boolean isRestricted(XParam param, int type) throws Throwable {
		if (type == Sensor.TYPE_ACCELEROMETER || type == Sensor.TYPE_LINEAR_ACCELERATION) {
			if (isRestricted(param, "acceleration"))
				return true;
		} else if (type == Sensor.TYPE_GRAVITY) {
			if (isRestricted(param, "gravity"))
				return true;
		} else if (type == Sensor.TYPE_RELATIVE_HUMIDITY) {
			if (isRestricted(param, "humidity"))
				return true;
		} else if (type == Sensor.TYPE_LIGHT) {
			if (isRestricted(param, "light"))
				return true;
		} else if (type == Sensor.TYPE_MAGNETIC_FIELD || type == Sensor.TYPE_MAGNETIC_FIELD_UNCALIBRATED) {
			if (isRestricted(param, "magnetic"))
				return true;
		} else if (type == Sensor.TYPE_SIGNIFICANT_MOTION) {
			if (isRestricted(param, "motion"))
				return true;
		} else if (type == Sensor.TYPE_GYROSCOPE || type == Sensor.TYPE_GYROSCOPE_UNCALIBRATED) {
			if (isRestricted(param, "orientation"))
				return true;
		} else if (type == Sensor.TYPE_PRESSURE) {
			if (isRestricted(param, "pressure"))
				return true;
		} else if (type == Sensor.TYPE_PROXIMITY) {
			if (isRestricted(param, "proximity"))
				return true;
		} else if (type == Sensor.TYPE_GAME_ROTATION_VECTOR || type == Sensor.TYPE_GEOMAGNETIC_ROTATION_VECTOR
				|| type == Sensor.TYPE_ROTATION_VECTOR) {
			if (isRestricted(param, "rotation"))
				return true;
		} else if (type == Sensor.TYPE_AMBIENT_TEMPERATURE) {
			if (isRestricted(param, "temperature"))
				return true;
		} else if (type == Sensor.TYPE_STEP_COUNTER || type == Sensor.TYPE_STEP_DETECTOR) {
			if (isRestricted(param, "step"))
				return true;
		}
		return false;
	}
}

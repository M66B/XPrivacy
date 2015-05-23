package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.hardware.Sensor;
import android.hardware.SensorManager;
import android.util.Log;

public class XSensorManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.hardware.SensorManager";

	private static final int cMaxRateUs = (int) (0.01 * 1000 * 1000); // 100 Hz

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
	// boolean registerListener(SensorEventListener listener, Sensor sensor, int rateUs, int maxBatchReportLatencyUs)
	// boolean registerListener(SensorEventListener listener, Sensor sensor, int rateUs, Handler handler)
	// boolean registerListener(SensorEventListener listener, Sensor sensor, int rateUs, int maxBatchReportLatencyUs, Handler handler)
	// boolean registerListener(SensorEventListener listener, Sensor sensor, int rateUs)
	// frameworks/base/core/java/android/hardware/SensorManager.java
	// http://developer.android.com/reference/android/hardware/SensorManager.html
	// http://developer.android.com/reference/android/hardware/Sensor.html
	
	// @formatter:on

	private enum Methods {
		getDefaultSensor, getSensorList, registerListener
	};

	public static List<XHook> getInstances(String className, boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			listHook.add(new XSensorManager(Methods.getDefaultSensor, PrivacyManager.cSensors, className));
			listHook.add(new XSensorManager(Methods.getSensorList, PrivacyManager.cSensors, className));
			listHook.add(new XSensorManager(Methods.registerListener, PrivacyManager.cSensors, className));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case getDefaultSensor:
			if (isRestricted(param))
				param.setResult(null);
			else if (param.args.length > 0 && param.args[0] instanceof Integer)
				if (isRestricted(param, (Integer) param.args[0]))
					param.setResult(null);
			break;

		case getSensorList:
			if (isRestricted(param))
				param.setResult(new ArrayList<Sensor>());
			else if (param.args.length > 0 && param.args[0] instanceof Integer)
				if (isRestricted(param, (Integer) param.args[0]))
					param.setResult(new ArrayList<Sensor>());
			break;

		case registerListener:
			if (param.args.length > 2 && param.args[1] instanceof Sensor && param.args[2] instanceof Integer) {
				int type = ((Sensor) param.args[1]).getType();
				if (type == Sensor.TYPE_GYROSCOPE || type == Sensor.TYPE_GYROSCOPE_UNCALIBRATED) {
					int rateUs = (Integer) param.args[2];

					// http://developer.android.com/guide/topics/sensors/sensors_overview.html
					if (rateUs == SensorManager.SENSOR_DELAY_NORMAL)
						return; // 200,000 us
					else if (rateUs == SensorManager.SENSOR_DELAY_UI)
						return; // 60,000 us
					else if (rateUs == SensorManager.SENSOR_DELAY_GAME)
						return; // 20,000 us
					else if (rateUs == SensorManager.SENSOR_DELAY_FASTEST)
						; // 0 us

					if (rateUs < cMaxRateUs) // 10,000 us
						if (isRestricted(param))
							param.args[2] = cMaxRateUs;
				}
			}
			break;
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getDefaultSensor:
		case registerListener:
			// Do nothing
			break;

		case getSensorList:
			if (param.getResult() != null && param.args.length > 0 && param.args[0] instanceof Integer)
				if ((Integer) param.args[0] == Sensor.TYPE_ALL) {
					List<Sensor> listSensor = new ArrayList<Sensor>();
					for (Sensor sensor : (List<Sensor>) param.getResult())
						if (!isRestricted(param, sensor.getType()))
							listSensor.add(sensor);
					param.setResult(listSensor);
				}
			break;
		}
	}

	@SuppressWarnings("deprecation")
	private boolean isRestricted(XParam param, int type) throws Throwable {
		if (type == Sensor.TYPE_ALL)
			return false;
		else if (type == Sensor.TYPE_ACCELEROMETER || type == Sensor.TYPE_LINEAR_ACCELERATION) {
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
		} else if (type == Sensor.TYPE_ORIENTATION || type == Sensor.TYPE_GYROSCOPE
				|| type == Sensor.TYPE_GYROSCOPE_UNCALIBRATED) {
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
		} else if (type == Sensor.TYPE_TEMPERATURE || type == Sensor.TYPE_AMBIENT_TEMPERATURE) {
			if (isRestricted(param, "temperature"))
				return true;
		} else if (type == Sensor.TYPE_STEP_COUNTER || type == Sensor.TYPE_STEP_DETECTOR) {
			if (isRestricted(param, "step"))
				return true;
		} else if (type == Sensor.TYPE_HEART_RATE) {
			if (isRestricted(param, "heartrate"))
				return true;
		} else if (type == 22) {
			// 22 = TYPE_TILT_DETECTOR
			// Do nothing
		} else if (type == 23 || type == 24 || type == 25) {
			// 23 = TYPE_WAKE_GESTURE
			// 24 = TYPE_GLANCE_GESTURE
			// 25 = TYPE_PICK_UP_GESTURE
			// 23/24 This sensor is expected to only be used by the system ui
			// 25 Expected to be used internally for always on display
		} else
			Util.log(this, Log.WARN, "Unknown sensor type=" + type);
		return false;
	}
}

package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.math.BigDecimal;

import android.content.Context;
import android.location.Location;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XLocationManager extends XHook {

	public XLocationManager(String methodName, String permissionName) {
		super(methodName, permissionName);
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (!methodName.equals("getLastKnownLocation"))
			if (!isAllowed(param))
				if (methodName.equals("addGpsStatusListener") || methodName.equals("addNmeaListener"))
					param.setResult(false);
				else
					param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("getLastKnownLocation"))
			if (param.getResultOrThrowable() != null)
				if (!isAllowed(param)) {
					String provider = (String) param.args[0];
					param.setResult(getRandomLocation(provider));
				}
	}

	@Override
	protected boolean isAllowed(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		return getAllowed(context, uid, true);
	}

	private Location getRandomLocation(String provider) {
		Location location = new Location(provider);
		location.setLatitude(getRandomLat());
		location.setLongitude(getRandomLon());
		return location;
	}

	private double getRandomLat() {
		double lat = Math.random() * 180;
		BigDecimal latitude = new BigDecimal(lat > 90 ? lat - 90 : -lat);
		return latitude.setScale(6, BigDecimal.ROUND_HALF_UP).doubleValue();
	}

	private double getRandomLon() {
		double lon = Math.random() * 360;
		BigDecimal longitude = new BigDecimal(lon > 180 ? lon - 180 : -lon);
		return longitude.setScale(6, BigDecimal.ROUND_HALF_UP).doubleValue();
	}
}

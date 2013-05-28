package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.content.Context;
import android.location.Location;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XGetLastKnownLocation extends XLocationManager {

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Get context
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);

		// Check if allowed
		if (!isAllowed(context, Binder.getCallingUid(), cPermissionName)) {
			Location location = (Location) param.getResult();
			if (location != null) {
				String provider = (String) param.args[0];
				location = getRandomLocation(provider);
				param.setResult(location);
			}
		}
	}
}

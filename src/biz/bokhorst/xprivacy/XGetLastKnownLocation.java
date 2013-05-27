package biz.bokhorst.xprivacy;

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
		// Log provider
		String provider = (String) param.args[0];
		info("provider=" + provider);

		// Check if allowed
		if (!isAllowed(Binder.getCallingUid(), cPermissionName)) {
			Location location = (Location) param.getResult();
			info("deny location=" + location);
			if (location != null) {
				location = getRandomLocation(provider);
				info("new=" + location);
				param.setResult(location);
			}
		}
	}
}

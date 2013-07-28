package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XLocationClient extends XHook {

	public XLocationClient(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "com.google.android.gms.location.LocationClient";
	}

	// void connect()
	// https://developer.android.com/reference/com/google/android/gms/location/LocationClient.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XLocationClient("connect", PrivacyManager.cLocation));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("connect")) {
			if (isRestricted(param))
				param.setResult(null);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XConnectionCallbacks extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XConnectionCallbacks(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), "GAC." + method.name());
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// abstract void onConnected(Bundle connectionHint)
	// https://developer.android.com/reference/com/google/android/gms/common/api/GoogleApiClient.ConnectionCallbacks.html
	
	// @formatter:on

	private enum Methods {
		onConnected
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		Util.log(null, Log.INFO, "Hooking class=" + className + " uid=" + Binder.getCallingUid());

		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XConnectionCallbacks(Methods.onConnected, null, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case onConnected:
			ClassLoader loader = param.thisObject.getClass().getClassLoader();
			Class<?> cLoc = Class.forName("com.google.android.gms.location.LocationServices", false, loader);
			Object fusedLocationApi = cLoc.getDeclaredField("FusedLocationApi").get(null);
			Util.log(this, Log.WARN, "FusedLocationApi class=" + fusedLocationApi.getClass());

			Class<?> cRec = Class.forName("com.google.android.gms.location.ActivityRecognition", false, loader);
			Object activityRecognitionApi = cRec.getDeclaredField("ActivityRecognitionApi").get(null);
			Util.log(this, Log.WARN, "ActivityRecognitionApi class=" + activityRecognitionApi.getClass());

			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

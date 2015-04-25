package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XConnectionCallbacks extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XConnectionCallbacks(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), "GMS5." + method.name());
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
			Util.log(this, Log.WARN, "GoogleApiClient onConnected uid=" + Binder.getCallingUid());
			ClassLoader loader = param.thisObject.getClass().getClassLoader();

			// FusedLocationApi
			try {
				Class<?> cLoc = Class.forName("com.google.android.gms.location.LocationServices", false, loader);
				Object fusedLocationApi = cLoc.getDeclaredField("FusedLocationApi").get(null);
				if (PrivacyManager.getTransient(fusedLocationApi.getClass().getName(), null) == null) {
					PrivacyManager.setTransient(fusedLocationApi.getClass().getName(), Boolean.toString(true));

					if (fusedLocationApi != null)
						XPrivacy.hookAll(XFusedLocationApi.getInstances(fusedLocationApi), loader, getSecret(), true);
				}
			} catch (ClassNotFoundException ex) {
				Util.log(this, Log.WARN, ex.toString());
			} catch (NoSuchFieldException ex) {
				Util.log(this, Log.WARN, ex.toString());
			} catch (ExceptionInInitializerError ex) {
				Util.log(this, Log.WARN, ex.toString());
			}

			// ActivityRecognitionApi
			try {
				Class<?> cRec = Class.forName("com.google.android.gms.location.ActivityRecognition", false, loader);
				Object activityRecognitionApi = cRec.getDeclaredField("ActivityRecognitionApi").get(null);
				if (PrivacyManager.getTransient(activityRecognitionApi.getClass().getName(), null) == null) {
					PrivacyManager.setTransient(activityRecognitionApi.getClass().getName(), Boolean.toString(true));

					if (activityRecognitionApi != null)
						XPrivacy.hookAll(XActivityRecognitionApi.getInstances(activityRecognitionApi), loader,
								getSecret(), true);
				}
			} catch (ClassNotFoundException ex) {
				Util.log(this, Log.WARN, ex.toString());
			} catch (NoSuchFieldException ex) {
				Util.log(this, Log.WARN, ex.toString());
			} catch (ExceptionInInitializerError ex) {
				Util.log(this, Log.WARN, ex.toString());
			}

			// AppIndexApi
			try {
				Class<?> cApp = Class.forName("com.google.android.gms.appindexing.AppIndex", false, loader);
				Object appIndexApi = cApp.getDeclaredField("AppIndexApi").get(null);
				if (PrivacyManager.getTransient(appIndexApi.getClass().getName(), null) == null) {
					PrivacyManager.setTransient(appIndexApi.getClass().getName(), Boolean.toString(true));

					if (appIndexApi != null)
						XPrivacy.hookAll(XAppIndexApi.getInstances(appIndexApi), loader, getSecret(), true);
				}
			} catch (ClassNotFoundException ex) {
				Util.log(this, Log.WARN, ex.toString());
			} catch (NoSuchFieldException ex) {
				Util.log(this, Log.WARN, ex.toString());
			} catch (ExceptionInInitializerError ex) {
				Util.log(this, Log.WARN, ex.toString());
			}

			// PlaceDetectionApi
			try {
				Class<?> cPlaces = Class.forName("com.google.android.gms.location.places.Places", false, loader);
				Object placeDetectionApi = cPlaces.getDeclaredField("PlaceDetectionApi").get(null);
				if (PrivacyManager.getTransient(placeDetectionApi.getClass().getName(), null) == null) {
					PrivacyManager.setTransient(placeDetectionApi.getClass().getName(), Boolean.toString(true));

					if (placeDetectionApi != null)
						XPrivacy.hookAll(XPlaceDetectionApi.getInstances(placeDetectionApi), loader, getSecret(), true);
				}
			} catch (ClassNotFoundException ex) {
				Util.log(this, Log.WARN, ex.toString());
			} catch (NoSuchFieldException ex) {
				Util.log(this, Log.WARN, ex.toString());
			} catch (ExceptionInInitializerError ex) {
				Util.log(this, Log.WARN, ex.toString());
			}

			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

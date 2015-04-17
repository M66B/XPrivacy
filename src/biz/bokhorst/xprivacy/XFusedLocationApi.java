package biz.bokhorst.xprivacy;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.app.PendingIntent;
import android.location.Location;
import android.os.Binder;
import android.util.Log;

public class XFusedLocationApi extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final Map<Object, Object> mMapProxy = new WeakHashMap<Object, Object>();

	private XFusedLocationApi(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), "GMS5." + method.name());
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// Location getLastLocation(GoogleApiClient client)
	// abstract PendingResult<Status> removeLocationUpdates(GoogleApiClient client, LocationListener listener)
	// abstract PendingResult<Status> removeLocationUpdates(GoogleApiClient client, PendingIntent callbackIntent)
	// abstract PendingResult<Status> requestLocationUpdates(GoogleApiClient client, LocationRequest request, LocationListener listener, Looper looper)
	// abstract PendingResult<Status> requestLocationUpdates(GoogleApiClient client, LocationRequest request, LocationListener listener)
	// abstract PendingResult<Status> requestLocationUpdates(GoogleApiClient client, LocationRequest request, PendingIntent callbackIntent)
	// https://developer.android.com/reference/com/google/android/gms/location/FusedLocationProviderApi.html
	
	// @formatter:on

	private enum Methods {
		getLastLocation, removeLocationUpdates, requestLocationUpdates
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		Util.log(null, Log.INFO, "Hooking FusedLocationApi class=" + className + " uid=" + Binder.getCallingUid());

		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XFusedLocationApi(Methods.getLastLocation, PrivacyManager.cLocation, className));
		listHook.add(new XFusedLocationApi(Methods.removeLocationUpdates, null, className));
		listHook.add(new XFusedLocationApi(Methods.requestLocationUpdates, PrivacyManager.cLocation, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case getLastLocation:
			// Do nothing
			break;

		case removeLocationUpdates:
			if (param.args.length > 1)
				if (param.args[1] instanceof PendingIntent) {
					if (isRestricted(param, PrivacyManager.cLocation, "GMS5.requestLocationUpdates"))
						param.setResult(XGoogleApiClient.getPendingResult(param.thisObject.getClass().getClassLoader()));
				} else
					synchronized (mMapProxy) {
						if (mMapProxy.containsKey(param.args[1]))
							param.args[1] = mMapProxy.get(param.args[1]);
					}
			break;

		case requestLocationUpdates:
			if (param.args.length > 2)
				if (isRestricted(param))
					if (param.args[2] instanceof PendingIntent)
						param.setResult(XGoogleApiClient.getPendingResult(param.thisObject.getClass().getClassLoader()));
					else if (param.thisObject != null && param.args[2] != null) {
						// Create proxy
						ClassLoader cl = param.thisObject.getClass().getClassLoader();
						Class<?> ll = Class.forName("com.google.android.gms.location.LocationListener", false, cl);
						InvocationHandler ih = new OnLocationChangedHandler(Binder.getCallingUid(), param.args[2]);
						Object proxy = Proxy.newProxyInstance(cl, new Class<?>[] { ll }, ih);

						// Use proxy
						synchronized (mMapProxy) {
							mMapProxy.put(param.args[2], proxy);
						}
						param.args[2] = proxy;
					}
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getLastLocation:
			Location location = (Location) param.getResult();
			if (location != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location));
			break;

		case removeLocationUpdates:
		case requestLocationUpdates:
			// Do nothing
			break;
		}
	}

	private class OnLocationChangedHandler implements InvocationHandler {
		private int mUid;
		private Object mTarget;

		public OnLocationChangedHandler(int uid, Object target) {
			mUid = uid;
			mTarget = target;
		}

		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if ("onLocationChanged".equals(method.getName()))
				args[0] = PrivacyManager.getDefacedLocation(mUid, (Location) args[0]);
			return method.invoke(mTarget, args);
		}
	}
}

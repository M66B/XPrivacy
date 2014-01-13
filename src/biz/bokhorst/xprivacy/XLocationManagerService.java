package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.content.Context;
import android.location.ILocationListener;
import android.location.Location;
import android.os.Binder;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XLocationManagerService extends XHook {
	private Methods mMethod;
	private static final Map<IBinder, XILocationListener> mListener = new WeakHashMap<IBinder, XILocationListener>();

	private XLocationManagerService(Methods method, String restrictionName) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()));
		mMethod = method;
	}

	private XLocationManagerService(Methods method, String restrictionName, int sdk) {
		super(restrictionName, method.name(), String.format("Srv.%s", method.name()), sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.server.LocationManagerService";
	}

	// @formatter:off

	// public boolean addGpsStatusListener(IGpsStatusListener listener, String packageName)
	// public Location getLastLocation(LocationRequest request, String packageName)
	// public List<String> getProviders(Criteria criteria, boolean enabledOnly)
	// public List<String> getAllProviders()
	// public String getBestProvider(Criteria criteria, boolean enabledOnly)
	// public boolean isProviderEnabled(String provider)
	// public void removeUpdates(ILocationListener listener, PendingIntent intent, String packageName)
	// public void requestGeofence(LocationRequest request, Geofence geofence, PendingIntent intent, String packageName)
	// public void requestLocationUpdates(LocationRequest request, ILocationListener listener, PendingIntent intent, String packageName)
	// public boolean sendExtraCommand(String provider, String command, Bundle extras)
	// frameworks/base/services/java/com/android/server/LocationManagerService.java
	// http://developer.android.com/reference/android/location/LocationManager.html
	
	// @formatter:on

	private enum Methods {
		addGpsStatusListener, getLastLocation, getProviders, getAllProviders, getBestProvider, isProviderEnabled, removeUpdates, requestGeofence, requestLocationUpdates, sendExtraCommand
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods loc : Methods.values())
			if (loc != Methods.requestGeofence)
				listHook.add(new XLocationManagerService(loc, PrivacyManager.cLocation));
		listHook.add(new XLocationManagerService(Methods.requestGeofence, PrivacyManager.cLocation,
				Build.VERSION_CODES.JELLY_BEAN_MR1));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.addGpsStatusListener) {
			if (isRestricted(param))
				param.setResult(false);
		} else if (mMethod == Methods.requestGeofence) {
			if (isRestricted(param))
				param.setResult(null);
		} else if (mMethod == Methods.removeUpdates) {
			if (isRestricted(param))
				removeLocationListener(param);
		} else if (mMethod == Methods.requestLocationUpdates) {
			if (isRestricted(param))
				replaceLocationListener(param);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod != Methods.addGpsStatusListener && mMethod != Methods.requestGeofence
				&& mMethod != Methods.removeUpdates && mMethod != Methods.requestLocationUpdates)
			if (mMethod == Methods.getLastLocation) {
				Location location = (Location) param.getResult();
				if (location != null)
					if (isRestricted(param))
						param.setResult(PrivacyManager.getDefacedLocation(Binder.getCallingUid(), location));
			} else if (mMethod == Methods.isProviderEnabled) {
				if (isRestricted(param))
					param.setResult(false);
			} else if (mMethod == Methods.getProviders || mMethod == Methods.getAllProviders) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new ArrayList<String>());
			} else if (mMethod == Methods.getBestProvider) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(null);
			} else if (mMethod == Methods.sendExtraCommand) {
				if (isRestricted(param))
					param.setResult(false);
			} else
				Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			// Not all location managers do have a context
		}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	private void replaceLocationListener(MethodHookParam param) throws Throwable {
		if (param.args.length > 1 && param.args[1] != null)
			if (param.args[1] instanceof ILocationListener) {
				// Replace
				ILocationListener listener = (ILocationListener) param.args[1];
				XILocationListener xListener = new XILocationListener(listener);
				synchronized (mListener) {
					mListener.put(listener.asBinder(), xListener);
					Util.log(this, Log.WARN, "Added class=" + listener.getClass().getName() + " method=" + param.method
							+ " count=" + mListener.size() + " uid=" + Binder.getCallingUid());
				}
				param.args[1] = xListener;
			} else
				Util.log(this, Log.WARN, "Unexpected method=" + param.method + " uid=" + Binder.getCallingUid());
	}

	private void removeLocationListener(MethodHookParam param) {
		if (param.args.length > 0 && param.args[0] != null)
			if (param.args[0] instanceof ILocationListener) {
				ILocationListener listener = (ILocationListener) param.args[0];
				synchronized (mListener) {
					XILocationListener xlistener = mListener.get(listener.asBinder());
					if (xlistener == null)
						Util.log(this, Log.WARN, "Unknown class=" + listener.getClass().getName() + " method="
								+ param.method + " count=" + mListener.size() + " uid=" + Binder.getCallingUid());
					else {
						param.args[0] = xlistener;
						mListener.remove(listener.asBinder());
						Util.log(this, Log.WARN, "Removed class=" + listener.getClass().getName() + " method="
								+ param.method + " count=" + mListener.size() + " uid=" + Binder.getCallingUid());
					}
				}
			} else
				Util.log(this, Log.WARN, "Unexpected method=" + param.method + " uid=" + Binder.getCallingUid());
	}

	private class XILocationListener implements ILocationListener {

		private ILocationListener mLocationListener;

		public XILocationListener(ILocationListener locationListener) {
			mLocationListener = locationListener;
		}

		@Override
		public void onLocationChanged(Location location) {
			mLocationListener.onLocationChanged(location == null ? location : PrivacyManager.getDefacedLocation(
					Binder.getCallingUid(), location));
		}

		@Override
		public void onProviderDisabled(String provider) {
			mLocationListener.onProviderDisabled(provider);
		}

		@Override
		public void onProviderEnabled(String provider) {
			mLocationListener.onProviderEnabled(provider);
		}

		@Override
		public void onStatusChanged(String provider, int status, Bundle extras) {
			mLocationListener.onStatusChanged(provider, status, extras);
		}

		@Override
		public IBinder asBinder() {
			return mLocationListener.asBinder();
		}
	}
}

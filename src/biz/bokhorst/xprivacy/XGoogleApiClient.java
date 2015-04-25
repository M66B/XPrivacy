package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XGoogleApiClient extends XHook {
	private Methods mMethod;

	private XGoogleApiClient(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "GMS5." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "com.google.android.gms.common.api.GoogleApiClient$Builder";
	}

	// @formatter:off

	// GoogleApiClient.Builder addConnectionCallbacks(GoogleApiClient.ConnectionCallbacks listener)
	// https://developer.android.com/reference/com/google/android/gms/common/api/GoogleApiClient.Builder.html

	// https://developer.android.com/reference/com/google/android/gms/common/api/PendingResult.html
	// https://developer.android.com/reference/com/google/android/gms/common/api/Status.html
	// https://developer.android.com/reference/com/google/android/gms/common/api/ResultCallback.html

	// @formatter:on

	private enum Methods {
		addConnectionCallbacks
	};

	public static List<XHook> getInstances() {
		Util.log(null, Log.INFO, "Loaded GoogleApiClient$Builder uid=" + Binder.getCallingUid());
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XGoogleApiClient(Methods.addConnectionCallbacks, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case addConnectionCallbacks:
			if (param.args.length > 0 && param.args[0] != null) {
				Class<?> clazz = param.args[0].getClass();
				if (PrivacyManager.getTransient(clazz.getName(), null) == null) {
					PrivacyManager.setTransient(clazz.getName(), Boolean.toString(true));

					XPrivacy.hookAll(XConnectionCallbacks.getInstances(param.args[0]), clazz.getClassLoader(),
							getSecret(), true);
				}
			}
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}

	public static Object getPendingResult(ClassLoader loader) throws Throwable {
		InvocationHandler ih = new PendingResultHandler(loader);
		Class<?> pr = Class.forName("com.google.android.gms.common.api.PendingResult", false, loader);
		return Proxy.newProxyInstance(loader, new Class<?>[] { pr }, ih);
	}

	private static class PendingResultHandler implements InvocationHandler {
		private ClassLoader mLoader;
		private boolean mCancelled = false;

		public PendingResultHandler(ClassLoader loader) {
			mLoader = loader;
		}

		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if ("await".equals(method.getName())) {
				// abstract R await()
				// abstract R await(long time, TimeUnit units)
				return getStatus(mLoader);

			} else if ("cancel".equals(method.getName())) {
				// abstract void cancel()
				mCancelled = true;
				return null;

			} else if ("isCanceled".equals(method.getName())) {
				// abstract boolean isCanceled()
				return mCancelled;

			} else if ("setResultCallback".equals(method.getName())) {
				// abstract void setResultCallback(ResultCallback<R> callback,
				// long time, TimeUnit units)
				// abstract void setResultCallback(ResultCallback<R> callback)
				Object callback = args[0];
				if (callback != null) {
					// abstract void onResult(R result)
					Class<?> cStatus = Class.forName("com.google.android.gms.common.api.Status", false, mLoader);
					callback.getClass().getMethod("onResult", cStatus).invoke(callback, getStatus(mLoader));
				}
				return null;
			}

			return null;
		}
	}

	private static Object getStatus(ClassLoader loader) throws Throwable {
		// public com.google.android.gms.common.api.Status(int)
		Class<?> cStatus = Class.forName("com.google.android.gms.common.api.Status", false, loader);
		Constructor<?> iStatus = cStatus.getConstructor(int.class);
		Object status = iStatus.newInstance(0); // SUCCESS
		return status;
	}
}

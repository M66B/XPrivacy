package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XAppIndexApi extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XAppIndexApi(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), "GMS5." + method.name());
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// abstract PendingResult<Status> view(GoogleApiClient apiClient, Activity activity, Intent viewIntent, String title, Uri webUrl, List<AppIndexApi.AppIndexingLink> outLinks)
	// abstract PendingResult<Status> view(GoogleApiClient apiClient, Activity activity, Uri appIndexingUrl, String title, Uri webUrl, List<AppIndexApi.AppIndexingLink> outLinks)
	// abstract PendingResult<Status> viewEnd(GoogleApiClient apiClient, Activity activity, Uri appIndexingUrl)
	// abstract PendingResult<Status> viewEnd(GoogleApiClient apiClient, Activity activity, Intent viewIntent)
	// https://developer.android.com/reference/com/google/android/gms/appindexing/AppIndexApi.html
	
	// @formatter:on

	private enum Methods {
		view, viewEnd
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		Util.log(null, Log.INFO, "Hooking AppIndex class=" + className + " uid=" + Binder.getCallingUid());

		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XAppIndexApi(Methods.viewEnd, null, className));
		listHook.add(new XAppIndexApi(Methods.view, PrivacyManager.cView, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case viewEnd:
			if (isRestricted(param, PrivacyManager.cView, "GMS5.view"))
				param.setResult(XGoogleApiClient.getPendingResult(param.thisObject.getClass().getClassLoader()));
			break;

		case view:
			if (isRestricted(param))
				param.setResult(XGoogleApiClient.getPendingResult(param.thisObject.getClass().getClassLoader()));
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

public class XGoogleApiClient extends XHook {
	private Methods mMethod;

	private XGoogleApiClient(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "GAC." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "com.google.android.gms.common.api.GoogleApiClient$Builder";
	}

	// @formatter:off

	// GoogleApiClient.Builder addConnectionCallbacks(GoogleApiClient.ConnectionCallbacks listener)
	// https://developer.android.com/reference/com/google/android/gms/common/api/GoogleApiClient.Builder.html
	
	// @formatter:on

	private enum Methods {
		addConnectionCallbacks
	};

	public static List<XHook> getInstances() {
		Util.log(null, android.util.Log.WARN, "Loaded GAC");
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
							getSecret());
				}
			}
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

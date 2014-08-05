package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.net.Uri;
import android.provider.MediaStore;

public class XActivity extends XHook {
	private Methods mMethod;
	private String mActionName;

	private XActivity(Methods method, String restrictionName, String actionName) {
		super(restrictionName, method.name(), actionName);
		mMethod = method;
		mActionName = actionName;
	}

	public String getClassName() {
		return "android.app.Activity";
	}

	// @formatter:off

	// public Object getSystemService(String name)
	// public void startActivities(Intent[] intents)
	// public void startActivities(Intent[] intents, Bundle options)
	// public void startActivity(Intent intent)
	// public void startActivity(Intent intent, Bundle options)
	// public void startActivityForResult(Intent intent, int requestCode)
	// public void startActivityForResult(Intent intent, int requestCode, Bundle options)
	// public void startActivityFromChild(Activity child, Intent intent, int requestCode)
	// public void startActivityFromChild(Activity child, Intent intent, int requestCode, Bundle options)
	// public void startActivityFromFragment(Fragment fragment, Intent intent, int requestCode)
	// public void startActivityFromFragment(Fragment fragment, Intent intent, int requestCode, Bundle options)
	// public boolean startActivityIfNeeded(Intent intent, int requestCode)
	// public boolean startActivityIfNeeded(Intent intent, int requestCode, Bundle options)
	// public boolean startNextMatchingActivity(Intent intent)
	// public boolean startNextMatchingActivity(Intent intent, Bundle options)
	// frameworks/base/core/java/android/app/Activity.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		getSystemService,
		startActivities, startActivity, startActivityForResult, startActivityFromChild, startActivityFromFragment, startActivityIfNeeded, startNextMatchingActivity
	};
	// @formatter:on

	@SuppressLint("InlinedApi")
	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XActivity(Methods.getSystemService, null, null));

		List<Methods> startMethods = new ArrayList<Methods>(Arrays.asList(Methods.values()));
		startMethods.remove(Methods.getSystemService);

		// Intent send: browser
		for (Methods activity : startMethods)
			listHook.add(new XActivity(activity, PrivacyManager.cView, Intent.ACTION_VIEW));

		// Intent send: call/dial
		for (Methods activity : startMethods) {
			listHook.add(new XActivity(activity, PrivacyManager.cCalling, Intent.ACTION_CALL));
			listHook.add(new XActivity(activity, PrivacyManager.cCalling, Intent.ACTION_DIAL));
		}

		// Intent send: media
		for (Methods activity : startMethods) {
			listHook.add(new XActivity(activity, PrivacyManager.cMedia, MediaStore.ACTION_IMAGE_CAPTURE));
			listHook.add(new XActivity(activity, PrivacyManager.cMedia, MediaStore.ACTION_IMAGE_CAPTURE_SECURE));
			listHook.add(new XActivity(activity, PrivacyManager.cMedia, MediaStore.ACTION_VIDEO_CAPTURE));
		}

		return listHook;
	}

	@Override
	@SuppressLint("DefaultLocale")
	protected void before(XParam param) throws Throwable {
		// Get intent(s)
		Intent[] intents = null;
		switch (mMethod) {
		case getSystemService:
			// Do nothing
			break;

		case startActivity:
		case startActivityForResult:
		case startActivityIfNeeded:
		case startNextMatchingActivity:
			if (param.args.length > 0 && param.args[0] instanceof Intent)
				intents = new Intent[] { (Intent) param.args[0] };
			break;

		case startActivityFromChild:
		case startActivityFromFragment:
			if (param.args.length > 1 && param.args[1] instanceof Intent)
				intents = new Intent[] { (Intent) param.args[1] };
			break;

		case startActivities:
			if (param.args.length > 0 && param.args[0] instanceof Intent[])
				intents = (Intent[]) param.args[0];
			break;
		}

		// Process intent(s)
		if (intents != null)
			for (Intent intent : intents)
				if (mActionName.equals(intent.getAction())) {
					boolean restricted = false;
					if (mActionName.equals(Intent.ACTION_VIEW)) {
						Uri uri = intent.getData();
						if (uri != null)
							if (isRestrictedExtra(param, mActionName, uri.toString()))
								restricted = true;
					} else
						restricted = isRestricted(param, mActionName);

					if (restricted) {
						if (mMethod == Methods.startActivityIfNeeded)
							param.setResult(true);
						else
							param.setResult(null);
						return;
					}
				}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getSystemService)
			if (param.args.length > 0 && param.args[0] instanceof String && param.getResult() != null) {
				String name = (String) param.args[0];
				Object instance = param.getResult();
				XPrivacy.handleGetSystemService(name, instance.getClass().getName(), getSecret());
			}
	}
}

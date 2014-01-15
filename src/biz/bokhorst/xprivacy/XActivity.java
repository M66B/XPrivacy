package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.provider.MediaStore;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivity extends XHook {
	private Methods mMethod;
	private String mActionName;

	private XActivity(Methods method, String restrictionName, String actionName) {
		super(restrictionName, method.name(), actionName);
		mMethod = method;
		mActionName = actionName;
	}

	private XActivity(Methods method, String restrictionName, String actionName, int sdk) {
		super(restrictionName, method.name(), actionName, sdk);
		mMethod = method;
		mActionName = actionName;
	}

	public String getClassName() {
		return "android.app.Activity";
	}

	@Override
	public boolean isVisible() {
		return !(mMethod == Methods.onDestroy || mMethod == Methods.onPause);
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
	// protected void onPause()
	// protected void onDestroy()
	// frameworks/base/core/java/android/app/Activity.java

	// @formatter:on

	private enum Methods {
		getSystemService, startActivities, startActivity, startActivityForResult, startActivityFromChild, startActivityFromFragment, startActivityIfNeeded, onPause, onDestroy
	};

	@SuppressLint("InlinedApi")
	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();

		listHook.add(new XActivity(Methods.getSystemService, null, null));
		listHook.add(new XActivity(Methods.onDestroy, null, null));
		listHook.add(new XActivity(Methods.onPause, null, null));

		List<Methods> startMethods = new ArrayList<Methods>(Arrays.asList(Methods.values()));
		startMethods.remove(Methods.getSystemService);
		startMethods.remove(Methods.onDestroy);
		startMethods.remove(Methods.onPause);

		// Intent send: browser
		for (Methods activity : startMethods)
			listHook.add(new XActivity(activity, PrivacyManager.cView, Intent.ACTION_VIEW));

		// Intent send: call
		for (Methods activity : startMethods)
			listHook.add(new XActivity(activity, PrivacyManager.cCalling, Intent.ACTION_CALL));

		// Intent send: media
		for (Methods activity : startMethods) {
			listHook.add(new XActivity(activity, PrivacyManager.cMedia, MediaStore.ACTION_IMAGE_CAPTURE));
			listHook.add(new XActivity(activity, PrivacyManager.cMedia, MediaStore.ACTION_IMAGE_CAPTURE_SECURE,
					Build.VERSION_CODES.JELLY_BEAN_MR1));
			listHook.add(new XActivity(activity, PrivacyManager.cMedia, MediaStore.ACTION_VIDEO_CAPTURE));
		}

		return listHook;
	}

	@Override
	@SuppressLint("DefaultLocale")
	protected void before(MethodHookParam param) throws Throwable {
		// Get intent(s)
		Intent[] intents = null;
		if (mMethod == Methods.getSystemService) {
			// Do nothing
		} else if (mMethod == Methods.onDestroy || mMethod == Methods.onPause) {
			// Do nothing
		} else if (mMethod == Methods.startActivity || mMethod == Methods.startActivityForResult
				|| mMethod == Methods.startActivityIfNeeded) {
			if (param.args.length > 0 && param.args[0] != null)
				intents = new Intent[] { (Intent) param.args[0] };
		} else if (mMethod == Methods.startActivityFromChild || mMethod == Methods.startActivityFromFragment) {
			if (param.args.length > 1 && param.args[1] != null)
				intents = new Intent[] { (Intent) param.args[1] };
		} else if (mMethod == Methods.startActivities) {
			if (param.args.length > 0 && param.args[0] != null)
				intents = (Intent[]) param.args[0];
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());

		// Process intent(s)
		if (intents != null)
			for (Intent intent : intents)
				if (mActionName.equals(intent.getAction())) {
					boolean restricted = false;
					if (mActionName.equals(Intent.ACTION_VIEW)) {
						Uri uri = intent.getData();
						if (uri != null) {
							String scheme = uri.getScheme();
							if (scheme != null) {
								scheme = scheme.toLowerCase();
								if (scheme.equals("http") || scheme.equals("https"))
									if (isRestricted(param, mActionName))
										restricted = true;
							}
						}
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
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getSystemService) {
			// TODO: check if this hook is needed
			if (param.args.length > 0 && param.args[0] != null) {
				String name = (String) param.args[0];
				Object instance = param.getResult();
				if (name != null && instance != null)
					XPrivacy.handleGetSystemService(this, name, instance);
			}
		} else if (mMethod == Methods.onDestroy || mMethod == Methods.onPause) {
			// TODO: remove after implementing privacy service
			try {
				PrivacyManager.sendUsageData(this, (Context) param.thisObject);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		}
	}
}

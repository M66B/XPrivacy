package biz.bokhorst.xprivacy;

import android.content.Intent;
import android.widget.Toast;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivity extends XHook {

	private String mActionName;

	// @formatter:off

	// public void startActivityForResult(Intent intent, int requestCode)
	// public void startActivityForResult(Intent intent, int requestCode, Bundle options)
	// frameworks/base/core/java/android/app/Activity.java

	// @formatter:on

	public XActivity(String methodName, String restrictionName, String[] permissions, String actionName) {
		super(methodName, restrictionName, permissions);
		mActionName = actionName;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Get intent
		Intent intent = (Intent) param.args[0];

		// Process intent
		if (intent != null && mActionName.equals(intent.getAction()))
			if (isRestricted(param)) {
				param.setResult(null);
				// Let user know
				String format = XUtil.getXString(getApplicationContext(), R.string.msg_restricted);
				String text = String.format(format, mActionName);
				Toast toast = Toast.makeText(getApplicationContext(), text, Toast.LENGTH_LONG);
				toast.show();
			}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

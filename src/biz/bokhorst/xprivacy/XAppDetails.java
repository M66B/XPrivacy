package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.res.Resources;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ScrollView;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XAppDetails extends XHook {

	public XAppDetails(String methodName) {
		super(methodName, null, new String[0]);
	}

	// @formatter:off

	// packages/apps/Settings/res/layout/installed_app_details.xml
	// packages/apps/Settings/src/com/android/settings/applications/InstalledAppDetails.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Get app entry
		// CM10/CM10.1
		Field fieldAppEntry = findField(param.thisObject.getClass(), "mAppEntry");
		Object appEntry = fieldAppEntry.get(param.thisObject);
		if (appEntry == null) {
			warning("appEntry is null");
			return;
		}

		// Get app info
		// CM10/CM10.1
		Field fieldInfo = findField(appEntry.getClass(), "info");
		final ApplicationInfo appInfo = (ApplicationInfo) fieldInfo.get(appEntry);
		if (appInfo == null) {
			warning("appInfo is null");
			return;
		}

		// Check for Android
		if (appInfo.uid == XRestriction.cUidAndroid)
			return;

		// Check for self
		if (appInfo.packageName.equals(XAppDetails.class.getPackage().getName()))
			return;

		// Get root view
		// CM10/CM10.1
		Field fieldRootView = findField(param.thisObject.getClass(), "mRootView");
		ScrollView rootView = (ScrollView) fieldRootView.get(param.thisObject);
		if (rootView == null) {
			warning("rootView is null");
			return;
		}

		// Get container view
		final LinearLayout containerView = (LinearLayout) ((ViewGroup) rootView).getChildAt(0);
		if (containerView == null) {
			warning("containerView is null");
			return;
		}

		// Get details view
		final LinearLayout detailsView = (LinearLayout) ((ViewGroup) containerView).getChildAt(0);
		if (detailsView == null) {
			warning("detailsView is null");
			return;
		}

		// Get helpers
		final Context xContext = XUtil.getXContext(rootView.getContext());
		Resources xResources = XUtil.getXResources(rootView.getContext());
		LayoutInflater inflater = (LayoutInflater) xContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);

		// Remove privacy button
		LinearLayout privacyButtonView = (LinearLayout) detailsView.findViewById(R.id.vwXPrivacyButton);
		if (privacyButtonView != null)
			detailsView.removeView(privacyButtonView);

		// Add privacy button
		privacyButtonView = (LinearLayout) inflater.inflate(xResources.getLayout(R.layout.xappbutton), null);
		Button btnXPrivacy = (Button) privacyButtonView.findViewById(R.id.btnXPrivacy);
		btnXPrivacy.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				Intent intentSettings = new Intent(xContext, XAppEdit.class);
				intentSettings.putExtra(XAppEdit.cPackageName, appInfo.packageName);
				intentSettings.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
				xContext.startActivity(intentSettings);
			}
		});
		detailsView.addView(privacyButtonView);
		detailsView.invalidate();
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		return true;
	}
}

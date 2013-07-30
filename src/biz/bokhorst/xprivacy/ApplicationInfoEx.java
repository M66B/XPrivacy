package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.annotation.SuppressLint;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.util.SparseArray;

@SuppressLint("DefaultLocale")
public class ApplicationInfoEx implements Comparable<ApplicationInfoEx> {
	private List<String> mListApplicationName;
	private String mPackageName;
	private boolean mHasInternet;
	private boolean mIsFrozen;
	private int mUid;
	private String mVersion;
	private boolean mSystem;
	private boolean mInstalled;
	private Drawable mIcon;

	public ApplicationInfoEx(Context context, String packageName) {
		// Get app info
		try {
			ApplicationInfo appInfo = context.getPackageManager().getApplicationInfo(packageName, 0);
			this.Initialize(context, appInfo);
		} catch (NameNotFoundException ex) {
			mInstalled = false;
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return;
		}
	}

	private ApplicationInfoEx(Context context, ApplicationInfo appInfo) {
		this.Initialize(context, appInfo);
	}

	private void Initialize(Context context, ApplicationInfo appInfo) {
		PackageManager pm = context.getPackageManager();
		mListApplicationName = new ArrayList<String>();
		mListApplicationName.add(getApplicationName(appInfo, pm));
		mPackageName = appInfo.packageName;
		mHasInternet = PrivacyManager.hasInternet(context, appInfo.packageName);
		mUid = appInfo.uid;

		// Get version
		try {
			mVersion = pm.getPackageInfo(appInfo.packageName, 0).versionName;
			mInstalled = true;
		} catch (NameNotFoundException ex) {
			mInstalled = false;
		} catch (Throwable ex) {
			mInstalled = false;
			Util.bug(null, ex);
		}

		// Get if system application
		mSystem = ((appInfo.flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0);
		mSystem = mSystem || appInfo.packageName.equals(ApplicationInfoEx.class.getPackage().getName());

		// Get if frozen (not enabled)
		int setting = pm.getApplicationEnabledSetting(appInfo.packageName);
		boolean enabled = (setting == PackageManager.COMPONENT_ENABLED_STATE_DEFAULT);
		enabled = (enabled || setting == PackageManager.COMPONENT_ENABLED_STATE_ENABLED);
		mIsFrozen = !enabled;

		// Get icon
		try {
			mIcon = pm.getApplicationInfo(mPackageName, 0).loadIcon(pm);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	public static List<ApplicationInfoEx> getXApplicationList(Context context, ProgressDialog dialog) {
		// Get references
		PackageManager pm = context.getPackageManager();
		boolean fSystem = PrivacyManager.getSettingBool(null, context, PrivacyManager.cSettingFSystem, true, false);

		// Get app list
		SparseArray<ApplicationInfoEx> mapApp = new SparseArray<ApplicationInfoEx>();
		List<ApplicationInfoEx> listApp = new ArrayList<ApplicationInfoEx>();
		List<ApplicationInfo> listAppInfo = pm.getInstalledApplications(PackageManager.GET_META_DATA);
		dialog.setMax(listAppInfo.size());
		for (int app = 0; app < listAppInfo.size(); app++)
			try {
				dialog.setProgress(app + 1);
				ApplicationInfoEx xAppInfo = new ApplicationInfoEx(context, listAppInfo.get(app));
				if (fSystem ? !(xAppInfo.isFrozen() || xAppInfo.getIsSystem()) : true) {
					ApplicationInfoEx yAppInfo = mapApp.get(xAppInfo.getUid());
					if (yAppInfo == null) {
						mapApp.put(xAppInfo.getUid(), xAppInfo);
						listApp.add(xAppInfo);
					} else
						yAppInfo.AddApplicationName(getApplicationName(listAppInfo.get(app), pm));
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		// Sort result
		Collections.sort(listApp);
		return listApp;
	}

	private static String getApplicationName(ApplicationInfo appInfo, PackageManager pm) {
		return (String) pm.getApplicationLabel(appInfo);
	}

	private void AddApplicationName(String Name) {
		mListApplicationName.add(Name);
	}

	public String getPackageName() {
		return mPackageName;
	}

	public Drawable getIcon() {
		return mIcon;
	}

	public boolean hasInternet() {
		return mHasInternet;
	}

	public boolean isFrozen() {
		return mIsFrozen;
	}

	public int getUid() {
		return mUid;
	}

	public String getVersion() {
		return mVersion;
	}

	public boolean getIsSystem() {
		return mSystem;
	}

	public boolean getIsInstalled() {
		return mInstalled;
	}

	@Override
	public String toString() {
		return String.format("%d %s", mUid, getApplicationNames());
	}

	@Override
	public int compareTo(ApplicationInfoEx other) {
		return getApplicationNames().compareToIgnoreCase(other.getApplicationNames());
	}

	private String getApplicationNames() {
		return TextUtils.join(", ", mListApplicationName);
	}
}

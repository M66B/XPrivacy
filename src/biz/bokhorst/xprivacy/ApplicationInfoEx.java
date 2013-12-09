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
	private ApplicationInfo mAppInfo;
	private List<String> mListApplicationName = null;

	// Cache
	private Drawable mIcon = null;
	private boolean mInternet;
	private boolean mInternetDetermined = false;
	private boolean mFrozen;
	private boolean mFrozenDetermined = false;
	private String mVersion = null;

	public ApplicationInfoEx(Context context, String packageName) throws NameNotFoundException {
		ApplicationInfo appInfo = context.getPackageManager().getApplicationInfo(packageName, 0);
		this.Initialize(context, appInfo);
	}

	private ApplicationInfoEx(Context context, ApplicationInfo appInfo) {
		this.Initialize(context, appInfo);
	}

	private void Initialize(Context context, ApplicationInfo appInfo) {
		mAppInfo = appInfo;
		PackageManager pm = context.getPackageManager();
		mListApplicationName = new ArrayList<String>();
		mListApplicationName.add((String) pm.getApplicationLabel(appInfo));
	}

	public static List<ApplicationInfoEx> getXApplicationList(Context context, ProgressDialog dialog) {
		// Get references
		PackageManager pm = context.getPackageManager();

		// Get app list
		SparseArray<ApplicationInfoEx> mapApp = new SparseArray<ApplicationInfoEx>();
		List<ApplicationInfoEx> listApp = new ArrayList<ApplicationInfoEx>();
		List<ApplicationInfo> listAppInfo = pm.getInstalledApplications(PackageManager.GET_META_DATA);
		if (dialog != null)
			dialog.setMax(listAppInfo.size());
		for (int app = 0; app < listAppInfo.size(); app++)
			try {
				if (dialog != null)
					dialog.setProgress(app + 1);
				ApplicationInfoEx xAppInfo = new ApplicationInfoEx(context, listAppInfo.get(app));
				ApplicationInfoEx yAppInfo = mapApp.get(xAppInfo.getUid());
				if (yAppInfo == null) {
					mapApp.put(xAppInfo.getUid(), xAppInfo);
					listApp.add(xAppInfo);
				} else
					yAppInfo.addApplicationName((String) pm.getApplicationLabel(listAppInfo.get(app)));
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		// Sort result
		Collections.sort(listApp);
		return listApp;
	}

	private void addApplicationName(String Name) {
		mListApplicationName.add(Name);
		Collections.sort(mListApplicationName);
	}

	public String getFirstApplicationName() {
		return (mListApplicationName.size() > 0 ? mListApplicationName.get(0) : null);
	}

	public String getPackageName() {
		return mAppInfo.packageName;
	}

	public Drawable getIcon(Context context) {
		if (mIcon == null)
			mIcon = mAppInfo.loadIcon(context.getPackageManager());
		return mIcon;
	}

	public boolean hasInternet(Context context) {
		if (!mInternetDetermined) {
			mInternet = PrivacyManager.hasInternet(context, mAppInfo.packageName);
			mInternetDetermined = true;
		}
		return mInternet;
	}

	public boolean isFrozen(Context context) {
		if (!mFrozenDetermined) {
			int setting = context.getPackageManager().getApplicationEnabledSetting(mAppInfo.packageName);
			boolean enabled = (setting == PackageManager.COMPONENT_ENABLED_STATE_DEFAULT);
			enabled = (enabled || setting == PackageManager.COMPONENT_ENABLED_STATE_ENABLED);
			mFrozen = !enabled;
			mFrozenDetermined = true;
		}
		return mFrozen;
	}

	public int getUid() {
		return mAppInfo.uid;
	}

	public String getVersion(Context context) {
		if (mVersion == null)
			try {
				mVersion = context.getPackageManager().getPackageInfo(mAppInfo.packageName, 0).versionName;
			} catch (Throwable ignored) {

			}
		return mVersion;
	}

	public boolean isSystem() {
		boolean mSystem = ((mAppInfo.flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0);
		mSystem = mSystem || mAppInfo.packageName.equals(this.getClass().getPackage().getName());
		mSystem = mSystem || mAppInfo.packageName.equals(this.getClass().getPackage().getName() + ".pro");
		mSystem = mSystem || mAppInfo.packageName.equals("de.robv.android.xposed.installer");
		return mSystem;
	}

	public boolean isShared() {
		return PrivacyManager.isShared(mAppInfo.uid);
	}

	public boolean isIsolated() {
		return PrivacyManager.isIsolated(mAppInfo.uid);
	}

	@Override
	public String toString() {
		return String.format("%d %s", mAppInfo.uid, getApplicationNames());
	}

	@Override
	public int compareTo(ApplicationInfoEx other) {
		return getApplicationNames().compareToIgnoreCase(other.getApplicationNames());
	}

	private String getApplicationNames() {
		return TextUtils.join(", ", mListApplicationName);
	}
}

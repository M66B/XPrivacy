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
	private List<ApplicationInfo> mListAppInfo = null;
	private List<String> mListApplicationName = null;
	private List<String> mListPackageName = null;
	private List<String> mVersion = null;

	// Cache
	private Drawable mIcon = null;
	private boolean mInternet;
	private boolean mInternetDetermined = false;
	private boolean mFrozen;
	private boolean mFrozenDetermined = false;

	public ApplicationInfoEx(Context context, int uid) throws NameNotFoundException {
		mListAppInfo = new ArrayList<ApplicationInfo>();
		mListApplicationName = new ArrayList<String>();
		PackageManager pm = context.getPackageManager();
		for (String packageName : pm.getPackagesForUid(uid)) {
			ApplicationInfo appInfo = pm.getApplicationInfo(packageName, 0);
			mListAppInfo.add(appInfo);
			mListApplicationName.add(pm.getApplicationLabel(appInfo).toString());
		}
		Collections.sort(mListApplicationName);
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
				ApplicationInfoEx appInfo = new ApplicationInfoEx(context, listAppInfo.get(app).uid);
				if (mapApp.get(appInfo.getUid()) == null) {
					mapApp.put(appInfo.getUid(), appInfo);
					listApp.add(appInfo);
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		// Sort result
		Collections.sort(listApp);
		return listApp;
	}

	public List<String> getApplicationName() {
		return mListApplicationName;
	}

	public List<String> getPackageName() {
		if (mListPackageName == null) {
			mListPackageName = new ArrayList<String>();
			for (ApplicationInfo appInfo : mListAppInfo)
				mListPackageName.add(appInfo.packageName);
			Collections.sort(mListPackageName);
		}
		return mListPackageName;
	}

	public List<String> getPackageVersion(Context context) {
		if (mVersion == null) {
			mVersion = new ArrayList<String>();
			PackageManager pm = context.getPackageManager();
			for (String packageName : this.getPackageName())
				try {
					mVersion.add(pm.getPackageInfo(packageName, 0).versionName);
				} catch (Throwable ignored) {
					mVersion.add("");
				}
		}
		return mVersion;
	}

	public String getVersion(Context context) {
		List<String> listVersion = new ArrayList<String>();
		for (String version : this.getPackageVersion(context))
			if (!listVersion.contains(version))
				listVersion.add(version);
		Collections.sort(listVersion);
		return TextUtils.join(",  ", listVersion);
	}

	public Drawable getIcon(Context context) {
		if (mIcon == null)
			mIcon = mListAppInfo.get(0).loadIcon(context.getPackageManager());
		return mIcon;
	}

	public boolean hasInternet(Context context) {
		if (!mInternetDetermined) {
			mInternet = PrivacyManager.hasInternet(context, mListAppInfo.get(0).packageName);
			mInternetDetermined = true;
		}
		return mInternet;
	}

	public boolean isFrozen(Context context) {
		if (!mFrozenDetermined) {
			int setting = context.getPackageManager().getApplicationEnabledSetting(mListAppInfo.get(0).packageName);
			boolean enabled = (setting == PackageManager.COMPONENT_ENABLED_STATE_DEFAULT);
			enabled = (enabled || setting == PackageManager.COMPONENT_ENABLED_STATE_ENABLED);
			mFrozen = !enabled;
			mFrozenDetermined = true;
		}
		return mFrozen;
	}

	public int getUid() {
		return mListAppInfo.get(0).uid;
	}

	public boolean isSystem() {
		boolean mSystem = ((mListAppInfo.get(0).flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0);
		mSystem = mSystem || mListAppInfo.get(0).packageName.equals(this.getClass().getPackage().getName());
		mSystem = mSystem || mListAppInfo.get(0).packageName.equals(this.getClass().getPackage().getName() + ".pro");
		mSystem = mSystem || mListAppInfo.get(0).packageName.equals("de.robv.android.xposed.installer");
		return mSystem;
	}

	public boolean isShared() {
		return PrivacyManager.isShared(mListAppInfo.get(0).uid);
	}

	public boolean isIsolated() {
		return PrivacyManager.isIsolated(mListAppInfo.get(0).uid);
	}

	@Override
	public String toString() {
		return String.format("%d %s", mListAppInfo.get(0).uid, TextUtils.join(", ", getApplicationName()));
	}

	@Override
	public int compareTo(ApplicationInfoEx other) {
		return TextUtils.join(", ", getApplicationName()).compareToIgnoreCase(
				TextUtils.join(", ", other.getApplicationName()));
	}
}

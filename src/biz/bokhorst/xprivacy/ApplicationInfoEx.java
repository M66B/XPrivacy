package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.util.SparseArray;

public class ApplicationInfoEx implements Comparable<ApplicationInfoEx> {
	private Drawable mDrawable;
	private List<String> mListApplicationName;
	private String mPackageName;
	private boolean mHasInternet;
	private int mUid;
	private String mVersion;
	private boolean mSystem;
	private boolean mInstalled;

	public ApplicationInfoEx(String packageName, Context context) {
		// Get app info
		try {
			ApplicationInfo appInfo = context.getPackageManager().getApplicationInfo(packageName, 0);
			this.Initialize(appInfo, context);
		} catch (NameNotFoundException ex) {
			mInstalled = false;
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return;
		}
	}

	private ApplicationInfoEx(ApplicationInfo appInfo, Context context) {
		this.Initialize(appInfo, context);
	}

	private void Initialize(ApplicationInfo appInfo, Context context) {
		PackageManager pm = context.getPackageManager();
		mDrawable = appInfo.loadIcon(pm);
		mListApplicationName = new ArrayList<String>();
		mListApplicationName.add(getApplicationName(appInfo, pm));
		mPackageName = appInfo.packageName;
		mHasInternet = PrivacyManager.hasInternet(context, appInfo.packageName);
		mUid = appInfo.uid;
		try {
			mVersion = pm.getPackageInfo(appInfo.packageName, 0).versionName;
			mInstalled = true;
		} catch (NameNotFoundException ex) {
			mInstalled = false;
		} catch (Throwable ex) {
			mInstalled = false;
			Util.bug(null, ex);
		}
		mSystem = ((appInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0);
		mSystem = mSystem || appInfo.packageName.equals(ApplicationInfoEx.class.getPackage().getName());
		mSystem = mSystem || appInfo.packageName.equals("de.robv.android.xposed.installer");
	}

	public static List<ApplicationInfoEx> getXApplicationList(Context context, ProgressDialog dialog) {
		// Get references
		PackageManager pm = context.getPackageManager();
		boolean expert = Boolean.parseBoolean(PrivacyManager.getSetting(null, context, PrivacyManager.cSettingExpert,
				Boolean.FALSE.toString(), false));

		// Get app list
		SparseArray<ApplicationInfoEx> mapApp = new SparseArray<ApplicationInfoEx>();
		List<ApplicationInfoEx> listApp = new ArrayList<ApplicationInfoEx>();
		List<ApplicationInfo> apps = pm.getInstalledApplications(PackageManager.GET_META_DATA);
		dialog.setMax(apps.size());
		int i = 1;
		for (ApplicationInfo appInfo : apps) {
		    dialog.setProgress(i++);
			ApplicationInfoEx xAppInfo = new ApplicationInfoEx(appInfo, context);
			if (xAppInfo.getIsSystem() ? expert : true) {
				ApplicationInfoEx yAppInfo = mapApp.get(appInfo.uid);
				if (yAppInfo == null) {
					mapApp.put(appInfo.uid, xAppInfo);
					listApp.add(xAppInfo);
				} else
					yAppInfo.AddApplicationName(getApplicationName(appInfo, pm));
			}
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

	public Drawable getDrawable() {
		return mDrawable;
	}

	public boolean hasInternet() {
		return mHasInternet;
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
		return String.format("%s", TextUtils.join(", ", mListApplicationName));
	}

	@Override
	public int compareTo(ApplicationInfoEx other) {
		return toString().compareToIgnoreCase(other.toString());
	}
}

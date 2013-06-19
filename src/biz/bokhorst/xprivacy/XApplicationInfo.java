package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.util.SparseArray;

public class XApplicationInfo implements Comparable<XApplicationInfo> {
	private Drawable mDrawable;
	private List<String> mListApplicationName;
	private String mPackageName;
	private boolean mHasInternet;
	private int mUid;
	private String mVersion;

	public XApplicationInfo(String packageName, Context context) {
		// Get app info
		try {
			ApplicationInfo appInfo = context.getPackageManager().getApplicationInfo(packageName, 0);
			this.Initialize(appInfo, context);
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
			return;
		}
	}

	private XApplicationInfo(ApplicationInfo appInfo, Context context) {
		this.Initialize(appInfo, context);
	}

	private void Initialize(ApplicationInfo appInfo, Context context) {
		PackageManager pm = context.getPackageManager();
		mDrawable = appInfo.loadIcon(pm);
		mListApplicationName = new ArrayList<String>();
		mListApplicationName.add(getApplicationName(appInfo, pm));
		mPackageName = appInfo.packageName;
		mHasInternet = XRestriction.hasInternet(context, appInfo.packageName);
		mUid = appInfo.uid;
		try {
			mVersion = pm.getPackageInfo(appInfo.packageName, 0).versionName;
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
		}
	}

	public static List<XApplicationInfo> getXApplicationList(Context context) {
		// Get references
		String self = XApplicationInfo.class.getPackage().getName();
		PackageManager pm = context.getPackageManager();
		boolean expert = Boolean.parseBoolean(XRestriction.getSetting(null, context, XRestriction.cSettingExpert,
				Boolean.FALSE.toString()));

		// Get app list
		SparseArray<XApplicationInfo> mapApp = new SparseArray<XApplicationInfo>();
		List<XApplicationInfo> listApp = new ArrayList<XApplicationInfo>();
		for (ApplicationInfo appInfo : pm.getInstalledApplications(PackageManager.GET_META_DATA)) {
			boolean system = ((appInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0);
			system = system || appInfo.packageName.equals(self);
			system = system || appInfo.packageName.equals("de.robv.android.xposed.installer");
			if (system ? expert : true) {
				XApplicationInfo xAppInfo = mapApp.get(appInfo.uid);
				if (xAppInfo == null) {
					xAppInfo = new XApplicationInfo(appInfo, context);
					mapApp.put(appInfo.uid, xAppInfo);
					listApp.add(xAppInfo);
				} else
					xAppInfo.AddApplicationName(getApplicationName(appInfo, pm));
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

	@Override
	@SuppressLint("DefaultLocale")
	public String toString() {
		return String.format("%s", TextUtils.join(", ", mListApplicationName));
	}

	@Override
	public int compareTo(XApplicationInfo other) {
		return toString().compareToIgnoreCase(other.toString());
	}
}

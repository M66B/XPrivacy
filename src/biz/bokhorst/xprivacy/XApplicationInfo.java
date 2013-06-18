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
	private boolean mIsUsed;
	private int mUid;

	private XApplicationInfo(ApplicationInfo appInfo, String restrictionName, Context context) {
		PackageManager pm = context.getPackageManager();
		mDrawable = appInfo.loadIcon(pm);
		mListApplicationName = new ArrayList<String>();
		mListApplicationName.add((String) pm.getApplicationLabel(appInfo));
		mPackageName = appInfo.packageName;
		mHasInternet = XRestriction.hasInternet(context, appInfo.packageName);
		if (restrictionName != null)
			mIsUsed = XRestriction.isUsed(context, appInfo.uid, restrictionName);
		mUid = appInfo.uid;
	}

	public static List<XApplicationInfo> getXApplicationList(Context context, String restrictionName) {
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
					xAppInfo = new XApplicationInfo(appInfo, restrictionName, context);
					mapApp.put(appInfo.uid, xAppInfo);
					listApp.add(xAppInfo);
				} else
					xAppInfo.AddApplicationName((String) pm.getApplicationLabel(appInfo));
			}
		}

		// Sort result
		Collections.sort(listApp);
		return listApp;
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

	public boolean isUsed() {
		return mIsUsed;
	}

	public int getUid() {
		return mUid;
	}

	@Override
	@SuppressLint("DefaultLocale")
	public String toString() {
		return String.format("%s (%d)", TextUtils.join(", ", mListApplicationName), mUid);
	}

	@Override
	public int compareTo(XApplicationInfo other) {
		return toString().compareToIgnoreCase(other.toString());
	}
}

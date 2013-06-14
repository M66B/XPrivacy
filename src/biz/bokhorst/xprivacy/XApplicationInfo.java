package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;

public class XApplicationInfo implements Comparable<XApplicationInfo> {
	private Drawable mDrawable;
	private List<String> mListApplicationName;
	private String mPackageName;
	private boolean mHasInternet;
	private boolean mIsUsed;
	private int mUid;

	public XApplicationInfo(ApplicationInfo appInfo, String restrictionName, Context context) {
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

	public void AddApplicationName(String Name) {
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

package biz.bokhorst.xprivacy;

import android.annotation.SuppressLint;
import android.os.Build;

public class Hook implements Comparable<Hook> {
	private String mRestrictionName;
	private String mMethodName;
	private String[] mPermissions;
	private int mSdkFrom;
	private int mSdkTo = Integer.MAX_VALUE;
	private Version mFrom;
	private String mReplacedRestriction;
	private String mReplacedMethod;

	private boolean mDangerous = false;
	private boolean mRestart = false;
	private boolean mNoUsageData = false;
	private boolean mNoOnDemand = false;
	private String mWhitelist = null;
	private boolean mNotify = false;
	private int mAOSPSdk = 0;
	private int mNotAOSPSdk = 0;
	private boolean mUnsafe = false;
	private boolean mOptional = false;
	private boolean mObsolete = false;
	private String mAnnotation = null;

	public Hook(String restrictionName, String methodName) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
	}

	public Hook(String restrictionName, String methodName, String permissions, int sdk, String from, String replaces) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mPermissions = (permissions == null ? null : permissions.split(","));
		mSdkFrom = sdk;
		mFrom = (from == null ? null : new Version(from));
		if (replaces != null) {
			int slash = replaces.indexOf('/');
			if (slash > 0) {
				mReplacedRestriction = replaces.substring(0, slash);
				mReplacedMethod = replaces.substring(slash + 1);
				if ("".equals(mReplacedMethod))
					mReplacedMethod = methodName;
			} else {
				mReplacedRestriction = mRestrictionName;
				mReplacedMethod = replaces;
			}
		}
	}

	// Definitions

	public Hook to(int sdk) {
		mSdkTo = sdk;
		return this;
	}

	public Hook dangerous() {
		mDangerous = true;
		return this;
	}

	@SuppressLint("FieldGetter")
	public void toggleDangerous() {
		String name = String.format("%s.%s.%s", PrivacyManager.cSettingDangerous, this.getRestrictionName(),
				this.getName());
		PrivacyManager.setSetting(0, name, Boolean.toString(!isDangerous()));
	}

	public Hook restart() {
		mRestart = true;
		return this;
	}

	public Hook noUsageData() {
		mNoUsageData = true;
		return this;
	}

	public Hook noOnDemand() {
		mNoOnDemand = true;
		return this;
	}

	public Hook whitelist(String whitelist) {
		mWhitelist = whitelist;
		return this;
	}

	public Hook doNotify() {
		mNotify = true;
		return this;
	}

	public Hook AOSP(int sdk) {
		mAOSPSdk = sdk;
		return this;
	}

	public Hook notAOSP(int sdk) {
		mNotAOSPSdk = sdk;
		if (!PrivacyManager.cIPC.equals(mRestrictionName))
			mUnsafe = true;
		return this;
	}

	public Hook unsafe() {
		mUnsafe = true;
		return this;
	}

	protected Hook optional() {
		mOptional = true;
		return this;
	}

	protected Hook obsolete() {
		mObsolete = true;
		return this;
	}

	public void annotate(String text) {
		mAnnotation = text;
	}

	// Getters

	public String getRestrictionName() {
		return mRestrictionName;
	}

	public String getName() {
		return mMethodName;
	}

	public String[] getPermissions() {
		return mPermissions;
	}

	public boolean isAvailable() {
		if (mObsolete)
			return false;
		if (Build.VERSION.SDK_INT < mSdkFrom)
			return false;
		if (Build.VERSION.SDK_INT > mSdkTo)
			return false;
		if (mAOSPSdk > 0 && !isAOSP(mAOSPSdk))
			return false;
		if (mNotAOSPSdk > 0 && isAOSP(mNotAOSPSdk))
			return false;
		return true;
	}

	public static boolean isAOSP(int sdk) {
		if (!PrivacyManager.cVersion3)
			return false;
		if (Build.VERSION.SDK_INT >= sdk) {
			if ("true".equals(System.getenv("XPrivacy.AOSP")))
				return true;
			if (Build.DISPLAY == null || Build.HOST == null)
				return false;
			if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP)
				// @formatter:off
				return (
						isAOSP() ||
						isCyanogenMod() ||
						isOmni() ||
						isMIUI() ||
						isSlim() ||
						isParanoidAndroid() ||
						isCarbon() ||
						isDirtyUnicorns() ||
						isLiquidSmooth() ||
						isAndroidRevolutionHD() ||
						isMahdi() ||
						isOmega()
					);
			// @formatter:on
			else
				return isAOSP();
		} else
			return false;
	}

	public static boolean isAOSP() {
		return Build.HOST.endsWith(".google.com");
	}

	public static boolean isCyanogenMod() {
		return Build.HOST.equals("cyanogenmod") || Build.DISPLAY.startsWith("cm_");
	}

	public static boolean isOmni() {
		return Build.DISPLAY.startsWith("slim");
	}

	public static boolean isMIUI() {
		return Build.HOST.equals("xiaomi");
	}

	public static boolean isSlim() {
		return Build.DISPLAY.startsWith("omni");
	}

	public static boolean isParanoidAndroid() {
		return Build.HOST.startsWith("paranoid") || Build.DISPLAY.startsWith("pa_");
	}

	public static boolean isCarbon() {
		return Build.DISPLAY.startsWith("carbon");
	}

	public static boolean isDirtyUnicorns() {
		return Build.DISPLAY.startsWith("du_");
	}

	public static boolean isLiquidSmooth() {
		return Build.DISPLAY.startsWith("liquid_");
	}

	public static boolean isAndroidRevolutionHD() {
		return Build.DISPLAY.startsWith("Android Revolution HD");
	}

	public static boolean isMahdi() {
		return Build.HOST.startsWith("mahdi");
	}

	public static boolean isOmega() {
		return Build.DISPLAY.startsWith("Omega");
	}

	public Version getFrom() {
		return mFrom;
	}

	public String getReplacedRestriction() {
		return mReplacedRestriction;
	}

	public String getReplacedMethod() {
		return mReplacedMethod;
	}

	@SuppressLint("FieldGetter")
	public boolean isDangerous() {
		String name = String.format("%s.%s.%s", PrivacyManager.cSettingDangerous, this.getRestrictionName(),
				this.getName());
		return PrivacyManager.getSettingBool(0, name, mDangerous);
	}

	public boolean isDangerousDefined() {
		return mDangerous;
	}

	public boolean isRestartRequired() {
		return mRestart;
	}

	public boolean hasUsageData() {
		return !mNoUsageData;
	}

	public boolean canOnDemand() {
		return !mNoOnDemand;
	}

	public String whitelist() {
		return mWhitelist;
	}

	public boolean shouldNotify() {
		return mNotify;
	}

	public boolean isUnsafe() {
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
			return false;
		if (PrivacyManager.cVersion3)
			return mUnsafe;
		else
			return false;
	}

	public boolean isOptional() {
		return mOptional;
	}

	public String getAnnotation() {
		return mAnnotation;
	}

	// Comparison

	@Override
	public int hashCode() {
		return (mRestrictionName.hashCode() ^ mMethodName.hashCode());
	}

	@Override
	public boolean equals(Object obj) {
		Hook other = (Hook) obj;
		return (mRestrictionName.equals(other.mRestrictionName) && mMethodName.equals(other.mMethodName));
	}

	@Override
	public int compareTo(Hook another) {
		int x = mRestrictionName.compareTo(another.mRestrictionName);
		return (x == 0 ? mMethodName.compareTo(another.mMethodName) : x);
	}

	@Override
	public String toString() {
		return String.format("%s/%s", mRestrictionName, mMethodName);
	}
}

package biz.bokhorst.xprivacy;

import java.io.FileNotFoundException;

import android.os.Process;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XIoBridge extends XHook {

	private String mFileName;

	public XIoBridge(String methodName, String restrictionName, String fileName) {
		super(restrictionName, methodName, fileName);
		mFileName = fileName;
	}

	public String getClassName() {
		return "libcore.io.IoBridge";
	}

	// public static FileDescriptor open(String path, int flags)
	// libcore/luni/src/main/java/libcore/io/IoBridge.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.args.length > 0) {
			String fileName = (String) param.args[0];
			if (fileName != null && fileName.startsWith(mFileName)) {
				// /proc
				if (mFileName.equals("/proc")) {
					// Zygote, Android
					if (Process.myUid() <= 0 || Process.myUid() == PrivacyManager.cUidAndroid)
						return;

					// Facebook
					if (fileName.equals("/proc/self/cmdline"))
						return;

					// Backward compatibility
					Version sVersion = new Version(PrivacyManager.getSetting(this, null,
							PrivacyManager.cSettingVersion, "0.0", true));
					if (sVersion.compareTo(new Version("1.7")) < 0)
						return;
				}

				// Check if restricted
				if (isRestricted(param, mFileName))
					param.setThrowable(new FileNotFoundException());
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

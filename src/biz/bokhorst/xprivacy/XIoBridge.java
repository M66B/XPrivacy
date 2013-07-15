package biz.bokhorst.xprivacy;

import java.io.FileNotFoundException;

import android.os.Binder;
import android.os.Process;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XIoBridge extends XHook {

	private String mFileName;

	public XIoBridge(String methodName, String restrictionName, String[] permissions, String fileName) {
		super(methodName, restrictionName, permissions, fileName);
		mFileName = fileName;
	}

	// public static FileDescriptor open(String path, int flags)
	// libcore/luni/src/main/java/libcore/io/IoBridge.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.args.length > 0) {
			String fileName = (String) param.args[0];
			if (fileName != null && fileName.startsWith(mFileName)) {
				Util.log(this, Log.INFO, "File name=" + fileName);

				// /proc
				if (mFileName.equals("/proc")) {
					// Android
					if (Process.myUid() == PrivacyManager.cUidAndroid || Process.myUid() <= 0
							|| Binder.getCallingUid() <= 0)
						return;

					// Facebook
					if (fileName.equals("/proc/self/cmdline"))
						return;

					// Backward compatibility
					if (PrivacyManager.getSetting(this, null, PrivacyManager.cSettingVersion, null, true) == null)
						return;

				}

				// Check if restricted
				if (isRestricted(param, mFileName))
					param.setResult(new FileNotFoundException());
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

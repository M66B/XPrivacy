package biz.bokhorst.xprivacy;

import java.io.File;
import java.io.FileNotFoundException;

import android.os.Binder;
import android.os.Environment;
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
				Util.log(this, Log.INFO, "File name=" + fileName + " uid=" + Process.myUid());

				// /proc
				if (mFileName.equals("/proc")) {
					// Zygote, Android
					if (Process.myUid() <= 0 || Process.myUid() == PrivacyManager.cUidAndroid)
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
					if (getRestrictionName().equals(PrivacyManager.cIsolation))
						param.args[0] = fileName.replace(mFileName,
								Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator
										+ ".xprivacy" + File.separator + Binder.getCallingUid() + File.separator);
					else
						param.setResult(new FileNotFoundException());
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

package biz.bokhorst.xprivacy;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import android.os.Process;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XIoBridge extends XHook {

	private String mFileName;

	private XIoBridge(String methodName, String restrictionName, String fileName) {
		super(restrictionName, methodName, fileName);
		mFileName = fileName;
	}

	public String getClassName() {
		return "libcore.io.IoBridge";
	}

	// public static FileDescriptor open(String path, int flags)
	// libcore/luni/src/main/java/libcore/io/IoBridge.java

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XIoBridge("open", PrivacyManager.cIdentification, "/proc"));
		listHook.add(new XIoBridge("open", PrivacyManager.cIdentification, "/system/build.prop"));
		listHook.add(new XIoBridge("open", PrivacyManager.cIdentification, "/sys/block/.../cid"));
		listHook.add(new XIoBridge("open", PrivacyManager.cIdentification, "/sys/class/.../cid"));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.args.length > 0) {
			String fileName = (String) param.args[0];
			if (fileName != null && (fileName.startsWith(mFileName) || mFileName.contains("..."))) {
				// Zygote, Android
				if (Process.myUid() <= 0 || Util.getAppId(Process.myUid()) == PrivacyManager.cAndroidUid)
					return;

				// Allow command line
				if (mFileName.equals("/proc"))
					if (fileName.equals("/proc/self/cmdline"))
						return;

				// Check if restricted
				if (mFileName.contains("...")) {
					String[] component = mFileName.split("\\.\\.\\.");
					if (fileName.startsWith(component[0]) && fileName.endsWith(component[1]))
						if (isRestricted(param, mFileName))
							param.setThrowable(new FileNotFoundException());
				} else if (isRestricted(param, mFileName))
					param.setThrowable(new FileNotFoundException());
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

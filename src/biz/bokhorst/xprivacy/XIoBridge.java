package biz.bokhorst.xprivacy;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;

import android.annotation.SuppressLint;
import android.os.Process;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XIoBridge extends XHook {
	private Methods mMethod;
	private String mFileName;

	private XIoBridge(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mFileName = null;
	}

	private XIoBridge(Methods method, String restrictionName, String fileName) {
		super(restrictionName, method.name(), fileName);
		mMethod = method;
		mFileName = fileName;
	}

	public String getClassName() {
		return "libcore.io.IoBridge";
	}

	// @formatter:off

	// public static void connect(FileDescriptor fd, InetAddress inetAddress, int port) throws SocketException
	// public static void connect(FileDescriptor fd, InetAddress inetAddress, int port, int timeoutMs) throws SocketException, SocketTimeoutException
	// TODO: public static FileDescriptor socket(boolean stream)
	// public static FileDescriptor open(String path, int flags) throws FileNotFoundException
	// libcore/luni/src/main/java/libcore/io/IoBridge.java

	// @formatter:on

	private enum Methods {
		open, connect
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cStorage));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cIdentification, "/proc"));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cIdentification, "/system/build.prop"));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cIdentification, "/sys/block/.../cid"));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cIdentification, "/sys/class/.../cid"));
		listHook.add(new XIoBridge(Methods.connect, PrivacyManager.cInternet));
		return listHook;
	}

	@Override
	@SuppressLint("SdCardPath")
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.open) {
			if (param.args.length > 0 && param.args[0] != null) {
				String fileName = (String) param.args[0];
				if (mFileName == null) {
					if (fileName.startsWith("/sdcard") || fileName.startsWith(System.getenv("EXTERNAL_STORAGE"))
							|| fileName.startsWith(System.getenv("EMULATED_STORAGE_SOURCE"))
							|| fileName.startsWith(System.getenv("EMULATED_STORAGE_TARGET")))
						if (isRestrictedExtra(param, fileName))
							param.setThrowable(new FileNotFoundException("XPrivacy"));

				} else if (fileName.startsWith(mFileName) || mFileName.contains("...")) {
					// Zygote, Android
					if (Util.getAppId(Process.myUid()) == Process.SYSTEM_UID)
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
								param.setThrowable(new FileNotFoundException("XPrivacy"));

					} else if (mFileName.equals("/proc")) {
						if (isRestrictedExtra(param, mFileName, fileName))
							param.setThrowable(new FileNotFoundException("XPrivacy"));

					} else {
						if (isRestricted(param, mFileName))
							param.setThrowable(new FileNotFoundException("XPrivacy"));
					}
				}
			}

		} else if (mMethod == Methods.connect) {
			if (param.args.length > 2 && param.args[1] instanceof InetAddress) {
				InetAddress address = (InetAddress) param.args[1];
				int port = (Integer) param.args[2];
				if (isRestrictedExtra(param, address.toString() + ":" + port))
					param.setThrowable(new IOException("XPrivacy"));
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

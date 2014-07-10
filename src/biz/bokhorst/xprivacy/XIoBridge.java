package biz.bokhorst.xprivacy;

import java.io.FileNotFoundException;
import java.net.InetAddress;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.List;

import android.annotation.SuppressLint;
import android.os.Process;
import android.text.TextUtils;
import android.util.Log;

public class XIoBridge extends XHook {
	private Methods mMethod;
	private String mFileName;

	private static String mExternalStorage = null;
	private static String mEmulatedSource = null;
	private static String mEmulatedTarget = null;
	private static String mMediaStorage = null;

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
	// public static FileDescriptor open(String path, int flags) throws FileNotFoundException
	// public static FileDescriptor socket(boolean stream) throws SocketException
	// libcore/luni/src/main/java/libcore/io/IoBridge.java

	// @formatter:on

	private enum Methods {
		open, connect
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XIoBridge(Methods.connect, PrivacyManager.cInternet));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cStorage));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cIdentification, "/proc"));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cIdentification, "/system/build.prop"));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cIdentification, "/sys/block/.../cid"));
		listHook.add(new XIoBridge(Methods.open, PrivacyManager.cIdentification, "/sys/class/.../cid"));
		return listHook;
	}

	@Override
	@SuppressLint("SdCardPath")
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.connect) {
			if (param.args.length > 2 && param.args[1] instanceof InetAddress && param.args[2] instanceof Integer) {
				InetAddress address = (InetAddress) param.args[1];
				int port = (Integer) param.args[2];
				String hostName;
				try {
					hostName = address.getHostName();
				} catch (Throwable ignored) {
					hostName = address.toString();
				}
				if (isRestrictedExtra(param, hostName + ":" + port))
					param.setThrowable(new SocketException("XPrivacy"));
			}

		} else if (mMethod == Methods.open) {
			if (param.args.length > 0) {
				String fileName = (String) param.args[0];
				if (mFileName == null && fileName != null) {
					// Get storage folders
					if (mExternalStorage == null) {
						mExternalStorage = System.getenv("EXTERNAL_STORAGE");
						mEmulatedSource = System.getenv("EMULATED_STORAGE_SOURCE");
						mEmulatedTarget = System.getenv("EMULATED_STORAGE_TARGET");
						mMediaStorage = System.getenv("MEDIA_STORAGE");
						if (TextUtils.isEmpty(mMediaStorage))
							mMediaStorage = "/data/media";
					}

					// Check storage folders
					if (fileName.startsWith("/sdcard") || (mMediaStorage != null && fileName.startsWith(mMediaStorage))
							|| (mExternalStorage != null && fileName.startsWith(mExternalStorage))
							|| (mEmulatedSource != null && fileName.startsWith(mEmulatedSource))
							|| (mEmulatedTarget != null && fileName.startsWith(mEmulatedTarget)))
						if (isRestrictedExtra(param, fileName))
							param.setThrowable(new FileNotFoundException("XPrivacy"));

				} else if (fileName.startsWith(mFileName) || mFileName.contains("...")) {
					// Zygote, Android
					if (Util.getAppId(Process.myUid()) == Process.SYSTEM_UID)
						return;

					// Proc white list
					if (mFileName.equals("/proc"))
						if ("/proc/self/cmdline".equals(fileName))
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

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

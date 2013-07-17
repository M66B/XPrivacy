package biz.bokhorst.xprivacy;

import java.io.File;

import android.os.Binder;
import android.os.Process;
import android.text.TextUtils;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XFile extends XHook {

	private String mPath;

	public XFile(String methodName, String restrictionName, String[] permissions, String path) {
		super(methodName, restrictionName, permissions, path);
		mPath = path;
	}

	// public File(File dir, String name) dir.getPath()
	// public File(String path)
	// public File(String dirPath, String name)
	// public File(URI uri)
	// public String getAbsolutePath()
	// public String getCanonicalPath()
	// public String getParent()
	// public String getPath()
	// public String[] list()
	// public String[] list(FilenameFilter filter)
	// public String toString()
	// libcore/luni/src/main/java/java/io/File.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (Process.myUid() <= 0 || Process.myUid() == PrivacyManager.cUidAndroid)
			return;

		String methodName = param.method.getName();
		if (methodName.equals("java.io.File")) {
			// Get path
			String path = null;
			if (param.args.length == 1 && param.args[0] != null) {
				if (param.args[0].getClass().equals(String.class))
					path = (String) param.args[0];
			} else if (param.args.length == 2 && param.args[0] != null) {
				if (param.args[0].getClass().equals(File.class)) {
					File dir = (File) param.args[0];
					path = dir.getPath();
				} else if (param.args[0].getClass().equals(String.class))
					path = (String) param.args[0];
			}

			// Check path
			if (path != null) {
				path = getAbsolutePath(path);
				Util.log(this, Log.INFO, "path=" + path + " uid=" + Process.myUid());
				if (path.startsWith(mPath)) {
					String isolatedStorage = getIsolateStorage();
					if (!path.startsWith(isolatedStorage)) {
						if (isRestricted(param, mPath))
							if (param.args[0].getClass().equals(String.class))
								param.args[0] = path.replace(mPath, isolatedStorage);
							else if (param.args[0].getClass().equals(File.class))
								param.args[0] = new File(path.replace(mPath, isolatedStorage));
					}
				}
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (Process.myUid() <= 0 || Process.myUid() == PrivacyManager.cUidAndroid)
			return;

		if (!param.method.getName().equals("java.io.File")) {
			Object result = param.getResult();
			if (result != null) {
				String isolatedStorage = getIsolateStorage();
				if (result.getClass().equals(String.class)) {
					String name = (String) result;
					if (name.startsWith(isolatedStorage))
						if (isRestricted(param, mPath))
							param.setResult(name.replace(isolatedStorage, mPath));
				} else if (result.getClass().equals(String[].class)) {
					boolean isolated = false;
					String[] names = (String[]) result;
					for (int i = 0; i < names.length; i++)
						if (names[i].startsWith(isolatedStorage)) {
							isolated = true;
							names[i] = names[i].replace(isolatedStorage, mPath);
						}
					if (isolated)
						if (isRestricted(param, mPath))
							param.setResult(names);
				}
			}
		}
	}

	// Derived from Environment.java

	public static String getIsolateStorage() {
		String rawExternalStorage = System.getenv("EXTERNAL_STORAGE");
		if (TextUtils.isEmpty(rawExternalStorage))
			rawExternalStorage = "/storage/sdcard0";
		return rawExternalStorage + File.separator + ".xprivacy" + File.separator + Binder.getCallingUid();
	}

	// Copied from File.java

	private static String getAbsolutePath(String path) {
		if (isAbsolute(path))
			return path;
		String userDir = System.getProperty("user.dir");
		return path.isEmpty() ? userDir : join(userDir, path);
	}

	private static boolean isAbsolute(String path) {
		return path.length() > 0 && path.charAt(0) == File.separatorChar;
	}

	private static String join(String prefix, String suffix) {
		int prefixLength = prefix.length();
		boolean haveSlash = (prefixLength > 0 && prefix.charAt(prefixLength - 1) == File.separatorChar);
		if (!haveSlash)
			haveSlash = (suffix.length() > 0 && suffix.charAt(0) == File.separatorChar);
		return haveSlash ? (prefix + suffix) : (prefix + File.separatorChar + suffix);
	}
}

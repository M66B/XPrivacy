package biz.bokhorst.xprivacy;

import java.io.File;

import android.os.Binder;
import android.os.Environment;
import android.os.Process;
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

	// System.getProperty("user.dir")
	// private static String join(String prefix, String suffix)
	// public boolean isAbsolute()

	@Override
	protected void before(MethodHookParam param) throws Throwable {

		if (Process.myUid() <= 0 || Process.myUid() == PrivacyManager.cUidAndroid)
			return;

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

		if (path != null) {
			if (!path.endsWith(File.separator))
				path = path + File.separator;
			if (path.startsWith(mPath)) {
				String isolated = Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator
						+ ".xprivacy" + File.separator + Binder.getCallingUid();
				if (!path.startsWith(isolated)) {
					Util.log(this, Log.INFO, "path=" + path + " uid=" + Process.myUid());
					if (isRestricted(param, mPath))
						if (param.args[0].getClass().equals(String.class))
							param.args[0] = path.replace(mPath, isolated);
						else if (param.args[0].getClass().equals(File.class))
							param.args[0] = new File(path.replace(mPath, isolated));
				}
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

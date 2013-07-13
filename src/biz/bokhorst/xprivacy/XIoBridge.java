package biz.bokhorst.xprivacy;

import java.io.FileNotFoundException;

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
		String fileName = (String) param.args[0];
		if (fileName != null && fileName.startsWith(mFileName))
			if (isRestricted(null))
				param.setResult(new FileNotFoundException());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

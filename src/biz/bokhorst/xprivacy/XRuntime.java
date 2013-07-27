package biz.bokhorst.xprivacy;

import java.io.IOException;
import android.os.Process;

import android.text.TextUtils;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XRuntime extends XHook {

	private String mCommand;

	public XRuntime(String methodName, String restrictionName, String[] permissions, String command) {
		super(restrictionName, methodName, permissions, command);
		mCommand = command;
	}

	// public Process exec(String[] progArray)
	// public Process exec(String[] progArray, String[] envp)
	// public Process exec(String[] progArray, String[] envp, File directory)
	// public Process exec(String prog)
	// public Process exec(String prog, String[] envp)
	// public Process exec(String prog, String[] envp, File directory)
	// void load(String filename, ClassLoader loader)
	// void loadLibrary(String libraryName, ClassLoader loader)
	// libcore/luni/src/main/java/java/lang/Runtime.java
	// http://developer.android.com/reference/java/lang/Runtime.html

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("exec")) {
			// Get programs
			String[] progs = null;
			if (param.args.length > 0 && param.args[0] != null)
				if (String.class.isAssignableFrom(param.args[0].getClass()))
					progs = new String[] { (String) param.args[0] };
				else
					progs = (String[]) param.args[0];

			// Check programs
			if (progs != null) {
				String command = TextUtils.join(" ", progs);
				if ((mCommand == null && !command.contains("sh ") && !command.contains("su "))
						|| (mCommand != null && command.contains(mCommand + " ")))
					if (isRestricted(param, mCommand == null ? getMethodName() : mCommand))
						param.setThrowable(new IOException());
			}
		} else if (methodName.equals("load") || methodName.equals("loadLibrary")) {
			// Skip pre Android
			if (Process.myUid() != 0)
				if (isRestricted(param))
					param.setResult(new UnsatisfiedLinkError());
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
	}
}

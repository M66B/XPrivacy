package biz.bokhorst.xprivacy;

import java.io.IOException;

import android.text.TextUtils;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XRuntime extends XHook {

	private String mCommand;

	public XRuntime(String methodName, String restrictionName, String[] permissions, String command) {
		super(methodName, restrictionName, permissions, command);
		mCommand = command;
	}

	// public Process exec(String[] progArray)
	// public Process exec(String[] progArray, String[] envp)
	// public Process exec(String[] progArray, String[] envp, File directory)
	// public Process exec(String prog)
	// public Process exec(String prog, String[] envp)
	// public Process exec(String prog, String[] envp, File directory)
	// public void load(String pathName)
	// public void loadLibrary(String libName)
	// libcore/luni/src/main/java/java/lang/Runtime.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("exec")) {
			// Get programs
			String[] progs;
			if (String.class.isAssignableFrom(param.args[0].getClass()))
				progs = new String[] { (String) param.args[0] };
			else
				progs = (String[]) param.args[0];

			// Check programs
			if (progs != null) {
				String command = TextUtils.join(" ", progs);
				XUtil.log(this, Log.INFO, "exec(" + command + ")");
				if ((mCommand == null && !command.startsWith("sh") && !command.startsWith("su"))
						|| (command != null && command.startsWith(mCommand)))
					if (isRestricted(param, mCommand == null ? getMethodName() : mCommand))
						param.setThrowable(new IOException(XRestriction.getDefacedString()));
			}
		} else if (isRestricted(param))
			param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

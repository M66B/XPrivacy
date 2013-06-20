package biz.bokhorst.xprivacy;

import java.io.IOException;

import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XRuntime extends XHook {

	public XRuntime(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public Process exec(String[] progArray)
	// public Process exec(String[] progArray, String[] envp)
	// public Process exec(String[] progArray, String[] envp, File directory)
	// public Process exec(String prog)
	// public Process exec(String prog, String[] envp)
	// public Process exec(String prog, String[] envp, File directory)
	// libcore/luni/src/main/java/java/lang/Runtime.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Get programs
		String[] progs;
		if (String.class.isAssignableFrom(param.args[0].getClass()))
			progs = new String[] { (String) param.args[0] };
		else
			progs = (String[]) param.args[0];

		// Check programs
		if (progs != null)
			for (String prog : progs) {
				XUtil.log(this, Log.INFO, "exec(" + prog + ")");
				if (prog.startsWith("ip"))
					if (isRestricted(param))
						throw new IOException(XRestriction.cDefaceString);
			}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

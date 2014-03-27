package biz.bokhorst.xprivacy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.text.TextUtils;
import android.util.Log;

public class XProcessBuilder extends XHook {

	private String mCommand;

	private XProcessBuilder(String methodName, String restrictionName, String command) {
		super(restrictionName, methodName, command);
		mCommand = command;
	}

	public String getClassName() {
		return "java.lang.ProcessBuilder";
	}

	// public Process start()
	// libcore/luni/src/main/java/java/lang/ProcessBuilder.java
	// http://developer.android.com/reference/java/lang/ProcessBuilder.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XProcessBuilder("start", PrivacyManager.cShell, "sh"));
		listHook.add(new XProcessBuilder("start", PrivacyManager.cShell, "su"));
		listHook.add(new XProcessBuilder("start", PrivacyManager.cShell, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("start")) {
			// Get commands
			ProcessBuilder builder = (ProcessBuilder) param.thisObject;
			List<String> listProg = (builder == null ? null : builder.command());

			// Check commands
			if (listProg != null) {
				String command = TextUtils.join(" ", listProg);
				if (XRuntime.matches(command, mCommand) && isRestrictedExtra(param, command))
					param.setThrowable(new IOException("XPrivacy"));
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

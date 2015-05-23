package biz.bokhorst.xprivacy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.os.Build;
import android.os.Process;
import android.text.TextUtils;

public class XRuntime extends XHook {
	private Methods mMethod;
	private String mCommand;

	private XRuntime(Methods method, String restrictionName, String command) {
		super(restrictionName, method.name(), command);
		mMethod = method;
		mCommand = command;
	}

	public String getClassName() {
		return "java.lang.Runtime";
	}

	@Override
	public boolean isVisible() {
		return !(mMethod == Methods.load || mMethod == Methods.loadLibrary);
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
	// http://developer.android.com/reference/java/lang/Runtime.html

	private enum Methods {
		exec, load, loadLibrary
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XRuntime(Methods.exec, PrivacyManager.cShell, "sh"));
		listHook.add(new XRuntime(Methods.exec, PrivacyManager.cShell, "su"));
		listHook.add(new XRuntime(Methods.exec, PrivacyManager.cShell, null));
		listHook.add(new XRuntime(Methods.load, PrivacyManager.cShell, null));
		listHook.add(new XRuntime(Methods.loadLibrary, PrivacyManager.cShell, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case exec:
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
				if (matches(command, mCommand) && isRestrictedExtra(param, command))
					param.setThrowable(new IOException("XPrivacy"));
			}
			break;

		case load:
		case loadLibrary:
			if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP || Process.myUid() != Process.SYSTEM_UID)
				if (param.args.length > 0) {
					String libName = (String) param.args[0];
					if (isRestrictedExtra(param, libName))
						param.setThrowable(new UnsatisfiedLinkError("XPrivacy"));
				}

			break;
		}
	}

	@Override
	protected void after(final XParam param) throws Throwable {
		// Do nothing
	}

	public static boolean matches(String command, String mCommand) {
		if (mCommand == null)
			return !isShell(command) && !isSU(command);
		else if (mCommand.equals("sh"))
			return isShell(command);
		else if (mCommand.equals("su"))
			return isSU(command);
		else
			return false;
	}

	private static boolean isShell(String command) {
		return command.startsWith("sh") || command.matches("/.*/.*/sh.*") || command.contains("sh ");
	}

	private static boolean isSU(String command) {
		return command.startsWith("su") || command.matches("/.*/.*/su.*") || command.contains("su ");
	}
}

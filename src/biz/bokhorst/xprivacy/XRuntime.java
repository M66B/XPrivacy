package biz.bokhorst.xprivacy;

import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;

import android.content.Context;
import android.os.Binder;
import android.text.TextUtils;
import android.util.Log;

public class XRuntime extends XHook {
	private Methods mMethod;
	private String mCommand;

	private static List<Method> listNative = new ArrayList<Method>();

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
		if (mMethod == Methods.exec) {
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

		} else if (mMethod == Methods.load || mMethod == Methods.loadLibrary) {
			if (param.args.length > 0) {
				String libName = (String) param.args[0];
				if (isRestrictedExtra(param, libName))
					param.setThrowable(new UnsatisfiedLinkError("XPrivacy"));
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(final XParam param) throws Throwable {
		if (mMethod == Methods.load || mMethod == Methods.loadLibrary) {
			if (!PrivacyManager.getSettingBool(0, PrivacyManager.cSettingPermMan, false))
				return;

			if (param.args.length > 0 && PrivacyManager.isApplication(Binder.getCallingUid())) {
				final String libName = (String) param.args[0];

				// Get caller class name
				String callerClassName = null;
				StackTraceElement[] ste = Thread.currentThread().getStackTrace();
				for (int i = 0; i < ste.length; i++)
					if (ste[i].getClassName().equals(param.thisObject.getClass().getName())) {
						if (i + 2 < ste.length)
							callerClassName = ste[i + 2].getClassName();
						break;
					}

				// Get caller class
				Class<?> clazz = null;
				Util.log(this, Log.WARN, "Class name=" + callerClassName);
				ClassLoader loader = Thread.currentThread().getContextClassLoader();
				try {
					clazz = Class.forName(callerClassName, false, loader);
				} catch (ClassNotFoundException ignored) {
					try {
						clazz = Class.forName(callerClassName, false, Context.class.getClassLoader());
					} catch (ClassNotFoundException ignored2) {
						Util.log(this, Log.WARN, "Class not found name=" + callerClassName);
					}
				}

				if (clazz != null)
					synchronized (listNative) {
						for (Method method : clazz.getDeclaredMethods())
							if (Modifier.isNative(method.getModifiers()) && !listNative.contains(method)) {
								listNative.add(method);
								XposedBridge.hookMethod(method, new XC_MethodHook() {
									@Override
									protected void beforeHookedMethod(MethodHookParam mparam) throws Throwable {
										Util.log(null, Log.WARN, "Native call method=" + mparam.method.getName());
										if (isRestrictedExtra(param, libName + ":" + mparam.method.getName()))
											mparam.setThrowable(new SecurityException("XPrivacy"));
									}
								});
								Util.log(this, Log.WARN, "Hooked native method=" + method.getName());
							}
					}
			}
		}
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

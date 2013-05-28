package biz.bokhorst.xprivacy;

import java.util.Set;

import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers.ClassNotFoundError;
import de.robv.android.xposed.callbacks.XC_LoadPackage.LoadPackageParam;
import de.robv.android.xposed.XC_MethodHook;
import static de.robv.android.xposed.XposedHelpers.findClass;

public class XPrivacy implements IXposedHookLoadPackage {
	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		XUtil.log(null, XUtil.LOG_INFO, String.format("load package=%s", lpparam.packageName));

		// Load any
		hook(new XContextImplInit(), lpparam, "android.app.ContextImpl", "init");

		// Load android
		if (lpparam.packageName.equals("android")) {
			hook(new XGetLastKnownLocation(), lpparam, "android.location.LocationManager", "getLastKnownLocation");
		} else {

			// Load providers.contacts
			if (lpparam.packageName.equals("com.android.providers.contacts")) {
				hook(new XContactProvider2query(), lpparam, "com.android.providers.contacts.ContactsProvider2", "query");
			}

			// Load settings.applications
			else if (lpparam.packageName.equals("com.android.settings")) {
				hook(new XInstalledAppDetails(), lpparam, "com.android.settings.applications.InstalledAppDetails",
						"refreshUi");
			}
		}
	}

	private void hook(final XHook hook, final LoadPackageParam lpparam, String className, String methodName) {
		try {
			// Create hook
			XC_MethodHook methodHook = new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
					try {
						XUtil.log(hook, XUtil.LOG_DEBUG, "before");
						hook.before(param);
					} catch (Exception ex) {
						XUtil.bug(null, ex);
						// throw ex;
					}
				}

				@Override
				protected void afterHookedMethod(MethodHookParam param) throws Throwable {
					try {
						XUtil.log(hook, XUtil.LOG_DEBUG, "after");
						hook.after(param);
					} catch (Exception ex) {
						XUtil.bug(null, ex);
						// throw ex;
					}
				}
			};

			// Add hook
			Set<XC_MethodHook.Unhook> hookSet;
			Class<?> clazz = findClass(className, lpparam.classLoader);
			if (methodName == null)
				hookSet = XposedBridge.hookAllConstructors(clazz, methodHook);
			else
				hookSet = XposedBridge.hookAllMethods(clazz, methodName, methodHook);
			for (XC_MethodHook.Unhook unhook : hookSet)
				XUtil.log(hook, XUtil.LOG_INFO,
						String.format("hooked %s of %s", unhook.getHookedMethod().getName(), lpparam.packageName));
		} catch (ClassNotFoundError ignored) {
			XUtil.log(hook, XUtil.LOG_ERROR, "class not found");
		} catch (NoSuchMethodError ignored) {
			XUtil.log(hook, XUtil.LOG_ERROR, "method not found");
		} catch (Exception ex) {
			XUtil.bug(null, ex);
		}
	}
}

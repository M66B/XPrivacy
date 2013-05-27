package biz.bokhorst.xprivacy;

import java.util.Arrays;

import android.net.Uri;
import android.os.CancellationSignal;

import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.XposedHelpers.ClassNotFoundError;
import de.robv.android.xposed.callbacks.XC_LoadPackage.LoadPackageParam;
import de.robv.android.xposed.XC_MethodHook;
import static de.robv.android.xposed.XposedHelpers.findAndHookMethod;

public class XPrivacy implements IXposedHookLoadPackage {
	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		XUtil.log(null, XUtil.LOG_INFO, String.format("load package=%s", lpparam.packageName));

		// Load android
		if (lpparam.packageName.equals("android")) {
			hook(new XGetLastKnownLocation(), lpparam, "android.location.LocationManager", "getLastKnownLocation",
					String.class /* provider */);
		} else {
			// Load any service/application
			if (!lpparam.packageName.equals("system")) {
				hook(new XApplicationOnCreate(), lpparam, "android.app.Service", "onCreate");
				hook(new XApplicationOnCreate(), lpparam, "android.app.Application", "onCreate");
			}

			// Load providers.contacts
			if (lpparam.packageName.equals("com.android.providers.contacts")) {
				hook(new XContactProvider2query(), lpparam, "com.android.providers.contacts.ContactsProvider2",
						"query", Uri.class, String[].class /* projection */, String.class /* selection */,
						String[].class /* selectionArgs */, String.class /* sortOrder */, CancellationSignal.class);
			}

			// Load settings.applications
			else if (lpparam.packageName.equals("com.android.settings")) {
				hook(new XInstalledAppDetails(), lpparam, "com.android.settings.applications.InstalledAppDetails",
						"refreshUi");
			}
		}
	}

	private void hook(final XHook hook, final LoadPackageParam lpparam, String className, String methodName,
			Object... parameterTypes) {
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
					}
				}

				@Override
				protected void afterHookedMethod(MethodHookParam param) throws Throwable {
					try {
						XUtil.log(hook, XUtil.LOG_DEBUG, "after");
						hook.after(param);
					} catch (Exception ex) {
						XUtil.bug(null, ex);
					}
				}
			};

			// Add hook
			Object[] parameterTypesAndHook = Arrays.copyOf(parameterTypes, parameterTypes.length + 1);
			parameterTypesAndHook[parameterTypes.length] = methodHook;
			findAndHookMethod(className, lpparam.classLoader, methodName, parameterTypesAndHook);
			XUtil.log(hook, XUtil.LOG_INFO, "hooked");

		} catch (ClassNotFoundError ignored) {
			XUtil.log(hook, XUtil.LOG_ERROR, "class not found");
		} catch (NoSuchMethodError ignored) {
			XUtil.log(hook, XUtil.LOG_ERROR, "method not found");
		} catch (Exception ex) {
			XUtil.bug(null, ex);
		}
	}
}

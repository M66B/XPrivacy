package biz.bokhorst.xprivacy;

import java.util.Arrays;

import android.net.Uri;
import android.os.CancellationSignal;
import android.util.Log;

import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.callbacks.XC_LoadPackage.LoadPackageParam;
import de.robv.android.xposed.XC_MethodHook;
import static de.robv.android.xposed.XposedHelpers.findAndHookMethod;

public class XPrivacy implements IXposedHookLoadPackage {
	static {
		System.setProperty(XContactProvider2query.cPropertyDeny, "com.facebook.katana");
	}

	public void handleLoadPackage(final LoadPackageParam lpparam) throws Throwable {
		XUtil.info(null, String.format("load package=%s process=%s", lpparam.packageName, lpparam.processName));

		// Load com.android.providers.contacts
		if (lpparam.packageName.equals("com.android.providers.contacts")) {
			// Hook ContactsProvider2.query
			hook(new XContactProvider2query(), lpparam, "com.android.providers.contacts.ContactsProvider2", "query",
					Uri.class, String[].class /* projection */, String.class /* selection */,
					String[].class /* selectionArgs */, String.class /* sortOrder */, CancellationSignal.class);
		}
	}

	private void hook(final XHook hook, final LoadPackageParam lpparam, String className, String methodName,
			Object... parameterTypes) {
		try {
			// Copy parameter types
			Object[] parameterTypesAndHook = Arrays.copyOf(parameterTypes, parameterTypes.length + 1);

			// Add hook
			parameterTypesAndHook[parameterTypes.length] = new XC_MethodHook() {
				@Override
				protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
					try {
						XUtil.info(hook, "before");
						hook.before(param);
					} catch (Exception ex) {
						XUtil.error(hook, "before " + Log.getStackTraceString(ex));
					}
				}

				@Override
				protected void afterHookedMethod(MethodHookParam param) throws Throwable {
					try {
						XUtil.info(hook, "after");
						hook.after(param);
					} catch (Exception ex) {
						XUtil.error(hook, "after " + Log.getStackTraceString(ex));
					}
				}
			};

			// Hook
			findAndHookMethod(className, lpparam.classLoader, methodName, parameterTypesAndHook);
			XUtil.info(hook, "hooked");
		} catch (NoSuchMethodError ignored) {
			XUtil.error(hook, "method not found");
		} catch (Exception ex) {
			XUtil.error(hook, "hook " + Log.getStackTraceString(ex));
		}
	}
}

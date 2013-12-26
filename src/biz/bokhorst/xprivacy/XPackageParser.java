package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.annotation.TargetApi;
import android.content.pm.ProviderInfo;
import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XPackageParser extends XHook {
	private Methods mMethod;

	private XPackageParser(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.content.pm.PackageParser";
	}

	public boolean isVisible() {
		return false;
	}

	// private Provider parseProvider(Package owner, ...)
	// frameworks/base/core/java/android/content/pm/PackageParser.java

	private enum Methods {
		parseProvider
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1)
			listHook.add(new XPackageParser(Methods.parseProvider, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.parseProvider) {
			if (param.args.length > 0)
				try {
					// Package: public String packageName;
					Field fieldPackageName = param.args[0].getClass().getDeclaredField("packageName");
					String packageName = (String) fieldPackageName.get(param.args[0]);
					String self = XPackageParser.class.getPackage().getName();
					if (self.equals(packageName)) {
						Object provider = param.getResult();
						// Provider: public final ProviderInfo info;
						Field fieldInfo = provider.getClass().getDeclaredField("info");
						ProviderInfo providerInfo = (ProviderInfo) fieldInfo.get(provider);
						providerInfo.flags |= ProviderInfo.FLAG_SINGLE_USER;
						providerInfo.exported = true;
						Util.log(this, Log.WARN, "Privacy provider single user");
					}
				} catch (Throwable ex) {
					Util.bug(this, ex);
				}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

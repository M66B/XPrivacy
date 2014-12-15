package biz.bokhorst.xprivacy;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.os.Build;
import android.os.Environment;

import biz.bokhorst.xprivacy.XHook;

public class XEnvironment extends XHook {
	private Methods mMethod;

	private XEnvironment(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.os.Environment";
	}

	// public static String getExternalStorageState()
	// frameworks/base/core/java/android/os/Environment.java
	// http://developer.android.com/reference/android/os/Environment.html

	private enum Methods {
		getExternalStorageState
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XEnvironment(Methods.getExternalStorageState, PrivacyManager.cStorage));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getExternalStorageState:
			if (param.getResult() != null) {
				String extra = null;
				if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
					if (param.args.length > 0 && param.args[0] instanceof File)
						extra = ((File) param.args[0]).getAbsolutePath();

				if (isRestrictedExtra(param, extra))
					param.setResult(Environment.MEDIA_UNMOUNTED);
			}
			break;
		}
	}
}

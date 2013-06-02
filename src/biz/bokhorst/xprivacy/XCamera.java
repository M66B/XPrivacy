package biz.bokhorst.xprivacy;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XCamera extends XHook {

	public XCamera(String methodName, String restrictionName) {
		super(methodName, restrictionName);
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param))
			param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		// How to get context?
		return false;
	}
}

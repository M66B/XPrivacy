package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;

import biz.bokhorst.xprivacy.XHook;

public class XIPC extends XHook {
	private Method mMethod;
	private String mInterfaceName;

	public XIPC(Method method, String descriptor) {
		super(PrivacyManager.cIPC, method.getName(), "Service");
		mMethod = method;
		String[] name = descriptor.split("\\.");
		mInterfaceName = name[name.length - 1];
	}

	public String getClassName() {
		return mMethod.getDeclaringClass().getName();
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (isRestrictedExtra(param, mInterfaceName + ":" + param.method.getName()))
			param.setThrowable(new SecurityException("XPrivacy"));
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

package biz.bokhorst.xprivacy;

import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XParam {
	public Member method;
	public Object thisObject;
	public Object[] args;
	private Object mResult;
	private Throwable mThrowable;
	private boolean mHasResult = false;
	private boolean mHasThrowable = false;
	private Map<String, Object> mExtra = null;

	private XParam() {
	}

	public static XParam fromXposed(MethodHookParam param) {
		XParam xparam = new XParam();
		xparam.method = param.method;
		xparam.thisObject = param.thisObject;
		xparam.args = param.args;
		xparam.mResult = param.getResult();
		xparam.mThrowable = param.getThrowable();

		if (xparam.args == null)
			xparam.args = new Object[] {};

		return xparam;
	}

	public boolean doesReturn(Class<?> result) {
		if (this.method instanceof Method)
			return (((Method) this.method).getReturnType().equals(result));
		return false;
	}

	public void setResult(Object result) {
		if (result instanceof Throwable) {
			Util.log(null, Log.ERROR, "Set result throwable=" + result);
			setThrowable((Throwable) result);
		} else {
			mResult = result;
			mHasResult = true;
		}
	}

	public boolean hasResult() {
		return mHasResult;
	}

	public Object getResult() {
		return mResult;
	}

	public boolean doesThrow(Class<?> ex) {
		if (this.method instanceof Method)
			for (Class<?> t : ((Method) this.method).getExceptionTypes())
				if (t.equals(ex))
					return true;
		return false;
	}

	public void setThrowable(Throwable ex) {
		mThrowable = ex;
		mHasThrowable = true;
	}

	public boolean hasThrowable() {
		return mHasThrowable;
	}

	public Throwable getThrowable() {
		return mThrowable;
	}

	public Object getExtras() {
		return mExtra;
	}

	@SuppressWarnings("unchecked")
	public void setExtras(Object extra) {
		mExtra = (Map<String, Object>) extra;
	}

	public void setObjectExtra(String name, Object value) {
		if (mExtra == null)
			mExtra = new HashMap<String, Object>();
		mExtra.put(name, value);
	}

	public Object getObjectExtra(String name) {
		return (mExtra == null ? null : mExtra.get(name));
	}
}

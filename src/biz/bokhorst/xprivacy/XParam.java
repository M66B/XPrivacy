package biz.bokhorst.xprivacy;

import java.lang.reflect.Member;
import java.util.HashMap;
import java.util.Map;

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
		return xparam;
	}

	public void setResult(Object result) {
		mResult = result;
		mHasResult = true;
	}

	public boolean hasResult() {
		return mHasResult;
	}

	public Object getResult() {
		return mResult;
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

	public void setObjectExtra(String name, Object value) {
		if (mExtra == null)
			mExtra = new HashMap<String, Object>();
		mExtra.put(name, value);
	}

	public Object getObjectExtra(String name) {
		return (mExtra == null ? null : mExtra.get(name));
	}
}

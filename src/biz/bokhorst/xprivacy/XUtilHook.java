package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XUtilHook extends XHook {

	private XUtilHook(String methodName, String restrictionName, int sdk) {
		super(restrictionName, methodName, null, sdk);
	}

	public String getClassName() {
		return Util.class.getName();
	}

	// isXposedEnabled
	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XUtilHook("isXposedEnabled", null, 1));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		Util.log(this, Log.INFO, param.method.getName() + "=true");
		param.setResult(true);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XUtilHook extends XHook {

	private XUtilHook(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return Util.class.getName();
	}

	// isXposedEnabled
	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XUtilHook("isXposedEnabled", null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		Util.log(this, Log.INFO, param.method.getName() + "=true");
		param.setResult(true);
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

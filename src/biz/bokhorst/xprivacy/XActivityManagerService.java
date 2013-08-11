package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.SystemClock;
import android.util.Log;
import biz.bokhorst.xprivacy.XHook;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityManagerService extends XHook {

	private static boolean mSystemReady = false;
	private static long mSystemReadyAt = 0;

	private static final long cSystemReadyDelay = 60 * 1000;

	protected XActivityManagerService(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	@Override
	public String getClassName() {
		return "com.android.server.am.ActivityManagerService";
	}

	// public void systemReady(final Runnable goingCallback)

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XActivityManagerService("systemReady", null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		mSystemReady = true;
		mSystemReadyAt = SystemClock.uptimeMillis();

		Util.log(this, Log.INFO, "System ready");
	}

	public static boolean isSystemReady() {
		return (mSystemReady ? SystemClock.uptimeMillis() - mSystemReadyAt > cSystemReadyDelay : false);
	}
}

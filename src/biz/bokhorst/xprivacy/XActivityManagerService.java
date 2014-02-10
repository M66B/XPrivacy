package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityManagerService extends XHook {
	private Methods mMethod;

	private static Semaphore mOndemandSemaphore;
	private static boolean mFinishedBooting = false;
	private static boolean mLockScreen = false;
	private static boolean mSleeping = false;
	private static boolean mShutdown = false;

	private XActivityManagerService(Methods method, int sdk) {
		super(null, method.name(), null, sdk);
		mMethod = method;
	}

	@Override
	public boolean isVisible() {
		return (mMethod != Methods.appNotResponding && mMethod != Methods.finishBooting);
	}

	public String getClassName() {
		return "com.android.server.am.ActivityManagerService";
	}

	// @formatter:off
	// 4.2+ public long inputDispatchingTimedOut(int pid, final boolean aboveSystem, String reason)
	// 4.3+ public boolean inputDispatchingTimedOut(final ProcessRecord proc, final ActivityRecord activity, final ActivityRecord parent, final boolean aboveSystem, String reason)
	// 4.0.3+ final void appNotResponding(ProcessRecord app, ActivityRecord activity, ActivityRecord parent, boolean aboveSystem, final String annotation)
	// 4.0.3+ public void systemReady(final Runnable goingCallback)
	// 4.0.3+ final void finishBooting()
	// 4.1+ public void setLockScreenShown(boolean shown)
	// 4.0.3+ public void goingToSleep()
	// 4.0.3+ public void wakingUp()
	// 4.0.3+ public boolean shutdown(int timeout)
	// frameworks/base/services/java/com/android/server/am/ActivityManagerService.java
	// @formatter:on

	private enum Methods {
		inputDispatchingTimedOut, appNotResponding, systemReady, finishBooting, setLockScreenShown, goingToSleep, wakingUp, shutdown
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XActivityManagerService(Methods.inputDispatchingTimedOut, Build.VERSION_CODES.JELLY_BEAN_MR1));
		listHook.add(new XActivityManagerService(Methods.appNotResponding, Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1));
		listHook.add(new XActivityManagerService(Methods.systemReady, Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1));
		listHook.add(new XActivityManagerService(Methods.finishBooting, Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1));
		listHook.add(new XActivityManagerService(Methods.setLockScreenShown, Build.VERSION_CODES.JELLY_BEAN).optional());
		listHook.add(new XActivityManagerService(Methods.goingToSleep, Build.VERSION_CODES.JELLY_BEAN));
		listHook.add(new XActivityManagerService(Methods.wakingUp, Build.VERSION_CODES.JELLY_BEAN));
		listHook.add(new XActivityManagerService(Methods.shutdown, Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1));
		// setLockScreenShown appears not to be present in some 4.2.2 ROMs
		return listHook;
	}

	public static void setSemaphore(Semaphore semaphore) {
		mOndemandSemaphore = semaphore;
	}

	public static boolean canOnDemand() {
		return (mFinishedBooting && !mLockScreen && !mSleeping);
	}

	public static boolean canWriteUsageData() {
		return !mShutdown;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.inputDispatchingTimedOut) {
			try {
				// Delay foreground ANRs while on demand dialog open
				boolean ondemanding = (mOndemandSemaphore.availablePermits() < 1);
				Util.log(this, Log.WARN, "Foreground ANR uid=" + getUidANR(param) + " ondemand=" + ondemanding);
				if (ondemanding)
					param.setResult(5 * 1000); // 5 seconds
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}

		} else if (mMethod == Methods.appNotResponding) {
			try {
				// Ignore background ANRs while on demand dialog open
				boolean ondemanding = (mOndemandSemaphore.availablePermits() < 1);
				Util.log(this, Log.WARN, "Background ANR uid=" + getUidANR(param) + " ondemand=" + ondemanding);
				if (ondemanding)
					param.setResult(null);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}

		} else if (mMethod == Methods.systemReady) {
			// Do nothing

		} else if (mMethod == Methods.finishBooting) {
			// Do nothing

		} else if (mMethod == Methods.setLockScreenShown) {
			if (param.args.length > 0)
				try {
					if ((Boolean) param.args[0]) {
						mLockScreen = true;
						Util.log(this, Log.WARN, "Lockscreen=" + mLockScreen);
					}
				} catch (Throwable ex) {
					Util.bug(this, ex);
				}

		} else if (mMethod == Methods.goingToSleep) {
			mSleeping = true;
			Util.log(this, Log.WARN, "Sleeping=" + mSleeping);

		} else if (mMethod == Methods.wakingUp) {
			// Do nothing

		} else if (mMethod == Methods.shutdown) {
			mShutdown = true;
			Util.log(this, Log.WARN, "Shutdown");

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.systemReady) {
			Util.log(this, Log.WARN, "System ready");

		} else if (mMethod == Methods.finishBooting) {
			mFinishedBooting = true;
			Util.log(this, Log.WARN, "Finished booting");

		} else if (mMethod == Methods.setLockScreenShown) {
			if (param.args.length > 0)
				if (!(Boolean) param.args[0]) {
					mLockScreen = false;
					Util.log(this, Log.WARN, "Lockscreen=" + mLockScreen);
				}

		} else if (mMethod == Methods.wakingUp) {
			mSleeping = false;
			Util.log(this, Log.WARN, "Sleeping=" + mSleeping);
		}
	}

	// Helper method

	private int getUidANR(MethodHookParam param) throws IllegalAccessException {
		int uid = -1;
		try {
			Class<?> pr = Class.forName("com.android.server.am.ProcessRecord");
			if (param.args.length > 0 && param.args[0] != null && param.args[0].getClass().equals(pr)) {
				uid = (Integer) pr.getDeclaredField("uid").get(param.args[0]);
			}
		} catch (ClassNotFoundException ignored) {
		} catch (NoSuchFieldException ignored) {
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
		return uid;
	}
}

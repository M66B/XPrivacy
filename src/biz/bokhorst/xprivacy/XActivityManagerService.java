package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

import android.annotation.SuppressLint;
import android.util.Log;

@SuppressLint("InlinedApi")
public class XActivityManagerService extends XHook {
	private Methods mMethod;

	private static Semaphore mOndemandSemaphore;
	private static boolean mFinishedBooting = false;
	private static boolean mLockScreen = false;
	private static boolean mSleeping = false;
	private static boolean mShutdown = false;

	private XActivityManagerService(Methods method) {
		super(null, method.name(), null);
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

	// @formatter:off
	private enum Methods {
		inputDispatchingTimedOut, appNotResponding,
		systemReady, finishBooting, setLockScreenShown, goingToSleep, wakingUp, shutdown
	};
	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XActivityManagerService(Methods.inputDispatchingTimedOut));
		listHook.add(new XActivityManagerService(Methods.appNotResponding));
		listHook.add(new XActivityManagerService(Methods.systemReady));
		listHook.add(new XActivityManagerService(Methods.finishBooting));
		// setLockScreenShown appears not to be present in some 4.2.2 ROMs
		listHook.add(new XActivityManagerService(Methods.setLockScreenShown));
		listHook.add(new XActivityManagerService(Methods.goingToSleep));
		listHook.add(new XActivityManagerService(Methods.wakingUp));
		listHook.add(new XActivityManagerService(Methods.shutdown));
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
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case inputDispatchingTimedOut:
			// Delay foreground ANRs while on demand dialog open
			if (mOndemandSemaphore != null && mOndemandSemaphore.availablePermits() < 1) {
				Util.log(this, Log.WARN, "Foreground ANR uid=" + getUidANR(param));
				param.setResult(5 * 1000); // 5 seconds
			}
			break;

		case appNotResponding:
			// Ignore background ANRs while on demand dialog open
			if (mOndemandSemaphore != null && mOndemandSemaphore.availablePermits() < 1) {
				Util.log(this, Log.WARN, "Background ANR uid=" + getUidANR(param));
				param.setResult(null);
			}
			break;

		case systemReady:
			// Do nothing
			break;

		case finishBooting:
			// Do nothing
			break;

		case setLockScreenShown:
			if (param.args.length > 0 && param.args[0] instanceof Boolean)
				try {
					if ((Boolean) param.args[0]) {
						mLockScreen = true;
						Util.log(this, Log.WARN, "Lockscreen=" + mLockScreen);
					}
				} catch (Throwable ex) {
					Util.bug(this, ex);
				}
			break;

		case goingToSleep:
			mSleeping = true;
			Util.log(this, Log.WARN, "Sleeping=" + mSleeping);
			break;

		case wakingUp:
			// Do nothing
			break;

		case shutdown:
			mShutdown = true;
			Util.log(this, Log.WARN, "Shutdown");
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case inputDispatchingTimedOut:
		case appNotResponding:
			break;

		case systemReady:
			Util.log(this, Log.WARN, "System ready");
			break;

		case finishBooting:
			mFinishedBooting = true;
			Util.log(this, Log.WARN, "Finished booting");
			break;

		case setLockScreenShown:
			if (param.args.length > 0 && param.args[0] instanceof Boolean)
				if (!(Boolean) param.args[0]) {
					mLockScreen = false;
					Util.log(this, Log.WARN, "Lockscreen=" + mLockScreen);
				}
			break;

		case goingToSleep:
			break;

		case wakingUp:
			mSleeping = false;
			Util.log(this, Log.WARN, "Sleeping=" + mSleeping);
			break;

		case shutdown:
			break;
		}
	}

	// Helper methods

	private int getUidANR(XParam param) throws IllegalAccessException {
		int uid = -1;
		try {
			Class<?> pr = Class.forName("com.android.server.am.ProcessRecord");
			if (param.args.length > 0 && param.args[0] != null && param.args[0].getClass().equals(pr)) {
				Field fUid = pr.getDeclaredField("uid");
				fUid.setAccessible(true);
				uid = (Integer) fUid.get(param.args[0]);
			}
		} catch (ClassNotFoundException ignored) {
		} catch (NoSuchFieldException ignored) {
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
		return uid;
	}
}

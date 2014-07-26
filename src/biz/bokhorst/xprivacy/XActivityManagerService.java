package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

import android.annotation.SuppressLint;
import android.content.ComponentName;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.provider.MediaStore;
import android.util.Log;

@SuppressLint("InlinedApi")
public class XActivityManagerService extends XHook {
	private Methods mMethod;
	private static Map<String, String> mapIntentRestriction = new HashMap<String, String>();

	private static Semaphore mOndemandSemaphore;
	private static boolean mFinishedBooting = false;
	private static boolean mLockScreen = false;
	private static boolean mSleeping = false;
	private static boolean mShutdown = false;

	static {
		mapIntentRestriction.put(Intent.ACTION_VIEW, PrivacyManager.cView);
		mapIntentRestriction.put(Intent.ACTION_CALL, PrivacyManager.cCalling);
		mapIntentRestriction.put(Intent.ACTION_DIAL, PrivacyManager.cCalling);
		mapIntentRestriction.put(MediaStore.ACTION_IMAGE_CAPTURE, PrivacyManager.cMedia);
		mapIntentRestriction.put(MediaStore.ACTION_IMAGE_CAPTURE_SECURE, PrivacyManager.cMedia);
		mapIntentRestriction.put(MediaStore.ACTION_VIDEO_CAPTURE, PrivacyManager.cMedia);
	}

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

	// public int startActivities(IApplicationThread caller, String callingPackage, Intent[] intents, String[] resolvedTypes, IBinder resultTo, Bundle options, int userId)
	// public int startActivity(IApplicationThread caller, String callingPackage, Intent intent, String resolvedType, IBinder resultTo, String resultWho, int requestCode, int flags, String profileFile, ParcelFileDescriptor profileFd, Bundle options)
	// public int startActivityAsUser(IApplicationThread caller, String callingPackage, Intent intent, String resolvedType, IBinder resultTo, String resultWho, int requestCode, int flags, String profileFile,ParcelFileDescriptor profileFd, Bundle options, int userId)
	// public WaitResult startActivityAndWait(IApplicationThread caller, String callingPackage, Intent intent, String resolvedType, IBinder resultTo, String resultWho, int requestCode, int flags, String profileFile, ParcelFileDescriptor profileFd, Bundle options, int userId)
	// public int startActivityWithConfig(IApplicationThread caller, String callingPackage, Intent intent, String resolvedType, IBinder resultTo, String resultWho, int requestCode, int startFlags, Configuration newConfig, Bundle options, int userId)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/server/am/ActivityManagerService.java/

	// @formatter:on

	// @formatter:off
	private enum Methods {
		inputDispatchingTimedOut, appNotResponding, systemReady, finishBooting, setLockScreenShown, goingToSleep, wakingUp, shutdown,
		startActivities, startActivity, startActivityAsUser, startActivityAndWait, startActivityWithConfig
	};
	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();

		listHook.add(new XActivityManagerService(Methods.startActivities, Build.VERSION_CODES.KITKAT));
		listHook.add(new XActivityManagerService(Methods.startActivity, Build.VERSION_CODES.KITKAT));
		listHook.add(new XActivityManagerService(Methods.startActivityAsUser, Build.VERSION_CODES.KITKAT));
		listHook.add(new XActivityManagerService(Methods.startActivityAndWait, Build.VERSION_CODES.KITKAT));
		listHook.add(new XActivityManagerService(Methods.startActivityWithConfig, Build.VERSION_CODES.KITKAT));

		listHook.add(new XActivityManagerService(Methods.inputDispatchingTimedOut, Build.VERSION_CODES.JELLY_BEAN_MR1));
		listHook.add(new XActivityManagerService(Methods.appNotResponding, Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1));
		listHook.add(new XActivityManagerService(Methods.systemReady, Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1));
		listHook.add(new XActivityManagerService(Methods.finishBooting, Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1));
		listHook.add(new XActivityManagerService(Methods.setLockScreenShown, Build.VERSION_CODES.JELLY_BEAN_MR1)
				.optional());
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
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case inputDispatchingTimedOut:
			try {
				// Delay foreground ANRs while on demand dialog open
				boolean ondemanding = (mOndemandSemaphore != null && mOndemandSemaphore.availablePermits() < 1);
				Util.log(this, Log.WARN, "Foreground ANR uid=" + getUidANR(param) + " ondemand=" + ondemanding);
				if (ondemanding)
					param.setResult(5 * 1000); // 5 seconds
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
			break;

		case appNotResponding:
			try {
				// Ignore background ANRs while on demand dialog open
				boolean ondemanding = (mOndemandSemaphore != null && mOndemandSemaphore.availablePermits() < 1);
				Util.log(this, Log.WARN, "Background ANR uid=" + getUidANR(param) + " ondemand=" + ondemanding);
				if (ondemanding)
					param.setResult(null);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
			break;

		case systemReady:
			// Do nothing
			break;

		case finishBooting:
			// Do nothing
			break;

		case setLockScreenShown:
			if (param.args.length > 0)
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

		case startActivities:
			if (param.args.length > 2 && param.args[2] instanceof Intent[]) {
				List<Intent> listIntent = new ArrayList<Intent>();
				for (Intent intent : (Intent[]) param.args[2])
					if (!isRestricted(param, intent))
						listIntent.add(intent);
				if (listIntent.size() == 0)
					param.setResult(0); // ActivityManager.START_SUCCESS
				else
					param.args[2] = listIntent.toArray();
			}
			break;

		case startActivity:
		case startActivityAsUser:
		case startActivityWithConfig:
			if (param.args.length > 2 && param.args[2] instanceof Intent) {
				Intent intent = (Intent) param.args[2];
				if (isRestricted(param, intent))
					param.setResult(0); // ActivityManager.START_SUCCESS
			}
			break;

		case startActivityAndWait:
			if (param.args.length > 2 && param.args[2] instanceof Intent) {
				Intent intent = (Intent) param.args[2];
				if (isRestricted(param, intent)) {
					Class<?> cWaitResult = Class.forName("android.app.IActivityManager.WaitResult");
					Field fWho = cWaitResult.getDeclaredField("who");
					Class<?> we = this.getClass();
					ComponentName component = new ComponentName(we.getPackage().getName(), we.getName());

					Object waitResult = cWaitResult.getConstructor().newInstance();
					fWho.set(waitResult, component);
					param.setResult(waitResult);
				}
			}
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
			if (param.args.length > 0)
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

		case startActivities:
		case startActivity:
		case startActivityAsUser:
		case startActivityAndWait:
		case startActivityWithConfig:
			break;
		}
	}

	// Helper methods

	private boolean isRestricted(XParam param, Intent intent) throws Throwable {
		String action = intent.getAction();
		if (mapIntentRestriction.containsKey(action)) {
			String restrictionName = mapIntentRestriction.get(action);
			if (Intent.ACTION_VIEW.equals(action)) {
				Uri uri = intent.getData();
				if (uri != null)
					return isRestrictedExtra(param, restrictionName, "Srv_" + action, uri.toString());
			} else
				return isRestricted(param, restrictionName, "Srv_" + action);
		}

		return false;
	}

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

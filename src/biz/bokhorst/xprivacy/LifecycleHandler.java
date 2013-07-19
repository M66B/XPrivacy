package biz.bokhorst.xprivacy;

import android.app.Activity;
import android.app.Application.ActivityLifecycleCallbacks;
import android.os.Bundle;

public class LifecycleHandler implements ActivityLifecycleCallbacks {

	private static int mCreated = 0;
	private static int mStarted = 0;
	private static int mResumed = 0;

	public static boolean isActive() {
		return (mCreated > 0 && mStarted > 0 && mResumed > 0);
	}

	@Override
	public void onActivityCreated(Activity activity, Bundle bundle) {
		if (activity.getClass().equals(ActivityMain.class))
			mCreated++;
	}

	@Override
	public void onActivityStarted(Activity activity) {
		if (activity.getClass().equals(ActivityMain.class))
			mStarted++;
	}

	@Override
	public void onActivityResumed(Activity activity) {
		if (activity.getClass().equals(ActivityMain.class))
			mResumed++;
	}

	@Override
	public void onActivityDestroyed(Activity activity) {
		if (activity.getClass().equals(ActivityMain.class))
			mCreated--;
	}

	@Override
	public void onActivityStopped(Activity activity) {
		if (activity.getClass().equals(ActivityMain.class))
			mStarted--;
	}

	@Override
	public void onActivityPaused(Activity activity) {
		if (activity.getClass().equals(ActivityMain.class))
			mResumed--;
	}

	@Override
	public void onActivitySaveInstanceState(Activity activity, Bundle bundle) {
	}
}

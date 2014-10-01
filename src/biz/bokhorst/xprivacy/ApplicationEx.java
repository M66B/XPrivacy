package biz.bokhorst.xprivacy;

import android.app.Application;
import android.util.Log;

public class ApplicationEx extends Application {
	private Thread.UncaughtExceptionHandler mPrevHandler;

	@Override
	public void onCreate() {
		super.onCreate();

		Util.log(null, Log.WARN, "UI started");
		mPrevHandler = Thread.getDefaultUncaughtExceptionHandler();
		Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
			@Override
			public void uncaughtException(Thread thread, Throwable ex) {
				Util.bug(null, ex);
				if (mPrevHandler != null)
					mPrevHandler.uncaughtException(thread, ex);
			}
		});
	}

	public void onDestroy() {
		Util.log(null, Log.WARN, "UI stopped");
	}
}

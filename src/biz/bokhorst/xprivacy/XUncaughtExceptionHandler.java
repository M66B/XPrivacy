package biz.bokhorst.xprivacy;

import android.app.Application;
import android.content.Context;

public class XUncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {

	private static Context mContext;

	public XUncaughtExceptionHandler(Application app) {
		mContext = app;
	}

	@Override
	public void uncaughtException(Thread arg0, Throwable arg1) {
		PrivacyManager.sendUsageData(null, mContext);
	}
}

package biz.bokhorst.xprivacy;

import android.app.Application;

public class ApplicationEx extends Application {
	@Override
	public void onCreate() {
		registerActivityLifecycleCallbacks(new LifecycleHandler());
	}
}

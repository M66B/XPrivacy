package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import android.util.Log;
import android.view.View;
import android.view.WindowManager;

public class XWindowManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.view.WindowManagerImpl";
	private static final Map<View, WindowManager.LayoutParams> mViewParam = new WeakHashMap<View, WindowManager.LayoutParams>();

	private XWindowManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public void addView(View view, ViewGroup.LayoutParams params)
	// public void removeView(View view)
	// public void updateViewLayout(View view, ViewGroup.LayoutParams params)
	// http://developer.android.com/reference/android/view/ViewManager.html
	// http://developer.android.com/reference/android/view/WindowManager.html

	// @formatter:on

	private enum Methods {
		addView, removeView, updateViewLayout
	};

	public static List<XHook> getInstances(String className, boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			listHook.add(new XWindowManager(Methods.addView, PrivacyManager.cOverlay, className));
			listHook.add(new XWindowManager(Methods.removeView, null, className));
			listHook.add(new XWindowManager(Methods.updateViewLayout, null, className));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.addView || mMethod == Methods.removeView || mMethod == Methods.updateViewLayout) {
			View view = (View) param.args[0];
			if (view != null) {
				// Get params
				WindowManager.LayoutParams wmParams = null;
				synchronized (mViewParam) {
					if (param.args.length > 1) {
						wmParams = (WindowManager.LayoutParams) param.args[1];
						if (wmParams != null)
							mViewParam.put(view, wmParams);
					} else if (mViewParam.containsKey(view))
						wmParams = mViewParam.get(view);
				}

				// Check for system alert/overlay
				if (wmParams != null)
					if (wmParams.type == WindowManager.LayoutParams.TYPE_SYSTEM_ALERT
							|| wmParams.type == WindowManager.LayoutParams.TYPE_SYSTEM_OVERLAY)
						if (mMethod == Methods.removeView || mMethod == Methods.updateViewLayout) {
							if (isRestricted(param, PrivacyManager.cOverlay, "addView"))
								param.setResult(null);
						}

						else if (isRestricted(param))
							param.setResult(null);
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

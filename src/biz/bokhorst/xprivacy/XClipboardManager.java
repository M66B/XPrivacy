package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XClipboardManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.content.ClipboardManager";

	private XClipboardManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	private XClipboardManager(Methods method, String restrictionName, String className, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public void addPrimaryClipChangedListener(OnPrimaryClipChangedListener what)
	// public ClipData getPrimaryClip()
	// public ClipDescription getPrimaryClipDescription()
	// public CharSequence getText()
	// public boolean hasPrimaryClip()
	// public boolean hasText()
	// public void removePrimaryClipChangedListener(ClipboardManager.OnPrimaryClipChangedListener what)
	// frameworks/base/core/java/android/content/ClipboardManager.java
	// http://developer.android.com/reference/android/content/ClipboardManager.html

	// @formatter:on

	private enum Methods {
		addPrimaryClipChangedListener, getPrimaryClip, getPrimaryClipDescription, getText, hasPrimaryClip, hasText, removePrimaryClipChangedListener
	};

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			for (Methods clip : Methods.values())
				if (clip == Methods.removePrimaryClipChangedListener)
					listHook.add(new XClipboardManager(clip, null, className, 11));
				else
					listHook.add(new XClipboardManager(clip, PrivacyManager.cClipboard, className));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.addPrimaryClipChangedListener) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.removePrimaryClipChangedListener)
			if (isRestricted(param, PrivacyManager.cClipboard, "addPrimaryClipChangedListener"))
				param.setResult(null);
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.addPrimaryClipChangedListener || mMethod == Methods.removePrimaryClipChangedListener)
			;

		else if (mMethod == Methods.getPrimaryClip || mMethod == Methods.getPrimaryClipDescription
				|| mMethod == Methods.getText) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.hasPrimaryClip || mMethod == Methods.hasText) {
			if (isRestricted(param))
				param.setResult(false);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

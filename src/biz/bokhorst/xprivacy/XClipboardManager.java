package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XClipboardManager extends XHook {
	private Methods mMethod;

	private XClipboardManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.content.ClipboardManager";
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

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods clip : Methods.values())
			listHook.add(new XClipboardManager(clip, PrivacyManager.cClipboard));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.addPrimaryClipChangedListener || mMethod == Methods.removePrimaryClipChangedListener)
			if (isRestricted(param))
				param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getPrimaryClip || mMethod == Methods.getPrimaryClipDescription
				|| mMethod == Methods.getText) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(null);
		} else if (mMethod == Methods.hasPrimaryClip || mMethod == Methods.hasText) {
			if (isRestricted(param))
				param.setResult(false);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

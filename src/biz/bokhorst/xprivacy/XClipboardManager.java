package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.content.Context;
import android.os.Binder;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XClipboardManager extends XHook {

	public XClipboardManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// @formatter:off

	// public void addPrimaryClipChangedListener(OnPrimaryClipChangedListener what)
	// public ClipData getPrimaryClip()
	// public ClipDescription getPrimaryClipDescription()
	// public CharSequence getText()
	// public boolean hasPrimaryClip()
	// public boolean hasText()
	// frameworks/base/core/java/android/content/ClipboardManager.java
	// http://developer.android.com/reference/android/content/ClipboardManager.html

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("addPrimaryClipChangedListener"))
			if (isRestricted(param))
				param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			String methodName = param.method.getName();
			if (methodName.equals("hasPrimaryClip") || methodName.equals("hasText"))
				param.setResult(false);
			else
				param.setResult(null);
		}
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

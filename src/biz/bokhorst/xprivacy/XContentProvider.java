package biz.bokhorst.xprivacy;

import android.content.ContentProvider;
import android.content.Context;
import android.database.Cursor;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContentProvider extends XHook {

	public XContentProvider(String restrictionName, String[] permissions) {
		super("query", restrictionName, permissions);
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Return empty cursor
		Cursor cursor = (Cursor) param.getResultOrThrowable();
		if (cursor != null)
			if (isRestricted(param))
				param.setResult(new XCursor(cursor));
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		ContentProvider contentProvider = (ContentProvider) param.thisObject;
		Context context = contentProvider.getContext();
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

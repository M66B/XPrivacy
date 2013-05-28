package biz.bokhorst.xprivacy;

import android.content.ContentProvider;
import android.database.Cursor;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContactProvider2query extends XHook {

	public final static String cPermissionName = "Contacts";

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Check if allowed
		ContentProvider contentProvider = (ContentProvider) param.thisObject;
		if (!isAllowed(contentProvider.getContext(), Binder.getCallingUid(), cPermissionName)) {
			// Return empty cursor
			info("deny");
			Cursor cursor = (Cursor) param.getResult();
			if (cursor != null)
				param.setResult(new XCursor(cursor));
		}
	}
}

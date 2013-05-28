package biz.bokhorst.xprivacy;

import android.content.ContentProvider;
import android.database.Cursor;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContentProvider extends XHook {

	private String mPermissionName;

	public XContentProvider(String permissionName) {
		mPermissionName = permissionName;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Check if allowed
		ContentProvider contentProvider = (ContentProvider) param.thisObject;
		if (!isAllowed(contentProvider.getContext(), Binder.getCallingUid(), mPermissionName)) {
			// Return empty cursor
			Cursor cursor = (Cursor) param.getResult();
			if (cursor != null)
				param.setResult(new XCursor(cursor));
		}
	}
}

package biz.bokhorst.xprivacy;

import android.database.Cursor;
import android.net.Uri;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContactProvider2query extends XHook {

	public final static String cPermissions = "XPrivacy.Contacts";

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Log uri
		Uri uri = (Uri) param.args[0];
		info("uri=" + uri);

		// Check if allowed
		if (!isAllowed(Binder.getCallingUid(), cPermissions)) {
			// Return empty cursor
			info("deny");
			Cursor cursor = (Cursor) param.getResult();
			if (cursor != null)
				param.setResult(new XCursor(cursor));
		}
	}
}

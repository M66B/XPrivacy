package biz.bokhorst.xprivacy;

import android.content.ContentProvider;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContactProvider2query extends XHook {

	public final static String cPropertyDeny = "XPrivacy.Contacts.Deny";

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Get uri
		Uri uri = (Uri) param.args[0];
		info("Uri=" + uri);

		// Get context
		ContentProvider contentProvider = (ContentProvider) param.thisObject;
		Context context = getContext(contentProvider);

		// Get uid of calling process
		int uid = Binder.getCallingUid();

		// Check if denied
		if (isCallingPackage(context, uid, cPropertyDeny)) {
			// Return empty cursor
			info("deny " + context.getPackageName());
			Cursor cursor = (Cursor) param.getResult();
			if (cursor != null)
				param.setResult(new XCursor(cursor));
		}
	}
}

package biz.bokhorst.xprivacy;

import android.content.ContentProvider;
import android.database.Cursor;
import android.net.Uri;

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
		XUtil.info(this, "Uri=" + uri);

		// Check if Facebook app
		if (XUtil.isCallingPackage(this, (ContentProvider) param.thisObject, cPropertyDeny)) {
			// Return empty cursor
			XUtil.info(this, "returning XCursor");
			Cursor cursor = (Cursor) param.getResult();
			if (cursor != null)
				param.setResult(new XCursor(cursor));
		}
	}
}

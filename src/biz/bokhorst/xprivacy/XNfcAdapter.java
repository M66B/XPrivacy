package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XNfcAdapter extends XHook {

	protected XNfcAdapter(String restrictionName, String methodName) {
		super(restrictionName, methodName, null);
	}

	@Override
	public String getClassName() {
		return "android.nfc.NfcAdapter";
	}

	// public static synchronized NfcAdapter getNfcAdapter(Context context)
	// frameworks/base/core/java/android/nfc/NfcAdapter.java
	// http://developer.android.com/reference/android/nfc/NfcAdapter.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XNfcAdapter(PrivacyManager.cNfc, "getNfcAdapter"));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param))
			param.setResult(null);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		if (param.args.length > 0)
			context = (Context) param.args[0];
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}

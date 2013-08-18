package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XNfcAdapter extends XHook {

	protected XNfcAdapter(String restrictionName, String methodName) {
		super(restrictionName, methodName, null);
	}

	@Override
	public String getClassName() {
		return "android.nfc.NfcAdapter";
	}

	// public static NfcAdapter getDefaultAdapter(Context context)
	// public static NfcAdapter getDefaultAdapter() (removed from SDK 16)
	// frameworks/base/core/java/android/nfc/NfcAdapter.java
	// http://developer.android.com/reference/android/nfc/NfcAdapter.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XNfcAdapter(PrivacyManager.cNfc, "getDefaultAdapter"));
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
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XNfcAdapter extends XHook {
	private Methods mMethod;

	protected XNfcAdapter(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	@Override
	public String getClassName() {
		return "android.nfc.NfcAdapter";
	}

	private enum Methods {
		getDefaultAdapter, getNfcAdapter
	};

	// public static NfcAdapter getDefaultAdapter() [deprecated]
	// public static NfcAdapter getDefaultAdapter(Context context)
	// public static synchronized NfcAdapter getNfcAdapter(Context context)
	// frameworks/base/core/java/android/nfc/NfcAdapter.java
	// http://developer.android.com/reference/android/nfc/NfcAdapter.html

	// NfcManager.getDefaultAdapter calls NfcAdapter.getNfcAdapter
	// http://developer.android.com/reference/android/nfc/NfcManager.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XNfcAdapter(Methods.getDefaultAdapter, PrivacyManager.cNfc));
		listHook.add(new XNfcAdapter(Methods.getNfcAdapter, PrivacyManager.cNfc));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.getDefaultAdapter || mMethod == Methods.getNfcAdapter) {
			if (isRestricted(param))
				param.setResult(null);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.SyncAdapterType;
import android.content.SyncInfo;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XContentResolver extends XHook {
	private Methods mMethod;

	private XContentResolver(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.content.ContentResolver";
	}

	// public static SyncInfo getCurrentSync()
	// static List<SyncInfo> getCurrentSyncs()
	// static SyncAdapterType[] getSyncAdapterTypes()
	// http://developer.android.com/reference/android/content/ContentResolver.html

	private enum Methods {
		getCurrentSync, getCurrentSyncs, getSyncAdapterTypes
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XContentResolver(Methods.getCurrentSync, PrivacyManager.cAccounts));
		listHook.add(new XContentResolver(Methods.getCurrentSyncs, PrivacyManager.cAccounts));
		listHook.add(new XContentResolver(Methods.getSyncAdapterTypes, PrivacyManager.cAccounts));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getCurrentSync) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.getCurrentSyncs) {
			if (isRestricted(param))
				param.setResult(new ArrayList<SyncInfo>());

		} else if (mMethod == Methods.getSyncAdapterTypes) {
			if (isRestricted(param))
				param.setResult(new SyncAdapterType[0]);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}

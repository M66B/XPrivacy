package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.SyncInfo;
import android.os.Binder;
import android.os.Build;

public class XContentService extends XHook {
	private Methods mMethod;

	private XContentService(Methods method, String restrictionName) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.server.content.ContentService";
	}

	// @formatter:off

	// public List<SyncInfo> getCurrentSyncs()
	// public void registerContentObserver(android.net.Uri uri, boolean notifyForDescendants, android.database.IContentObserver observer, int userHandle)
	// public void unregisterContentObserver(android.database.IContentObserver observer)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.2.2_r1/android/content/ContentService.java

	// @formatter:on

	private enum Methods {
		getCurrentSyncs
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		if (isAOSP(Build.VERSION_CODES.KITKAT))
			listHook.add(new XContentService(Methods.getCurrentSyncs, PrivacyManager.cAccounts));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getCurrentSyncs:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					int uid = Binder.getCallingUid();
					List<SyncInfo> listSync = (List<SyncInfo>) param.getResult();
					List<SyncInfo> listFiltered = new ArrayList<SyncInfo>();
					for (SyncInfo sync : listSync)
						if (XAccountManager.isAccountAllowed(sync.account, uid))
							listFiltered.add(sync);
					param.setResult(listFiltered);
				}
			break;
		}
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.IBinder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XBinder extends XHook {
	private Methods mMethod;

	private XBinder(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return (mMethod == Methods.transact ? "android.os.BinderProxy" : "android.os.Binder");
	}

	public boolean isVisible() {
		return (mMethod != Methods.execTransact);
	}

	// @formatter:off

	// private boolean execTransact(int code, int dataObj, int replyObj, int flags)
	// public final boolean transact(int code, Parcel data, Parcel reply, int flags)
	// public native boolean transact(int code, Parcel data, Parcel reply, int flags)
	// frameworks/base/core/java/android/os/Binder.java
	// http://developer.android.com/reference/android/os/Binder.html

	// @formatter:on

	private enum Methods {
		execTransact, transact
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		//listHook.add(new XBinder(Methods.execTransact, null));
		//listHook.add(new XBinder(Methods.transact, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.execTransact) {
			// Entry point from android_util_Binder.cpp's onTransact
			int flags = (Integer) param.args[3];
			boolean ok = ((flags & 0x00010000) != 0);
			flags &= 0x0000000f;
			param.args[3] = flags;

			String name = null;
			try {
				IBinder b = (IBinder) param.thisObject;
				name = b.getInterfaceDescriptor();
			} catch (Throwable ex) {
				name = ex.getMessage();
			}

			Log.w("XPrivacy", "flags in ok=" + ok + " name=" + name);
		} else if (mMethod == Methods.transact) {
			int flags = (Integer) param.args[3];
			if (flags != 0 && flags != IBinder.FLAG_ONEWAY)
				Util.log(this, Log.WARN, "Flags=" + Integer.toHexString(flags));
			flags |= 0x00010000;
			param.args[3] = flags;
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

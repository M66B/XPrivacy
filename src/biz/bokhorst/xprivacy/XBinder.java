package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.os.IBinder;
import android.os.Parcel;
import android.os.Process;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XBinder extends XHook {
	private Methods mMethod;
	private static Method mObtain = null;

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

	// private boolean execTransact(int code, int dataObj, int replyObj,int flags)
	// public final boolean transact(int code, Parcel data, Parcel reply, int flags)
	// public native boolean transact(int code, Parcel data, Parcel reply, int flags)
	// frameworks/base/core/java/android/os/Binder.java
	// http://developer.android.com/reference/android/os/Binder.html

	// @formatter:on

	private enum Methods {
		execTransact, transact
	};

	public static List<XHook> getInstances() {
		try {
			// static protected final Parcel obtain(int obj)
			// frameworks/base/core/java/android/os/Parcel.java
			mObtain = Parcel.class.getDeclaredMethod("obtain", int.class);
			mObtain.setAccessible(true);
		} catch (NoSuchMethodException ex) {
			Util.bug(null, ex);
		}

		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XBinder(Methods.execTransact, null));
		listHook.add(new XBinder(Methods.transact, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.execTransact) {
			// Entry point from android_util_Binder.cpp's onTransact
			if (mObtain != null) {
				int code = (Integer) param.args[0];
				int dataObj = (Integer) param.args[1];
				Parcel data = (Parcel) mObtain.invoke(null, dataObj);
			}
		} else if (mMethod == Methods.transact) {
			try {
				int code = (Integer) param.args[0];
				Parcel data = (Parcel) param.args[1];
				IBinder binder = (IBinder) param.thisObject;
				Log.w("XPrivacy", "transact uid=" + Process.myUid() + " name=" + binder.getInterfaceDescriptor());
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}

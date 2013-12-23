package biz.bokhorst.xprivacy;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.os.IBinder;
import android.os.Parcel;
import android.os.Process;
import android.os.RemoteException;
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
				int replyObj = (Integer) param.args[2];
				int flags = (Integer) param.args[3];
				Parcel data = (Parcel) mObtain.invoke(null, dataObj);
				Parcel reply = (Parcel) mObtain.invoke(null, replyObj);
				IBinder binder = (IBinder) param.thisObject;
				//boolean result = execTransact(binder, code, data, reply, flags);
				//param.setResult(result);
			}
		} else if (mMethod == Methods.transact) {
			try {
				int code = (Integer) param.args[0];
				Parcel data = (Parcel) param.args[1];
				Parcel reply = (Parcel) param.args[2];
				int flags = (Integer) param.args[3];
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

	private boolean execTransact(IBinder binder, int code, Parcel data, Parcel reply, int flags) {
		boolean res = false;
		try {
			// protected boolean onTransact(int code, Parcel data, Parcel reply,
			// int flags)
			Method onTransact = binder.getClass().getMethod("onTransact", int.class, Parcel.class, Parcel.class,
					int.class);
			onTransact.setAccessible(true);
			try {
				res = (Boolean) onTransact.invoke(binder, code, data, reply, flags);
			} catch (InvocationTargetException ex) {
				Throwable target = ex.getTargetException();
				if (target == null)
					new RuntimeException(target);
				else if (target.getClass().equals(RemoteException.class))
					throw (RemoteException) target;
				else if (target.getClass().equals(RuntimeException.class))
					throw (RuntimeException) target;
				else if (target.getClass().equals(OutOfMemoryError.class))
					throw (OutOfMemoryError) target;
				else {
					Util.bug(this, target);
					throw new RuntimeException(target);
				}
			} catch (IllegalAccessException ex) {
				Util.bug(this, ex);
			}
		} catch (NoSuchMethodException ex) {
			Util.bug(this, ex);
		} catch (RemoteException e) {
			reply.setDataPosition(0);
			reply.writeException(e);
			res = true;
		} catch (RuntimeException e) {
			reply.setDataPosition(0);
			reply.writeException(e);
			res = true;
		} catch (OutOfMemoryError e) {
			RuntimeException re = new RuntimeException("Out of memory", e);
			reply.setDataPosition(0);
			reply.writeException(re);
			res = true;
		}
		reply.recycle();
		data.recycle();
		return res;
	}
}

package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.Inet4Address;
import java.net.InterfaceAddress;
import java.util.ArrayList;
import java.util.List;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.net.Uri;
import android.net.wifi.WifiInfo;
import android.os.Build;
import android.os.IBinder;
import android.text.TextUtils;
import android.util.Log;

public class Requirements {

	public static void check(final Context context) {
		// Check Android version
		if (Build.VERSION.SDK_INT != Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1
				&& Build.VERSION.SDK_INT != Build.VERSION_CODES.JELLY_BEAN
				&& Build.VERSION.SDK_INT != Build.VERSION_CODES.JELLY_BEAN_MR1
				&& Build.VERSION.SDK_INT != Build.VERSION_CODES.JELLY_BEAN_MR2
				&& Build.VERSION.SDK_INT != Build.VERSION_CODES.KITKAT) {
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
			alertDialogBuilder.setTitle(context.getString(R.string.app_name));
			alertDialogBuilder.setMessage(context.getString(R.string.app_wrongandroid));
			alertDialogBuilder.setIcon(Util.getThemed(context, R.attr.icon_launcher));
			alertDialogBuilder.setPositiveButton(context.getString(android.R.string.ok),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							Intent androidIntent = new Intent(Intent.ACTION_VIEW);
							androidIntent.setData(Uri.parse("https://github.com/M66B/XPrivacy#installation"));
							context.startActivity(androidIntent);
						}
					});
			AlertDialog alertDialog = alertDialogBuilder.create();
			alertDialog.show();
		}

		// Check Xposed version
		int xVersion = Util.getXposedAppProcessVersion();
		if (xVersion < PrivacyManager.cXposedAppProcessMinVersion) {
			String msg = String.format(context.getString(R.string.app_notxposed),
					PrivacyManager.cXposedAppProcessMinVersion);
			Util.log(null, Log.WARN, msg);

			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
			alertDialogBuilder.setTitle(context.getString(R.string.app_name));
			alertDialogBuilder.setMessage(msg);
			alertDialogBuilder.setIcon(Util.getThemed(context, R.attr.icon_launcher));
			alertDialogBuilder.setPositiveButton(context.getString(android.R.string.ok),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							Intent xposedIntent = new Intent(Intent.ACTION_VIEW);
							xposedIntent.setData(Uri.parse("http://forum.xda-developers.com/showthread.php?t=1574401"));
							context.startActivity(xposedIntent);
						}
					});
			AlertDialog alertDialog = alertDialogBuilder.create();
			alertDialog.show();
		}

		// Check if XPrivacy is enabled
		if (!Util.isXposedEnabled()) {
			String msg = context.getString(R.string.app_notenabled);
			Util.log(null, Log.WARN, msg);

			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
			alertDialogBuilder.setTitle(context.getString(R.string.app_name));
			alertDialogBuilder.setMessage(msg);
			alertDialogBuilder.setIcon(Util.getThemed(context, R.attr.icon_launcher));
			alertDialogBuilder.setPositiveButton(context.getString(android.R.string.ok),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							Intent xInstallerIntent = context.getPackageManager().getLaunchIntentForPackage(
									"de.robv.android.xposed.installer");
							if (xInstallerIntent != null)
								context.startActivity(xInstallerIntent);
						}
					});
			AlertDialog alertDialog = alertDialogBuilder.create();
			alertDialog.show();
		}

		// Check pro enabler
		Version version = Util.getProEnablerVersion(context);
		if (version != null && !Util.isValidProEnablerVersion(version)) {
			String msg = context.getString(R.string.app_wrongenabler);
			Util.log(null, Log.WARN, msg);

			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
			alertDialogBuilder.setTitle(context.getString(R.string.app_name));
			alertDialogBuilder.setMessage(msg);
			alertDialogBuilder.setIcon(Util.getThemed(context, R.attr.icon_launcher));
			alertDialogBuilder.setPositiveButton(context.getString(android.R.string.ok),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							Intent storeIntent = new Intent(Intent.ACTION_VIEW, Uri.parse("market://details?id="
									+ context.getPackageName() + ".pro"));
							context.startActivity(storeIntent);
						}
					});
			AlertDialog alertDialog = alertDialogBuilder.create();
			alertDialog.show();
		}

		// TODO: account manager

		// Check activity manager
		if (!checkField(context.getSystemService(Context.ACTIVITY_SERVICE), "mContext", Context.class))
			reportClass(context.getSystemService(Context.ACTIVITY_SERVICE).getClass(), context);

		// Check activity thread
		try {
			Class<?> clazz = Class.forName("android.app.ActivityThread");
			try {
				clazz.getDeclaredMethod("unscheduleGcIdler");
			} catch (NoSuchMethodException ex) {
				reportClass(clazz, context);
			}
		} catch (ClassNotFoundException ex) {
			sendSupportInfo(ex.toString(), context);
		}

		// Check activity thread receiver data
		try {
			Class<?> clazz = Class.forName("android.app.ActivityThread$ReceiverData");
			if (!checkField(clazz, "intent"))
				reportClass(clazz, context);
		} catch (ClassNotFoundException ex) {
			try {
				reportClass(Class.forName("android.app.ActivityThread"), context);
			} catch (ClassNotFoundException exex) {
				sendSupportInfo(exex.toString(), context);
			}
		}

		// Check clipboard manager
		if (!checkField(context.getSystemService(Context.CLIPBOARD_SERVICE), "mContext", Context.class))
			reportClass(context.getSystemService(Context.CLIPBOARD_SERVICE).getClass(), context);

		// Check content resolver
		if (!checkField(context.getContentResolver(), "mContext", Context.class))
			reportClass(context.getContentResolver().getClass(), context);

		// Check interface address
		if (!checkField(InterfaceAddress.class, "address") || !checkField(InterfaceAddress.class, "broadcastAddress")
				|| PrivacyManager.getDefacedProp(0, "InetAddress") == null)
			reportClass(InterfaceAddress.class, context);

		// Check package manager
		if (!checkField(context.getPackageManager(), "mContext", Context.class))
			reportClass(context.getPackageManager().getClass(), context);

		// Check package manager service
		try {
			Class<?> clazz = Class.forName("com.android.server.pm.PackageManagerService");
			try {
				if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)
					clazz.getDeclaredMethod("getPackageUid", String.class, int.class);
				else
					clazz.getDeclaredMethod("getPackageUid", String.class);
			} catch (NoSuchMethodException ex) {
				reportClass(clazz, context);
			}
		} catch (ClassNotFoundException ex) {
			sendSupportInfo(ex.toString(), context);
		}

		// Check runtime
		try {
			Runtime.class.getDeclaredMethod("load", String.class, ClassLoader.class);
			Runtime.class.getDeclaredMethod("loadLibrary", String.class, ClassLoader.class);
		} catch (NoSuchMethodException ex) {
			reportClass(Runtime.class, context);
		}

		// Check service manager
		try {
			Class<?> clazz = Class.forName("android.os.ServiceManager");
			try {
				// public static String[] listServices()
				// public static IBinder checkService(String name)
				Method listServices = clazz.getDeclaredMethod("listServices");
				Method checkService = clazz.getDeclaredMethod("checkService", String.class);

				// Get services
				List<String> listService = new ArrayList<String>();
				for (String service : (String[]) listServices.invoke(null)) {
					IBinder binder = (IBinder) checkService.invoke(null, service);
					String serviceName = binder.getInterfaceDescriptor();
					if (!"".equals(serviceName))
						listService.add(serviceName);
				}
				for (String service : XBinder.cListService) {
					if (!listService.contains(service))
						sendSupportInfo(TextUtils.join("\r\n", listService), context);
				}
			} catch (NoSuchMethodException ex) {
				reportClass(clazz, context);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		} catch (ClassNotFoundException ex) {
			sendSupportInfo(ex.toString(), context);
		}

		// Check telephony manager
		if (!checkField(context.getSystemService(Context.TELEPHONY_SERVICE), "sContext", Context.class)
				&& !checkField(context.getSystemService(Context.TELEPHONY_SERVICE), "mContext", Context.class)
				&& !checkField(context.getSystemService(Context.TELEPHONY_SERVICE), "sContextDuos", Context.class))
			reportClass(context.getSystemService(Context.TELEPHONY_SERVICE).getClass(), context);

		// Check wifi info
		if (!checkField(WifiInfo.class, "mSupplicantState") || !checkField(WifiInfo.class, "mBSSID")
				|| !checkField(WifiInfo.class, "mIpAddress") || !checkField(WifiInfo.class, "mMacAddress")
				|| !(checkField(WifiInfo.class, "mSSID") || checkField(WifiInfo.class, "mWifiSsid")))
			reportClass(WifiInfo.class, context);

		// Check mWifiSsid.octets
		if (checkField(WifiInfo.class, "mWifiSsid"))
			try {
				Class<?> clazz = Class.forName("android.net.wifi.WifiSsid");
				try {
					clazz.getDeclaredMethod("createFromAsciiEncoded", String.class);
				} catch (NoSuchMethodException ex) {
					reportClass(clazz, context);
				}
			} catch (ClassNotFoundException ex) {
				sendSupportInfo(ex.toString(), context);
			}

		// Check Inet4Address/ANY
		try {
			Inet4Address.class.getDeclaredField("ANY");
		} catch (Throwable ex) {
			reportClass(Inet4Address.class, context);
		}
	}

	private static boolean checkField(Object obj, String fieldName, Class<?> expectedClass) {
		try {
			// Find field
			Field field = null;
			Class<?> superClass = (obj == null ? null : obj.getClass());
			while (superClass != null)
				try {
					field = superClass.getDeclaredField(fieldName);
					field.setAccessible(true);
					break;
				} catch (NoSuchFieldException ex) {
					superClass = superClass.getSuperclass();
				}

			// Check field
			if (field != null) {
				Object value = field.get(obj);
				if (value == null || expectedClass.isAssignableFrom(value.getClass()))
					return true;
			}
		} catch (Throwable ex) {
		}
		return false;
	}

	private static boolean checkField(Class<?> clazz, String fieldName) {
		try {
			clazz.getDeclaredField(fieldName);
			return true;
		} catch (NoSuchFieldException ex) {
			return false;
		}
	}

	private static void reportClass(final Class<?> clazz, final Context context) {
		String msg = String.format("Incompatible %s", clazz.getName());
		Util.log(null, Log.WARN, msg);

		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
		alertDialogBuilder.setTitle(context.getString(R.string.app_name));
		alertDialogBuilder.setMessage(msg);
		alertDialogBuilder.setIcon(Util.getThemed(context, R.attr.icon_launcher));
		alertDialogBuilder.setPositiveButton(context.getString(android.R.string.ok),
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						sendClassInfo(clazz, context);
					}
				});
		alertDialogBuilder.setNegativeButton(context.getString(android.R.string.cancel),
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						dialog.dismiss();
					}
				});
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private static void sendClassInfo(Class<?> clazz, Context context) {
		StringBuilder sb = new StringBuilder();
		sb.append(clazz.getName());
		sb.append("\r\n");
		sb.append("\r\n");
		for (Constructor<?> constructor : clazz.getConstructors()) {
			sb.append(constructor.toString());
			sb.append("\r\n");
		}
		sb.append("\r\n");
		for (Method method : clazz.getDeclaredMethods()) {
			sb.append(method.toString());
			sb.append("\r\n");
		}
		sb.append("\r\n");
		for (Field field : clazz.getDeclaredFields()) {
			sb.append(field.toString());
			sb.append("\r\n");
		}
		sb.append("\r\n");
		sendSupportInfo(sb.toString(), context);
	}

	public static void sendSupportInfo(String text, Context context) {
		String xversion = null;
		try {
			PackageInfo pInfo = context.getPackageManager().getPackageInfo(context.getPackageName(), 0);
			xversion = pInfo.versionName;
		} catch (Throwable ex) {
		}

		StringBuilder sb = new StringBuilder(text);
		sb.insert(0, "\r\n");
		sb.insert(0, String.format("Model: %s (%s)\r\n", Build.MODEL, Build.PRODUCT));
		sb.insert(0, String.format("Android SDK int: %d\r\n", Build.VERSION.SDK_INT));
		sb.insert(0, String.format("XPrivacy version: %s\r\n", xversion));

		Intent sendEmail = new Intent(Intent.ACTION_SEND);
		sendEmail.setType("message/rfc822");
		sendEmail.putExtra(Intent.EXTRA_EMAIL, new String[] { "marcel+xprivacy@faircode.eu" });
		sendEmail.putExtra(Intent.EXTRA_SUBJECT, "XPrivacy support info");
		sendEmail.putExtra(Intent.EXTRA_TEXT, sb.toString());
		sendEmail.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(Util.getDataFile()));
		try {
			context.startActivity(sendEmail);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}
}

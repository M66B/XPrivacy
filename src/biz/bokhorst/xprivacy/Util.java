package biz.bokhorst.xprivacy;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.lang.RuntimeException;
import java.nio.channels.FileChannel;
import java.security.KeyFactory;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.os.Process;
import android.os.RemoteException;
import android.os.TransactionTooLargeException;
import android.os.UserHandle;
import android.util.Base64;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

public class Util {
	private static boolean mPro = false;
	private static boolean mLog = true;
	private static boolean mLogDetermined = false;
	private static Boolean mHasLBE = null;

	private static Version MIN_PRO_VERSION = new Version("1.12");
	private static String LICENSE_FILE_NAME = "XPrivacy_license.txt";

	public static int NOTIFY_RESTART = 0;
	public static int NOTIFY_NOTXPOSED = 1;
	public static int NOTIFY_SERVICE = 2;
	public static int NOTIFY_MIGRATE = 3;
	public static int NOTIFY_RANDOMIZE = 4;
	public static int NOTIFY_UPGRADE = 5;

	public static void log(XHook hook, int priority, String msg) {
		// Check if logging enabled
		int uid = Process.myUid();
		if (!mLogDetermined && uid > 0) {
			mLogDetermined = true;
			try {
				mLog = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingLog, false);
			} catch (Throwable ignored) {
				mLog = false;
			}
		}

		// Log if enabled
		if (priority != Log.DEBUG && (priority == Log.INFO ? mLog : true))
			if (hook == null)
				Log.println(priority, "XPrivacy", msg);
			else
				Log.println(priority, String.format("XPrivacy/%s", hook.getClass().getSimpleName()), msg);

		// Report to service
		if (uid > 0 && priority == Log.ERROR)
			if (PrivacyService.isRegistered())
				PrivacyService.reportErrorInternal(msg);
			else
				try {
					IPrivacyService client = PrivacyService.getClient();
					if (client != null)
						client.reportError(msg);
				} catch (RemoteException ignored) {
				}
	}

	public static void bug(XHook hook, Throwable ex) {
		int priority;
		if (ex instanceof RuntimeException)
			priority = Log.WARN;
		else if (ex instanceof OutOfMemoryError)
			priority = Log.WARN;
		else if (ex instanceof ActivityShare.AbortException)
			priority = Log.WARN;
		else if (ex instanceof ActivityShare.ServerException)
			priority = Log.WARN;
		else if (ex instanceof NoClassDefFoundError)
			priority = Log.WARN;
		else if (ex instanceof SecurityException)
			priority = Log.WARN;
		else if (ex instanceof TransactionTooLargeException)
			priority = Log.WARN;
		else
			priority = Log.ERROR;

		boolean xprivacy = false;
		for (StackTraceElement frame : ex.getStackTrace())
			if (frame.getClassName() != null && frame.getClassName().startsWith("biz.bokhorst.xprivacy")) {
				xprivacy = true;
				break;
			}
		if (!xprivacy)
			priority = Log.WARN;

		log(hook, priority, ex.toString() + " uid=" + Process.myUid() + "\n" + Log.getStackTraceString(ex));
	}

	public static void logStack(XHook hook, int priority) {
		logStack(hook, priority, false);
	}

	public static void logStack(XHook hook, int priority, boolean cl) {
		StringBuilder trace = new StringBuilder();
		ClassLoader loader = Thread.currentThread().getContextClassLoader();
		for (StackTraceElement ste : Thread.currentThread().getStackTrace()) {
			trace.append(ste.toString());
			if (cl)
				try {
					Class<?> clazz = Class.forName(ste.getClassName(), false, loader);
					trace.append(" [");
					trace.append(clazz.getClassLoader().toString());
					trace.append("]");
				} catch (ClassNotFoundException ignored) {
				}
			trace.append("\n");
		}
		log(hook, priority, trace.toString());
	}

	public static int getXposedAppProcessVersion() {
		final Pattern PATTERN_APP_PROCESS_VERSION = Pattern.compile(".*with Xposed support \\(version (.+)\\).*");
		try {
			InputStream is = new FileInputStream("/system/bin/app_process");
			BufferedReader br = new BufferedReader(new InputStreamReader(is));
			String line;
			while ((line = br.readLine()) != null) {
				if (!line.contains("Xposed"))
					continue;
				Matcher m = PATTERN_APP_PROCESS_VERSION.matcher(line);
				if (m.find()) {
					br.close();
					is.close();
					return Integer.parseInt(m.group(1));
				}
			}
			br.close();
			is.close();
		} catch (Throwable ex) {
		}
		return -1;
	}

	public static boolean isXposedEnabled() {
		// Will be hooked to return true
		log(null, Log.WARN, "XPrivacy not enabled");
		return false;
	}

	public static void setPro(boolean enabled) {
		mPro = enabled;
	}

	public static boolean isProEnabled() {
		return mPro;
	}

	public static String hasProLicense(Context context) {
		try {
			// Get license
			String[] license = getProLicenseUnchecked();
			if (license == null)
				return null;
			String name = license[0];
			String email = license[1];
			String signature = license[2];

			// Get bytes
			byte[] bEmail = email.getBytes("UTF-8");
			byte[] bSignature = hex2bytes(signature);
			if (bEmail.length == 0 || bSignature.length == 0) {
				Util.log(null, Log.ERROR, "Licensing: invalid file");
				return null;
			}

			// Verify license
			boolean licensed = verifyData(bEmail, bSignature, getPublicKey(context));
			if (licensed)
				Util.log(null, Log.INFO, "Licensing: ok");
			else
				Util.log(null, Log.ERROR, "Licensing: invalid");

			// Return result
			if (licensed)
				return name;
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
		return null;
	}

	@SuppressLint("NewApi")
	public static int getAppId(int uid) {
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1)
			try {
				// TODO: update by method in SDK 20
				// UserHandle: public static final int getAppId(int uid)
				Method method = (Method) UserHandle.class.getDeclaredMethod("getAppId", int.class);
				uid = (Integer) method.invoke(null, uid);
			} catch (Throwable ex) {
				Util.log(null, Log.WARN, ex.toString());
			}
		return uid;
	}

	@SuppressLint("NewApi")
	public static int getUserId(int uid) {
		int userId = 0;
		if (uid > 99) {
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1)
				try {
					// TODO: update by method in SDK 20
					// UserHandle: public static final int getUserId(int uid)
					Method method = (Method) UserHandle.class.getDeclaredMethod("getUserId", int.class);
					userId = (Integer) method.invoke(null, uid);
				} catch (Throwable ex) {
					Util.log(null, Log.WARN, ex.toString());
				}
		} else
			userId = uid;
		return userId;
	}

	public static String getUserDataDirectory(int uid) {
		// Build data directory
		String dataDir = Environment.getDataDirectory() + File.separator;
		int userId = getUserId(uid);
		if (userId == 0)
			dataDir += "data";
		else
			dataDir += "user" + File.separator + userId;
		dataDir += File.separator + Util.class.getPackage().getName();
		return dataDir;
	}

	public static String[] getProLicenseUnchecked() {
		// Get license file name
		String storageDir = Environment.getExternalStorageDirectory().getAbsolutePath();
		File licenseFile = new File(storageDir + File.separator + LICENSE_FILE_NAME);
		if (!licenseFile.exists())
			licenseFile = new File(storageDir + File.separator + ".xprivacy" + File.separator + LICENSE_FILE_NAME);

		String importedLicense = importProLicense(licenseFile);
		if (importedLicense == null)
			return null;

		// Check license file
		licenseFile = new File(importedLicense);
		if (licenseFile.exists()) {
			// Read license
			try {
				IniFile iniFile = new IniFile(licenseFile);
				String name = iniFile.get("name", "");
				String email = iniFile.get("email", "");
				String signature = iniFile.get("signature", "");
				if (name == null || email == null || signature == null)
					return null;
				else
					return new String[] { name, email, signature };
			} catch (FileNotFoundException ex) {
				return null;
			} catch (Throwable ex) {
				bug(null, ex);
				return null;
			}
		} else
			Util.log(null, Log.INFO, "Licensing: no license file");
		return null;
	}

	public static String importProLicense(File licenseFile) {
		// Get imported license file name
		String importedLicense = getUserDataDirectory(Process.myUid()) + File.separator + LICENSE_FILE_NAME;
		File out = new File(importedLicense);

		// Check if license file exists
		if (licenseFile.exists() && licenseFile.canRead()) {
			try {
				// Import license file
				Util.log(null, Log.WARN, "Licensing: importing " + out.getAbsolutePath());
				InputStream is = null;
				is = new FileInputStream(licenseFile.getAbsolutePath());
				try {
					OutputStream os = null;
					try {
						os = new FileOutputStream(out.getAbsolutePath());
						byte[] buffer = new byte[1024];
						int read;
						while ((read = is.read(buffer)) != -1)
							os.write(buffer, 0, read);
						os.flush();
					} finally {
						if (os != null)
							os.close();
					}
				} finally {
					if (is != null)
						is.close();
				}

				// Protect imported license file
				setPermissions(out.getAbsolutePath(), 0700, Process.myUid(), Process.myUid());

				// Remove original license file
				licenseFile.delete();
			} catch (FileNotFoundException ignored) {
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		return (out.exists() && out.canRead() ? importedLicense : null);
	}

	public static Version getProEnablerVersion(Context context) {
		try {
			String proPackageName = context.getPackageName() + ".pro";
			PackageManager pm = context.getPackageManager();
			PackageInfo pi = pm.getPackageInfo(proPackageName, 0);
			return new Version(pi.versionName);
		} catch (NameNotFoundException ignored) {
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
		return null;
	}

	public static boolean isValidProEnablerVersion(Version version) {
		return (version.compareTo(MIN_PRO_VERSION) >= 0);
	}

	private static boolean hasValidProEnablerSignature(Context context) {
		return (context.getPackageManager()
				.checkSignatures(context.getPackageName(), context.getPackageName() + ".pro") == PackageManager.SIGNATURE_MATCH);
	}

	public static boolean isProEnablerInstalled(Context context) {
		Version version = getProEnablerVersion(context);
		if (version != null && isValidProEnablerVersion(version) && hasValidProEnablerSignature(context)) {
			Util.log(null, Log.INFO, "Licensing: enabler installed");
			return true;
		}
		Util.log(null, Log.INFO, "Licensing: enabler not installed");
		return false;
	}

	public static boolean hasMarketLink(Context context, String packageName) {
		try {
			PackageManager pm = context.getPackageManager();
			String installer = pm.getInstallerPackageName(packageName);
			if (installer != null)
				return installer.equals("com.android.vending") || installer.contains("google");
		} catch (Exception ex) {
			log(null, Log.WARN, ex.toString());
		}
		return false;
	}

	public static void viewUri(Context context, Uri uri) {
		Intent infoIntent = new Intent(Intent.ACTION_VIEW);
		infoIntent.setData(uri);
		if (isIntentAvailable(context, infoIntent))
			context.startActivity(infoIntent);
		else
			Toast.makeText(context, "View action not available", Toast.LENGTH_LONG).show();
	}

	public static boolean hasLBE() {
		if (mHasLBE == null) {
			mHasLBE = false;
			try {
				File apps = new File(Environment.getDataDirectory() + File.separator + "app");
				File[] files = (apps == null ? null : apps.listFiles());
				if (files != null)
					for (File file : files)
						if (file.getName().startsWith("com.lbe.security")) {
							mHasLBE = true;
							break;
						}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}
		return mHasLBE;
	}

	public static int getSelfVersionCode(Context context) {
		try {
			String self = Util.class.getPackage().getName();
			PackageManager pm = context.getPackageManager();
			PackageInfo pInfo = pm.getPackageInfo(self, 0);
			return pInfo.versionCode;
		} catch (NameNotFoundException ex) {
			Util.bug(null, ex);
			return 0;
		}
	}

	public static String getSelfVersionName(Context context) {
		try {
			String self = Util.class.getPackage().getName();
			PackageManager pm = context.getPackageManager();
			PackageInfo pInfo = pm.getPackageInfo(self, 0);
			return pInfo.versionName;
		} catch (NameNotFoundException ex) {
			Util.bug(null, ex);
			return null;
		}
	}

	private static byte[] hex2bytes(String hex) {
		// Convert hex string to byte array
		int len = hex.length();
		byte[] result = new byte[len / 2];
		for (int i = 0; i < len; i += 2)
			result[i / 2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4) + Character.digit(hex.charAt(i + 1), 16));
		return result;
	}

	private static PublicKey getPublicKey(Context context) throws Throwable {
		// Read public key
		String sPublicKey = "";
		InputStreamReader isr = new InputStreamReader(context.getAssets().open("XPrivacy_public_key.txt"), "UTF-8");
		BufferedReader br = new BufferedReader(isr);
		String line = br.readLine();
		while (line != null) {
			if (!line.startsWith("-----"))
				sPublicKey += line;
			line = br.readLine();
		}
		br.close();
		isr.close();

		// Create public key
		byte[] bPublicKey = Base64.decode(sPublicKey, Base64.NO_WRAP);
		KeyFactory keyFactory = KeyFactory.getInstance("RSA");
		X509EncodedKeySpec encodedPubKeySpec = new X509EncodedKeySpec(bPublicKey);
		return keyFactory.generatePublic(encodedPubKeySpec);
	}

	private static boolean verifyData(byte[] data, byte[] signature, PublicKey publicKey) throws Throwable {
		// Verify signature
		Signature verifier = Signature.getInstance("SHA1withRSA");
		verifier.initVerify(publicKey);
		verifier.update(data);
		return verifier.verify(signature);
	}

	public static String sha1(String text) throws NoSuchAlgorithmException, UnsupportedEncodingException {
		// SHA1
		int userId = Util.getUserId(Process.myUid());
		String salt = PrivacyManager.getSalt(userId);
		MessageDigest digest = MessageDigest.getInstance("SHA-1");
		byte[] bytes = (text + salt).getBytes("UTF-8");
		digest.update(bytes, 0, bytes.length);
		bytes = digest.digest();
		StringBuilder sb = new StringBuilder();
		for (byte b : bytes)
			sb.append(String.format("%02X", b));
		return sb.toString();
	}

	public static String md5(String text) throws NoSuchAlgorithmException, UnsupportedEncodingException {
		// MD5
		int userId = Util.getUserId(Process.myUid());
		String salt = PrivacyManager.getSalt(userId);
		byte[] bytes = MessageDigest.getInstance("MD5").digest((text + salt).getBytes("UTF-8"));
		StringBuilder sb = new StringBuilder();
		for (byte b : bytes)
			sb.append(String.format("%02X", b));
		return sb.toString();
	}

	@SuppressLint("DefaultLocale")
	public static boolean hasValidFingerPrint(Context context) {
		try {
			PackageManager pm = context.getPackageManager();
			String packageName = context.getPackageName();
			PackageInfo packageInfo = pm.getPackageInfo(packageName, PackageManager.GET_SIGNATURES);
			byte[] cert = packageInfo.signatures[0].toByteArray();
			MessageDigest digest = MessageDigest.getInstance("SHA1");
			byte[] bytes = digest.digest(cert);
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < bytes.length; ++i)
				sb.append((Integer.toHexString((bytes[i] & 0xFF) | 0x100)).substring(1, 3).toLowerCase());
			String calculated = sb.toString();
			String expected = context.getString(R.string.fingerprint);
			return calculated.equals(expected);
		} catch (Throwable ex) {
			bug(null, ex);
			return false;
		}
	}

	public static boolean isDebuggable(Context context) {
		return ((context.getApplicationContext().getApplicationInfo().flags & ApplicationInfo.FLAG_DEBUGGABLE) != 0);
	}

	public static boolean isIntentAvailable(Context context, Intent intent) {
		PackageManager packageManager = context.getPackageManager();
		return (packageManager.queryIntentActivities(intent, PackageManager.GET_ACTIVITIES).size() > 0);
	}

	public static void setPermissions(String path, int mode, int uid, int gid) {
		try {
			// frameworks/base/core/java/android/os/FileUtils.java
			Class<?> fileUtils = Class.forName("android.os.FileUtils");
			Method setPermissions = fileUtils
					.getMethod("setPermissions", String.class, int.class, int.class, int.class);
			setPermissions.invoke(null, path, mode, uid, gid);
			Util.log(null, Log.WARN, "Changed permission path=" + path + " mode=" + Integer.toOctalString(mode)
					+ " uid=" + uid + " gid=" + gid);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	public static void copy(File src, File dst) throws IOException {
		FileInputStream inStream = null;
		try {
			inStream = new FileInputStream(src);
			FileOutputStream outStream = null;
			try {
				outStream = new FileOutputStream(dst);
				FileChannel inChannel = inStream.getChannel();
				FileChannel outChannel = outStream.getChannel();
				inChannel.transferTo(0, inChannel.size(), outChannel);
			} finally {
				if (outStream != null)
					outStream.close();
			}
		} finally {
			if (inStream != null)
				inStream.close();
		}
	}

	public static boolean move(File src, File dst) {
		try {
			copy(src, dst);
		} catch (IOException ex) {
			Util.bug(null, ex);
			return false;
		}
		return src.delete();
	}

	public static List<View> getViewsByTag(ViewGroup root, String tag) {
		List<View> views = new ArrayList<View>();
		for (int i = 0; i < root.getChildCount(); i++) {
			View child = root.getChildAt(i);

			if (child instanceof ViewGroup)
				views.addAll(getViewsByTag((ViewGroup) child, tag));

			if (tag.equals(child.getTag()))
				views.add(child);
		}
		return views;
	}
}

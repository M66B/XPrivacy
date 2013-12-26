package biz.bokhorst.xprivacy;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.security.KeyFactory;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;
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
import android.graphics.Bitmap;
import android.graphics.Bitmap.Config;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.os.Environment;
import android.os.Process;
import android.os.UserHandle;
import android.util.Base64;
import android.util.Log;
import android.util.TypedValue;

public class Util {
	private static boolean mPro = false;
	private static boolean mLog = true;
	private static boolean mLogDetermined = false;
	private static Version MIN_PRO_VERSION = new Version("1.12");
	private static String LICENSE_FILE_NAME = "XPrivacy_license.txt";

	public static void log(XHook hook, int priority, String msg) {
		// Check if logging enabled
		if (Process.myUid() != 0 && !mLogDetermined) {
			mLog = false;
			mLogDetermined = true;
			mLog = PrivacyManager.getSettingBool(null, null, 0, PrivacyManager.cSettingLog, false, false);
		}

		// Log if enabled
		if (priority != Log.DEBUG && (priority == Log.INFO ? mLog : true))
			if (hook == null)
				Log.println(priority, "XPrivacy", msg);
			else
				Log.println(priority, String.format("XPrivacy/%s", hook.getClass().getSimpleName()), msg);
	}

	public static void bug(XHook hook, Throwable ex) {
		log(hook, Log.ERROR, ex.toString() + " uid=" + Process.myUid());
		ex.printStackTrace();
	}

	public static void logStack(XHook hook) {
		log(hook, Log.INFO, Log.getStackTraceString(new Exception("StackTrace")));
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

	public static String hasProLicense(Context context) {
		try {
			// Pro enabled
			if (mPro)
				return "";

			// Disable storage restriction
			PrivacyManager.setRestricted(null, context, Process.myUid(), PrivacyManager.cStorage, null, false);

			// Get license
			String[] license = getProLicense();
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
			if (licensed && (isDebuggable(context) || hasValidFingerPrint(context)))
				Util.log(null, Log.INFO, "Licensing: ok for " + name);
			else
				Util.log(null, Log.ERROR, "Licensing: invalid for " + name);

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
				// UserHandle: public static final int getAppId(int uid)
				Method method = (Method) UserHandle.class.getDeclaredMethod("getAppId", int.class);
				uid = (Integer) method.invoke(null, uid);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		return uid;
	}

	@SuppressLint("NewApi")
	public static int getUserId(int uid) {
		int userId = 0;
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1)
			try {
				// UserHandle: public static final int getUserId(int uid)
				Method method = (Method) UserHandle.class.getDeclaredMethod("getUserId", int.class);
				userId = (Integer) method.invoke(null, uid);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		return userId;
	}

	public static String getUserDataDirectory() {
		// Build data directory
		String dataDir = Environment.getDataDirectory() + File.separator;
		int userId = getUserId(Process.myUid());
		if (userId == 0)
			dataDir += "data";
		else
			dataDir += "user" + File.separator + userId;
		dataDir += File.separator + Util.class.getPackage().getName();
		return dataDir;
	}

	public static String[] getProLicense() {
		// Get license file name
		String storageDir = Environment.getExternalStorageDirectory().getAbsolutePath();
		File licenseFile = new File(storageDir + File.separator + LICENSE_FILE_NAME);
		if (!licenseFile.exists())
			licenseFile = new File(storageDir + File.separator + ".xprivacy" + File.separator + LICENSE_FILE_NAME);

		// Get imported license file name
		String importedLicense = getUserDataDirectory() + File.separator + LICENSE_FILE_NAME;

		// Import license file
		if (licenseFile.exists()) {
			try {
				File out = new File(importedLicense);
				Util.log(null, Log.WARN, "Licensing: importing " + out.getAbsolutePath());
				InputStream is = new FileInputStream(licenseFile.getAbsolutePath());
				OutputStream os = new FileOutputStream(out.getAbsolutePath());
				byte[] buffer = new byte[1024];
				int read;
				while ((read = is.read(buffer)) != -1)
					os.write(buffer, 0, read);
				is.close();
				os.flush();
				os.close();

				out.setWritable(false);
				licenseFile.delete();
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		// Check license file
		licenseFile = new File(importedLicense);
		if (licenseFile.exists()) {
			// Read license
			try {
				IniFile iniFile = new IniFile(licenseFile);
				String name = iniFile.get("name", "");
				String email = iniFile.get("email", "");
				String signature = iniFile.get("signature", "");
				return new String[] { name, email, signature };
			} catch (Throwable ex) {
				bug(null, ex);
				return null;
			}
		} else
			Util.log(null, Log.INFO, "Licensing: no license file");
		return null;
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
		if (version != null && isValidProEnablerVersion(version) && hasValidProEnablerSignature(context)
				&& hasValidFingerPrint(context)) {
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
		String salt = PrivacyManager.getSetting(null, null, 0, PrivacyManager.cSettingSalt, "", true);
		MessageDigest digest = MessageDigest.getInstance("SHA-1");
		byte[] bytes = (text + salt).getBytes("UTF-8");
		digest.update(bytes, 0, bytes.length);
		bytes = digest.digest();
		StringBuilder sb = new StringBuilder();
		for (byte b : bytes)
			sb.append(String.format("%02X", b));
		return sb.toString();
	}

	public static String md5(String string) throws NoSuchAlgorithmException, UnsupportedEncodingException {
		byte[] bytes = MessageDigest.getInstance("MD5").digest(string.getBytes("UTF-8"));
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
			boolean valid = calculated.equals(expected);
			if (valid)
				log(null, Log.INFO, "Valid fingerprint");
			else
				log(null, Log.ERROR, "Invalid fingerprint calculate=" + calculated + " expected=" + expected);
			return valid;
		} catch (Throwable ex) {
			bug(null, ex);
			return false;
		}
	}

	public static boolean isDebuggable(Context context) {
		return ((context.getApplicationContext().getApplicationInfo().flags & ApplicationInfo.FLAG_DEBUGGABLE) != 0);
	}

	public static boolean containsIgnoreCase(List<String> strings, String value) {
		for (String string : strings)
			if (string.equalsIgnoreCase(value))
				return true;
		return false;
	}

	public static boolean isIntentAvailable(Context context, Intent intent) {
		PackageManager packageManager = context.getPackageManager();
		return (packageManager.queryIntentActivities(intent, PackageManager.GET_ACTIVITIES).size() > 0);
	}

	public static void copy(File src, File dst) throws IOException {
		InputStream in = new FileInputStream(src);
		OutputStream out = new FileOutputStream(dst);
		byte[] buf = new byte[1024];
		int len;
		while ((len = in.read(buf)) > 0)
			out.write(buf, 0, len);
		in.close();
		out.close();
	}

	public static Bitmap[] getTriStateCheckBox(Context context) {
		Bitmap[] bitmap = new Bitmap[3];

		int size = 24;
		int border0 = 4;
		int border1 = border0 * 2;
		int border2 = border0;

		// Create off check box
		bitmap[0] = Bitmap.createBitmap(size, size, Config.ARGB_8888);
		Canvas canvas0 = new Canvas(bitmap[0]);
		Paint paint0 = new Paint();
		paint0.setStyle(Paint.Style.STROKE);
		paint0.setColor(Color.GRAY);
		paint0.setStrokeWidth(2);
		canvas0.drawRect(border0, border0, bitmap[0].getWidth() - border0, bitmap[0].getHeight() - border0, paint0);

		// Create half check box
		bitmap[1] = Bitmap.createBitmap(bitmap[0].getWidth(), bitmap[0].getHeight(), bitmap[0].getConfig());
		Canvas canvas1 = new Canvas(bitmap[1]);
		Paint paint1 = new Paint();
		paint1.setStyle(Paint.Style.FILL);
		paint1.setColor(Color.GRAY);
		canvas1.drawBitmap(bitmap[0], 0, 0, paint1);
		canvas1.drawRect(border1, border1, bitmap[1].getWidth() - border1, bitmap[1].getHeight() - border1, paint1);

		// Create full check box
		bitmap[2] = Bitmap.createBitmap(bitmap[0].getWidth(), bitmap[0].getHeight(), bitmap[0].getConfig());
		Canvas canvas2 = new Canvas(bitmap[2]);
		canvas2.drawBitmap(bitmap[0], 0, 0, new Paint());
		Drawable checkmark = context.getResources().getDrawable(getThemed(context, R.attr.icon_checked));
		checkmark.setBounds(border2, border2, bitmap[2].getWidth() - border2, bitmap[2].getHeight() - border2);
		checkmark.draw(canvas2);

		return bitmap;
	}

	public static int getThemed(Context context, int attr) {
		TypedValue tv = new TypedValue();
		context.getTheme().resolveAttribute(attr, tv, true);
		return tv.resourceId;
	}
}

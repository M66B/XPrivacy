package biz.bokhorst.xprivacy;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
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
import android.content.res.Resources;
import android.os.Environment;
import android.util.Base64;
import android.util.Log;

public class Util {
	private static boolean mPro = false;

	public static void log(XHook hook, int priority, String msg) {
		if (priority != Log.DEBUG)
			if (hook == null)
				Log.println(priority, "XPrivacy", msg);
			else
				Log.println(priority, String.format("XPrivacy/%s", hook.getClass().getSimpleName()), msg);
	}

	public static void bug(XHook hook, Throwable ex) {
		log(hook, Log.ERROR, ex.toString());
		ex.printStackTrace();
	}

	public static void logStack(XHook hook) {
		log(hook, Log.INFO, Log.getStackTraceString(new Exception("StackTrace")));
	}

	public static int getXposedVersion() {
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

			// Get license file name
			String folder = Environment.getExternalStorageDirectory().getAbsolutePath();
			String fileName = folder + File.separator + "XPrivacy_license.txt";
			File licenseFile = new File(fileName);
			if (!licenseFile.exists()) {
				fileName = folder + File.separator + ".xprivacy" + File.separator + "XPrivacy_license.txt";
				licenseFile = new File(fileName);
			}
			if (licenseFile.exists()) {
				// Read license
				IniFile iniFile = new IniFile(licenseFile);
				String name = iniFile.get("name", "");
				String email = iniFile.get("email", "");
				String signature = iniFile.get("signature", "");

				// Get bytes
				byte[] bEmail = email.getBytes("UTF-8");
				byte[] bSignature = hex2bytes(signature);
				if (bEmail.length == 0 || bSignature.length == 0) {
					Util.log(null, Log.ERROR, "Licensing: invalid file");
					return null;
				}

				// Verify license
				boolean licensed = verifyData(bEmail, bSignature, getPublicKey(context));
				if (licensed && (isDebug(context) || validFingerPrint(context)))
					Util.log(null, Log.INFO, "Licensing: ok for " + name + " (" + email + ")");
				else
					Util.log(null, Log.ERROR, "Licensing: invalid for " + name + " (" + email + ")");

				// Return result
				if (licensed)
					return name;
			} else
				Util.log(null, Log.INFO, "Licensing: no license folder=" + Environment.getExternalStorageDirectory());
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
		return null;
	}

	public static boolean isProInstalled(Context context) {
		try {
			String proPackageName = "biz.bokhorst.xprivacy.pro";
			PackageManager pm = context.getPackageManager();
			PackageInfo pi = pm.getPackageInfo(proPackageName, 0);
			Version vPro = new Version(pi.versionName);
			if (pm.checkSignatures(context.getPackageName(), proPackageName) == PackageManager.SIGNATURE_MATCH
					&& vPro.compareTo(new Version("1.7")) >= 0 && validFingerPrint(context)) {
				Util.log(null, Log.INFO, "Licensing: enabler installed");
				return true;
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
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
		String salt = PrivacyManager.getSetting(null, null, PrivacyManager.cSettingSalt, "", true);
		MessageDigest digest = MessageDigest.getInstance("SHA-1");
		byte[] bytes = (text + salt).getBytes("UTF-8");
		digest.update(bytes, 0, bytes.length);
		bytes = digest.digest();
		StringBuilder sb = new StringBuilder();
		for (byte b : bytes)
			sb.append(String.format("%02X", b));
		return sb.toString();
	}

	@SuppressLint("DefaultLocale")
	public static boolean validFingerPrint(Context context) {
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

	public static boolean isDebug(Context context) {
		return ((context.getApplicationContext().getApplicationInfo().flags & ApplicationInfo.FLAG_DEBUGGABLE) != 0);
	}

	public static Context getXContext(Context context) throws Throwable {
		String xPackageName = Util.class.getPackage().getName();
		return context.createPackageContext(xPackageName, 0);
	}

	public static Resources getXResources(Context context) throws Throwable {
		String xPackageName = Util.class.getPackage().getName();
		PackageManager pm = context.getPackageManager();
		return pm.getResourcesForApplication(xPackageName);
	}

	public static String getXString(Context context, int id) throws Throwable {
		return getXResources(context).getString(id);
	}

	public static boolean containsIgnoreCase(List<String> strings, String value) {
		for (String string : strings)
			if (string.equalsIgnoreCase(value))
				return true;
		return false;
	}

	public static boolean isIntentAvailable(Context context, String action) {
		PackageManager packageManager = context.getPackageManager();
		Intent intent = new Intent(action);
		return (packageManager.queryIntentActivities(intent, PackageManager.MATCH_DEFAULT_ONLY).size() > 0);
	}
}

package biz.bokhorst.xprivacy;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.content.Context;
import android.content.pm.PackageManager;
import android.content.res.Resources;
import android.os.Environment;
import android.util.Base64;
import android.util.Log;

public class Util {

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

	public static String isProVersion(Context context) {
		try {
			// Check for pro app
			String proPackageName = "biz.bokhorst.xprivacy.pro";
			PackageManager manager = context.getPackageManager();
			if (manager.checkSignatures(context.getPackageName(), proPackageName) == PackageManager.SIGNATURE_MATCH
					&& hasMarketLink(context, proPackageName)) {
				Util.log(null, Log.INFO, "Pro version installed");
				return "";
			}

			// Get license file name
			String folder = Environment.getExternalStorageDirectory().getAbsolutePath();
			String fileName = folder + File.separator + "XPrivacy_license.txt";
			File licenseFile = new File(fileName);
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
					Util.log(null, Log.ERROR, "Invalid license file");
					return null;
				}

				// Verify license
				boolean licensed = verifyData(bEmail, bSignature, getPublicKey(context));
				if (licensed)
					Util.log(null, Log.INFO, "Licensed to " + name + " (" + email + ")");
				else
					Util.log(null, Log.ERROR, "Invalid license for " + name + " (" + email + ")");

				// Return result
				return (licensed ? name : null);
			} else
				Util.log(null, Log.INFO, "No license folder=" + Environment.getExternalStorageDirectory());
		} catch (Throwable ex) {
			Util.log(null, Log.ERROR, "Processing license");
			Util.bug(null, ex);
		}
		return null;
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
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import android.annotation.SuppressLint;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.os.Build;
import android.provider.MediaStore;
import android.telephony.TelephonyManager;
import android.util.Log;

public class XRestriction {

	// This should correspond with restrict_<name> in strings.xml
	public static final String cAccounts = "accounts";
	public static final String cBoot = "boot";
	public static final String cBrowser = "browser";
	public static final String cCalendar = "calendar";
	public static final String cCalling = "calling";
	public static final String cContacts = "contacts";
	public static final String cIdentification = "identification";
	public static final String cInternet = "internet";
	public static final String cLocation = "location";
	public static final String cMedia = "media";
	public static final String cMessages = "messages";
	public static final String cNetwork = "network";
	public static final String cPhone = "phone";
	public static final String cStorage = "storage";
	public static final String cShell = "shell";
	public static final String cSystem = "system";
	public static final String cView = "view";

	private static final String cRestrictionNames[] = new String[] { cAccounts, cBoot, cBrowser, cCalendar, cCalling,
			cContacts, cIdentification, cInternet, cLocation, cMedia, cMessages, cNetwork, cPhone, cStorage, cShell,
			cSystem, cView };

	public final static int cXposedMinVersion = 34;
	public final static int cUidAndroid = 1000;

	public final static String cSettingExpert = "Expert";
	public final static String cSettingLatitude = "Latitude";
	public final static String cSettingLongitude = "Longitude";

	private final static int cCacheTimeoutMs = 15 * 1000;
	private static Map<String, List<String>> mPermissions = new LinkedHashMap<String, List<String>>();
	private static Map<String, List<String>> mMethods = new LinkedHashMap<String, List<String>>();
	private static Map<String, CacheEntry> mRestrictionCache = new HashMap<String, CacheEntry>();

	static {
		// This is a workaround, the idea was to use registerMethod
		// Static data is not shared across VM's
		// If you know a better solution, please let me know

		// Restrictions
		for (String restrictionName : cRestrictionNames) {
			mPermissions.put(restrictionName, new ArrayList<String>());
			mMethods.put(restrictionName, new ArrayList<String>());
		}

		// Permissions
		mPermissions.get(cAccounts).add("GET_ACCOUNTS");
		mPermissions.get(cAccounts).add("USE_CREDENTIALS");
		mPermissions.get(cAccounts).add("MANAGE_ACCOUNTS");
		mPermissions.get(cBoot).add("RECEIVE_BOOT_COMPLETED");
		mPermissions.get(cBrowser).add("READ_HISTORY_BOOKMARKS");
		mPermissions.get(cBrowser).add("GLOBAL_SEARCH");
		mPermissions.get(cCalendar).add("READ_CALENDAR");
		mPermissions.get(cCalling).add("SEND_SMS");
		mPermissions.get(cCalling).add("CALL_PHONE");
		mPermissions.get(cContacts).add("READ_CONTACTS");
		mPermissions.get(cInternet).add("INTERNET");
		mPermissions.get(cLocation).add("ACCESS_COARSE_LOCATION");
		mPermissions.get(cLocation).add("ACCESS_FINE_LOCATION");
		mPermissions.get(cLocation).add("ACCESS_COARSE_UPDATES");
		mPermissions.get(cLocation).add("CONTROL_LOCATION_UPDATES");
		mPermissions.get(cMedia).add("CAMERA");
		mPermissions.get(cMedia).add("RECORD_AUDIO");
		mPermissions.get(cMedia).add("RECORD_VIDEO");
		mPermissions.get(cMessages).add("READ_WRITE_ALL_VOICEMAIL");
		mPermissions.get(cMessages).add("READ_SMS");
		mPermissions.get(cMessages).add("RECEIVE_SMS");
		mPermissions.get(cNetwork).add("ACCESS_NETWORK_STATE");
		mPermissions.get(cNetwork).add("ACCESS_WIFI_STATE");
		mPermissions.get(cNetwork).add("BLUETOOTH");
		mPermissions.get(cPhone).add("READ_PHONE_STATE");
		mPermissions.get(cPhone).add("PROCESS_OUTGOING_CALLS");
		mPermissions.get(cPhone).add("READ_CALL_LOG");
		mPermissions.get(cPhone).add("WRITE_APN_SETTINGS");
		mPermissions.get(cStorage).add("READ_EXTERNAL_STORAGE");
		mPermissions.get(cStorage).add("WRITE_EXTERNAL_STORAGE");

		// Methods

		// Account manager
		String[] accs = new String[] { "getAccounts", "getAccountsByType", "getAccountsByTypeAndFeatures",
				"hasFeatures", "addOnAccountsUpdatedListener", "getAuthToken", "getAuthTokenByFeatures",
				"blockingGetAuthToken" };
		for (String acc : accs)
			mMethods.get(cAccounts).add(acc);

		// Application package manager
		String[] ams = new String[] { "getInstalledApplications", "getInstalledPackages", "getInstalledThemePackages",
				"getPreferredPackages" };
		for (String am : ams)
			mMethods.get(cSystem).add(am);

		// Audio record
		mMethods.get(XRestriction.cMedia).add("startRecording");

		// Bluetooth adapter
		mMethods.get(XRestriction.cNetwork).add("getAddress");
		mMethods.get(XRestriction.cNetwork).add("getBondedDevices");

		// Camera
		String[] cams = new String[] { "setPreviewCallback", "setPreviewCallbackWithBuffer",
				"setOneShotPreviewCallback", "takePicture" };
		for (String cam : cams)
			mMethods.get(cMedia).add(cam);

		// Location manager
		String[] locs = new String[] { "addNmeaListener", "addProximityAlert", "getLastKnownLocation", "removeUpdates",
				"requestLocationUpdates", "requestSingleUpdate" };
		for (String loc : locs)
			mMethods.get(cLocation).add(loc);

		// Media recorder
		mMethods.get(cMedia).add("setOutputFile");

		// Network interface
		String[] nets = new String[] { "getHardwareAddress", "getInetAddresses", "getInterfaceAddresses" };
		for (String net : nets)
			mMethods.get(cNetwork).add(net);

		// Package manager service
		mMethods.get(cInternet).add("getPackageGids");
		mMethods.get(cStorage).add("getPackageGids");

		// Runtime
		mMethods.get(cShell).add("sh");
		mMethods.get(cShell).add("su");
		mMethods.get(cShell).add("exec");
		mMethods.get(cShell).add("load");
		mMethods.get(cShell).add("loadLibrary");

		// Settings secure
		mMethods.get(cIdentification).add("getString");

		// SMS manager
		mMethods.get(cMessages).add("getAllMessagesFromIcc");

		String[] smses = new String[] { "sendDataMessage", "sendMultipartTextMessage", "sendTextMessage" };
		for (String sms : smses)
			mMethods.get(cCalling).add(sms);

		// System properties
		String[] props = new String[] { "ro.gsm.imei", "net.hostname", "ro.serialno", "ro.boot.serialno",
				"ro.boot.wifimacaddr", "ro.boot.btmacaddr" };
		for (String prop : props)
			mMethods.get(cIdentification).add(prop);

		// Telephony
		String[] tlocs = new String[] { "disableLocationUpdates", "enableLocationUpdates", "getAllCellInfo",
				"getCellLocation", "getNeighboringCellInfo" };
		for (String tloc : tlocs)
			mMethods.get(cLocation).add(tloc);

		String[] phones = new String[] { "getDeviceId", "getIsimDomain", "getIsimImpi", "getIsimImpu",
				"getLine1AlphaTag", "getLine1Number", "getMsisdn", "getNetworkCountryIso", "getNetworkOperator",
				"getNetworkOperatorName", "getSimCountryIso", "getSimOperator", "getSimOperatorName",
				"getSimSerialNumber", "getSubscriberId", "getVoiceMailAlphaTag", "getVoiceMailNumber", "listen" };
		for (String phone : phones)
			mMethods.get(cPhone).add(phone);

		// Wi-Fi manager
		String[] wifis = new String[] { "getConfiguredNetworks", "getConnectionInfo", "getDhcpInfo", "getScanResults" };
		for (String wifi : wifis)
			mMethods.get(cNetwork).add(wifi);

		// Intent receive: boot
		mMethods.get(cBoot).add(Intent.ACTION_BOOT_COMPLETED);

		// Intent receive: calling
		mMethods.get(cPhone).add(Intent.ACTION_NEW_OUTGOING_CALL);
		mMethods.get(cPhone).add(TelephonyManager.ACTION_PHONE_STATE_CHANGED);

		// Intent send: browser
		mMethods.get(cView).add(Intent.ACTION_VIEW);

		// Intent send: call
		mMethods.get(cCalling).add(Intent.ACTION_CALL);

		// Intent send: media
		mMethods.get(cMedia).add(MediaStore.ACTION_IMAGE_CAPTURE);
		if (Build.VERSION.SDK_INT >= 17)
			mMethods.get(cMedia).add("android.media.action.IMAGE_CAPTURE_SECURE");
		mMethods.get(cMedia).add(MediaStore.ACTION_VIDEO_CAPTURE);

		// Content providers
		mMethods.get(cBrowser).add("BrowserProvider");
		mMethods.get(cBrowser).add("BrowserProvider2");
		mMethods.get(cCalendar).add("CalendarProvider2");
		mMethods.get(cContacts).add("ContactsProvider2");
		mMethods.get(cPhone).add("CallLogProvider");
		mMethods.get(cMessages).add("VoicemailContentProvider");
		mMethods.get(cMessages).add("SmsProvider");
		mMethods.get(cMessages).add("MmsProvider");
		mMethods.get(cMessages).add("MmsSmsProvider");
		mMethods.get(cPhone).add("TelephonyProvider");
	}

	public static void registerMethod(String methodName, String restrictionName, String[] permissions) {
		if (restrictionName != null && !mPermissions.containsKey(restrictionName))
			XUtil.log(null, Log.WARN, "Missing restriction " + restrictionName);

		for (String permission : permissions)
			if (!mPermissions.get(restrictionName).contains(permission))
				XUtil.log(null, Log.WARN, "Missing permission " + permission);

		if (!mMethods.containsKey(restrictionName) || !mMethods.get(restrictionName).contains(methodName))
			XUtil.log(null, Log.WARN, "Missing method " + methodName);
	}

	public static List<String> getRestrictions(Context context) {
		List<String> listRestriction = new ArrayList<String>(Arrays.asList(cRestrictionNames));
		if (!Boolean.parseBoolean(XRestriction.getSetting(null, context, XRestriction.cSettingExpert,
				Boolean.FALSE.toString())))
			listRestriction.remove(cBoot);
		return listRestriction;
	}

	public static List<String> getMethodNames(Context context, String restrictionName) {
		return mMethods.get(restrictionName);
	}

	public static List<String> getPermissions(String restrictionName) {
		return mPermissions.get(restrictionName);
	}

	public static List<String> getMethods(String restrictionName) {
		return mMethods.get(restrictionName);
	}

	public static boolean hasInternet(Context context, String packageName) {
		// TODO: check if internet permission restricted
		PackageManager pm = context.getPackageManager();
		return (pm.checkPermission("android.permission.INTERNET", packageName) == PackageManager.PERMISSION_GRANTED);
	}

	@SuppressLint("DefaultLocale")
	public static boolean hasPermission(Context context, String packageName, String restrictionName) {
		List<String> listPermission = mPermissions.get(restrictionName);
		if (listPermission == null || listPermission.size() == 0)
			return true;

		try {
			PackageManager pm = context.getPackageManager();
			PackageInfo pInfo = pm.getPackageInfo(packageName, PackageManager.GET_PERMISSIONS);
			if (pInfo != null && pInfo.requestedPermissions != null)
				for (String rPermission : pInfo.requestedPermissions)
					for (String permission : listPermission)
						if (rPermission.toLowerCase().contains(permission.toLowerCase()))
							return true;
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
			return false;
		}
		return false;
	}

	public static boolean isUsed(Context context, int uid, String restrictionName, String methodName) {
		long lastUsage = 0;
		ContentResolver cr = context.getContentResolver();
		Cursor cursor = cr.query(XPrivacyProvider.URI_USAGE, null, restrictionName,
				new String[] { Integer.toString(uid), methodName }, null);
		if (cursor.moveToNext())
			lastUsage = cursor.getLong(cursor.getColumnIndex(XPrivacyProvider.COL_USED));
		cursor.close();
		boolean used = (lastUsage != 0);
		logRestriction(null, context, uid, "used", restrictionName, methodName, used, false);
		return used;
	}

	@SuppressLint("DefaultLocale")
	public static boolean getRestricted(XHook hook, Context context, int uid, String restrictionName,
			String methodName, boolean usage, boolean useCache) {
		try {
			// Check uid
			if (uid <= 0) {
				XUtil.log(hook, Log.WARN, "uid <= 0");
				XUtil.logStack(hook);
				return false;
			}

			// Check restriction
			if (restrictionName == null || restrictionName.equals("")) {
				XUtil.log(hook, Log.WARN, "restriction empty");
				XUtil.logStack(hook);
				return false;
			}

			// Check cache
			String keyCache = String.format("%d.%s.%s", uid, restrictionName, methodName);
			if (useCache)
				synchronized (mRestrictionCache) {
					if (mRestrictionCache.containsKey(keyCache)) {
						CacheEntry entry = mRestrictionCache.get(keyCache);
						if (entry.isExpired())
							mRestrictionCache.remove(keyCache);
						else {
							logRestriction(hook, context, uid, "get", restrictionName, methodName,
									entry.isRestricted(), true);
							return entry.isRestricted();
						}
					}
				}

			// Check if restricted
			boolean fallback = true;
			boolean restricted = false;
			if (context != null && uid != XRestriction.cUidAndroid) {
				// Get content resolver
				ContentResolver contentResolver = context.getContentResolver();
				if (contentResolver == null) {
					XUtil.log(hook, Log.WARN, "contentResolver is null");
					XUtil.logStack(hook);
				} else {
					// Query restriction
					Cursor cursor = contentResolver.query(XPrivacyProvider.URI_RESTRICTION, null, restrictionName,
							new String[] { Integer.toString(uid), Boolean.toString(usage), methodName }, null);
					if (cursor == null) {
						// Can happen if memory low
						XUtil.log(hook, Log.WARN, "cursor is null");
						XUtil.logStack(null);
					} else {
						// Get restriction
						if (cursor.moveToNext()) {
							restricted = Boolean.parseBoolean(cursor.getString(cursor
									.getColumnIndex(XPrivacyProvider.COL_RESTRICTED)));
							fallback = false;
						} else {
							XUtil.log(hook, Log.WARN, "cursor is empty");
							XUtil.logStack(null);
						}
						cursor.close();
					}
				}
			}

			// Use fallback
			if (fallback)
				restricted = XPrivacyProvider.getRestrictedFallback(hook, uid, restrictionName, methodName);
			else {
				// Add to cache
				synchronized (mRestrictionCache) {
					if (mRestrictionCache.containsKey(keyCache))
						mRestrictionCache.remove(keyCache);
					mRestrictionCache.put(keyCache, new CacheEntry(restricted));
				}
			}

			// Result
			logRestriction(hook, context, uid, "get", restrictionName, methodName, restricted, false);
			return restricted;
		} catch (Throwable ex) {
			XUtil.bug(hook, ex);
			return false;
		}
	}

	public static void setRestricted(XHook hook, Context context, int uid, String restrictionName, String methodName,
			boolean restricted) {
		// Check context
		if (context == null) {
			XUtil.log(hook, Log.WARN, "context is null");
			return;
		}

		// Check uid
		if (uid == 0) {
			XUtil.log(hook, Log.WARN, "uid=0");
			return;
		}

		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			XUtil.log(hook, Log.WARN, "contentResolver is null");
			return;
		}

		// Set restrictions
		ContentValues values = new ContentValues();
		values.put(XPrivacyProvider.COL_UID, uid);
		values.put(XPrivacyProvider.COL_METHOD, methodName);
		values.put(XPrivacyProvider.COL_RESTRICTED, Boolean.toString(restricted));
		contentResolver.update(XPrivacyProvider.URI_RESTRICTION, values, restrictionName, null);

		// Result
		logRestriction(hook, context, uid, "set", restrictionName, methodName, restricted, false);
	}

	public static String getSetting(XHook hook, Context context, String settingName, String defaultValue) {
		// TODO: settings caching
		String value = defaultValue;
		if (context == null) {
			XUtil.log(hook, Log.WARN, "context is null");
			XUtil.logStack(hook);
			// TODO: fallback
		} else
			try {
				ContentResolver cr = context.getContentResolver();
				Cursor cursor = cr.query(XPrivacyProvider.URI_SETTING, null, settingName, null, null);
				if (cursor.moveToNext())
					value = cursor.getString(cursor.getColumnIndex(XPrivacyProvider.COL_VALUE));
				else {
					XUtil.log(hook, Log.WARN, "cursor is empty setting=" + settingName);
					XUtil.logStack(hook);
				}
				cursor.close();
			} catch (Throwable ex) {
				XUtil.log(hook, Log.ERROR, "get setting=" + settingName);
				XUtil.bug(hook, ex);
			}
		XUtil.log(hook, Log.INFO, String.format("get %s=%s", settingName, value));
		return value;
	}

	public static void setSetting(XHook hook, Context context, String settingName, String value) {
		ContentResolver contentResolver = context.getContentResolver();
		ContentValues values = new ContentValues();
		values.put(XPrivacyProvider.COL_VALUE, value);
		contentResolver.update(XPrivacyProvider.URI_SETTING, values, settingName, null);
		XUtil.log(hook, Log.INFO, String.format("set %s=%s", settingName, value));
	}

	public static void deleteUsageData(Context context, int uid) {
		for (String restrictionName : XRestriction.getRestrictions(context))
			context.getContentResolver().delete(XPrivacyProvider.URI_USAGE, restrictionName,
					new String[] { Integer.toString(uid) });
	}

	public static String getLocalizedName(Context context, String restrictionName) {
		String packageName = XRestriction.class.getPackage().getName();
		int stringId = context.getResources().getIdentifier("restrict_" + restrictionName, "string", packageName);
		return (stringId == 0 ? null : context.getString(stringId));
	}

	public static String getDefacedString() {
		return "DEFACE";
	}

	public static long getDefacedHex() {
		return 0xDEFACEL;
	}

	public static String getDefacedMac() {
		return "de:fa:ce:de:fa:ce";
	}

	public static byte[] getDefacedBytes() {
		return new byte[] { (byte) 0xDE, (byte) 0xFA, (byte) 0xCE };
	}

	public static int getDefacedIPInt() {
		return 127 + (0 << 8) + (0 << 16) + (1 << 24);
	}

	public static byte[] getDefacedIPBytes() {
		return new byte[] { 10, 1, 1, 1 };
	}

	// Helper methods

	private static void logRestriction(XHook hook, Context context, int uid, String prefix, String restrictionName,
			String methodName, boolean restricted, boolean cached) {
		XUtil.log(hook, Log.INFO, String.format("%s %s/%s %s=%b%s", prefix, getPackageName(context, uid), methodName,
				restrictionName, restricted, (cached ? " *" : (context == null ? " #" : ""))));
	}

	private static String getPackageName(Context context, int uid) {
		if (context != null) {
			String[] packages = context.getPackageManager().getPackagesForUid(uid);
			if (packages != null && packages.length == 1)
				return packages[0];
		}
		return Integer.toString(uid);
	}

	// Helper classes

	private static class CacheEntry {
		public long mTimestamp;
		public boolean mRestricted;

		public CacheEntry(boolean restricted) {
			mTimestamp = new Date().getTime();
			mRestricted = restricted;
		}

		public boolean isExpired() {
			return (mTimestamp + cCacheTimeoutMs < new Date().getTime());
		}

		public boolean isRestricted() {
			return mRestricted;
		}
	}
}

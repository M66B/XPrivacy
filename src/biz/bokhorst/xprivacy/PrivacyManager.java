package biz.bokhorst.xprivacy;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Field;
import java.net.Inet4Address;
import java.net.InetAddress;
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
import android.location.Location;
import android.nfc.NfcAdapter;
import android.os.Build;
import android.provider.MediaStore;
import android.telephony.TelephonyManager;
import android.util.Log;

public class PrivacyManager {

	// This should correspond with restrict_<name> in strings.xml
	public static final String cAccounts = "accounts";
	public static final String cBrowser = "browser";
	public static final String cCalendar = "calendar";
	public static final String cCalling = "calling";
	public static final String cContacts = "contacts";
	public static final String cDictionary = "dictionary";
	public static final String cEMail = "email";
	public static final String cIdentification = "identification";
	public static final String cInternet = "internet";
	public static final String cLocation = "location";
	public static final String cMedia = "media";
	public static final String cMessages = "messages";
	public static final String cNetwork = "network";
	public static final String cNfc = "nfc";
	public static final String cPhone = "phone";
	public static final String cStorage = "storage";
	public static final String cShell = "shell";
	public static final String cSystem = "system";
	public static final String cView = "view";

	private static final String cRestrictionNames[] = new String[] { cAccounts, cBrowser, cCalendar, cCalling,
			cContacts, cDictionary, cEMail, cIdentification, cInternet, cLocation, cMedia, cMessages, cNetwork, cNfc,
			cPhone, cShell, cStorage, cSystem, cView };

	public final static int cXposedMinVersion = 34;
	public final static int cUidAndroid = 1000;

	public final static String cSettingLatitude = "Latitude";
	public final static String cSettingLongitude = "Longitude";
	public final static String cSettingMac = "Mac";
	public final static String cSettingImei = "IMEI";
	public final static String cSettingPhone = "Phone";
	public final static String cSettingId = "ID";
	public final static String cSettingGsfId = "GSF_ID";
	public final static String cSettingMcc = "MCC";
	public final static String cSettingMnc = "MNC";
	public final static String cSettingCountry = "Country";
	public final static String cSettingIccId = "ICC_ID";
	public final static String cSettingSubscriber = "Subscriber";
	public final static String cSettingFPermission = "FPermission";
	public final static String cSettingExpert = "Expert";
	public final static String cSettingTheme = "Theme";
	public final static String cSettingSalt = "Salt";

	private final static String cDeface = "DEFACE";

	private final static int cCacheTimeoutMs = 15 * 1000;

	private static Map<String, List<String>> mPermissions = new LinkedHashMap<String, List<String>>();
	private static Map<String, List<String>> mMethods = new LinkedHashMap<String, List<String>>();
	private static Map<String, CRestriction> mRestrictionCache = new HashMap<String, CRestriction>();
	private static Map<String, CSetting> mSettingsCache = new HashMap<String, CSetting>();
	private static Map<UsageData, UsageData> mUsageQueue = new LinkedHashMap<UsageData, UsageData>();

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
		mPermissions.get(cBrowser).add("READ_HISTORY_BOOKMARKS");
		mPermissions.get(cBrowser).add("GLOBAL_SEARCH");
		mPermissions.get(cCalendar).add("READ_CALENDAR");
		mPermissions.get(cCalling).add("SEND_SMS");
		mPermissions.get(cCalling).add("CALL_PHONE");
		mPermissions.get(cContacts).add("READ_CONTACTS");
		mPermissions.get(cDictionary).add("READ_USER_DICTIONARY");
		mPermissions.get(cEMail).add("ACCESS_PROVIDER");
		mPermissions.get(cIdentification).add("READ_GSERVICES");
		mPermissions.get(cIdentification).add("");
		mPermissions.get(cInternet).add("INTERNET");
		mPermissions.get(cLocation).add("ACCESS_COARSE_LOCATION");
		mPermissions.get(cLocation).add("ACCESS_FINE_LOCATION");
		mPermissions.get(cLocation).add("ACCESS_COARSE_UPDATES");
		mPermissions.get(cLocation).add("CONTROL_LOCATION_UPDATES");
		mPermissions.get(cLocation).add("ACCESS_WIFI_STATE");
		mPermissions.get(cMedia).add("CAMERA");
		mPermissions.get(cMedia).add("RECORD_AUDIO");
		mPermissions.get(cMedia).add("RECORD_VIDEO");
		mPermissions.get(cMessages).add("READ_WRITE_ALL_VOICEMAIL");
		mPermissions.get(cMessages).add("READ_SMS");
		mPermissions.get(cMessages).add("RECEIVE_SMS");
		mPermissions.get(cNetwork).add("ACCESS_NETWORK_STATE");
		mPermissions.get(cNetwork).add("ACCESS_WIFI_STATE");
		mPermissions.get(cNetwork).add("BLUETOOTH");
		mPermissions.get(cNfc).add("NFC");
		mPermissions.get(cPhone).add("READ_PHONE_STATE");
		mPermissions.get(cPhone).add("PROCESS_OUTGOING_CALLS");
		mPermissions.get(cPhone).add("READ_CALL_LOG");
		mPermissions.get(cPhone).add("WRITE_APN_SETTINGS");
		mPermissions.get(cShell).add("");
		mPermissions.get(cStorage).add("READ_EXTERNAL_STORAGE");
		mPermissions.get(cStorage).add("WRITE_EXTERNAL_STORAGE");
		mPermissions.get(cStorage).add("WRITE_MEDIA_STORAGE");
		mPermissions.get(cSystem).add("GET_TASKS");
		mPermissions.get(cSystem).add("");
		mPermissions.get(cView).add("");

		// Methods

		// Account manager
		String[] accs = new String[] { "addOnAccountsUpdatedListener", "blockingGetAuthToken", "getAccounts",
				"getAccountsByType", "getAccountsByTypeAndFeatures", "getAuthToken", "getAuthTokenByFeatures",
				"hasFeatures", "removeOnAccountsUpdatedListener" };
		for (String acc : accs)
			mMethods.get(cAccounts).add(acc);

		// Activity manager
		String[] acts = new String[] { "getRecentTasks", "getRunningAppProcesses", "getRunningServices",
				"getRunningTasks" };
		for (String act : acts)
			mMethods.get(cSystem).add(act);

		// App widget manager
		mMethods.get(cSystem).add("getInstalledProviders");

		// Application package manager
		String[] ams = new String[] { "getInstalledApplications", "getInstalledPackages", "getPreferredPackages",
				"queryBroadcastReceivers", "queryContentProviders", "queryIntentActivities",
				"queryIntentActivityOptions", "queryIntentServices" };
		for (String am : ams)
			mMethods.get(cSystem).add(am);

		// Audio record
		mMethods.get(PrivacyManager.cMedia).add("startRecording");

		// Bluetooth adapter
		mMethods.get(PrivacyManager.cNetwork).add("getAddress");
		mMethods.get(PrivacyManager.cNetwork).add("getBondedDevices");

		// Camera
		String[] cams = new String[] { "setPreviewCallback", "setPreviewCallbackWithBuffer",
				"setOneShotPreviewCallback", "takePicture" };
		for (String cam : cams)
			mMethods.get(cMedia).add(cam);

		// Environment
		mMethods.get(PrivacyManager.cStorage).add("getExternalStorageState");

		// Identification
		mMethods.get(PrivacyManager.cIdentification).add("SERIAL");

		// Location manager
		String[] locs = new String[] { "addNmeaListener", "addProximityAlert", "getLastKnownLocation", "removeUpdates",
				"requestLocationUpdates", "requestSingleUpdate", "sendExtraCommand" };
		for (String loc : locs)
			mMethods.get(cLocation).add(loc);

		// Media recorder
		mMethods.get(cMedia).add("setOutputFile");

		// Network info
		String[] ninfos = new String[] { "getDetailedState", "getState", "isConnected", "isConnectedOrConnecting" };
		for (String ninfo : ninfos)
			mMethods.get(cInternet).add(ninfo);

		// Network interface
		String[] nets = new String[] { "getHardwareAddress", "getInetAddresses", "getInterfaceAddresses" };
		for (String net : nets)
			mMethods.get(cNetwork).add(net);

		// Package manager service
		mMethods.get(cInternet).add("inet");
		mMethods.get(cStorage).add("media");
		mMethods.get(cStorage).add("sdcard");

		// Runtime
		mMethods.get(cShell).add("sh");
		mMethods.get(cShell).add("su");
		mMethods.get(cShell).add("exec");
		mMethods.get(cShell).add("load");
		mMethods.get(cShell).add("loadLibrary");
		mMethods.get(cShell).add("start");

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
		mMethods.get(cLocation).add("getScanResults");
		mMethods.get(cInternet).add("getConnectionInfo");

		// Intent receive: calling
		mMethods.get(cPhone).add(Intent.ACTION_NEW_OUTGOING_CALL);
		mMethods.get(cPhone).add(TelephonyManager.ACTION_PHONE_STATE_CHANGED);

		// Intent receive: NFC
		mMethods.get(cNfc).add(NfcAdapter.ACTION_NDEF_DISCOVERED);
		mMethods.get(cNfc).add(NfcAdapter.ACTION_TAG_DISCOVERED);
		mMethods.get(cNfc).add(NfcAdapter.ACTION_TECH_DISCOVERED);

		// Intent send: browser
		mMethods.get(cView).add(Intent.ACTION_VIEW);

		// Intent send: call
		mMethods.get(cCalling).add(Intent.ACTION_CALL);

		// Intent send: media
		mMethods.get(cMedia).add(MediaStore.ACTION_IMAGE_CAPTURE);
		if (Build.VERSION.SDK_INT >= 17)
			mMethods.get(cMedia).add("android.media.action.IMAGE_CAPTURE_SECURE");
		mMethods.get(cMedia).add(MediaStore.ACTION_VIDEO_CAPTURE);

		// Applications provider
		mMethods.get(cSystem).add("ApplicationsProvider");

		// Browser provider
		mMethods.get(cBrowser).add("BrowserProvider");
		mMethods.get(cBrowser).add("BrowserProvider2");

		// Calendar provider
		mMethods.get(cCalendar).add("CalendarProvider2");

		// Contacts provider
		mMethods.get(cContacts).add("ContactsProvider2");
		mMethods.get(cPhone).add("CallLogProvider");
		mMethods.get(cMessages).add("VoicemailContentProvider");

		// E-mail provider
		mMethods.get(cEMail).add("EMailProvider");

		// Google services provider
		mMethods.get(cIdentification).add("GservicesProvider");

		// Telephony providers
		mMethods.get(cMessages).add("SmsProvider");
		mMethods.get(cMessages).add("MmsProvider");
		mMethods.get(cMessages).add("MmsSmsProvider");
		mMethods.get(cPhone).add("TelephonyProvider");

		// User dictionary
		mMethods.get(cDictionary).add("UserDictionary");
	}

	// Data

	public static void registerMethod(String methodName, String restrictionName, String[] permissions) {
		if (restrictionName != null && !mPermissions.containsKey(restrictionName))
			Util.log(null, Log.WARN, "Missing restriction " + restrictionName);

		if (permissions != null) {
			if (permissions.length == 0)
				if (!mPermissions.get(restrictionName).contains(""))
					Util.log(null, Log.WARN, "Missing no permission restriction=" + restrictionName);

			for (String permission : permissions)
				if (!mPermissions.get(restrictionName).contains(permission))
					Util.log(null, Log.WARN, "Missing permission " + permission);
		}

		if (!mMethods.containsKey(restrictionName) || !mMethods.get(restrictionName).contains(methodName))
			Util.log(null, Log.WARN, "Missing method " + methodName);
	}

	public static List<String> getRestrictions() {
		return new ArrayList<String>(Arrays.asList(cRestrictionNames));
	}

	public static List<String> getMethodNames(String restrictionName) {
		return mMethods.get(restrictionName);
	}

	public static List<String> getPermissions(String restrictionName) {
		return mPermissions.get(restrictionName);
	}

	public static List<String> getMethods(String restrictionName) {
		return mMethods.get(restrictionName);
	}

	public static String getLocalizedName(Context context, String restrictionName) {
		String packageName = PrivacyManager.class.getPackage().getName();
		int stringId = context.getResources().getIdentifier("restrict_" + restrictionName, "string", packageName);
		return (stringId == 0 ? null : context.getString(stringId));
	}

	// Restrictions

	@SuppressLint("DefaultLocale")
	public static boolean getRestricted(XHook hook, Context context, int uid, String restrictionName,
			String methodName, boolean usage, boolean useCache) {
		try {
			long start = System.currentTimeMillis();

			// Check uid
			if (uid <= 0) {
				Util.log(hook, Log.WARN, "uid <= 0");
				Util.logStack(hook);
				return false;
			}

			// Check restriction
			if (restrictionName == null || restrictionName.equals("")) {
				Util.log(hook, Log.WARN, "restriction empty");
				Util.logStack(hook);
				return false;
			}

			if (usage)
				if (methodName == null || methodName.equals("")) {
					Util.log(hook, Log.WARN, "method empty");
					Util.logStack(hook);
				}

			// Check cache
			String keyCache = String.format("%d.%s.%s", uid, restrictionName, methodName);
			if (useCache)
				synchronized (mRestrictionCache) {
					if (mRestrictionCache.containsKey(keyCache)) {
						CRestriction entry = mRestrictionCache.get(keyCache);
						if (entry.isExpired())
							mRestrictionCache.remove(keyCache);
						else {
							long ms = System.currentTimeMillis() - start;
							logRestriction(hook, context, uid, "get", restrictionName, methodName,
									entry.isRestricted(), true, ms);
							return entry.isRestricted();
						}
					}
				}

			// Check if restricted
			boolean fallback = true;
			boolean restricted = false;
			if (context != null && uid != PrivacyManager.cUidAndroid)
				try {
					// Get content resolver
					ContentResolver contentResolver = context.getContentResolver();
					if (contentResolver == null) {
						Util.log(hook, Log.WARN, "contentResolver is null");
						Util.logStack(hook);
					} else {
						// Query restriction
						Cursor cursor = contentResolver.query(PrivacyProvider.URI_RESTRICTION, null, restrictionName,
								new String[] { Integer.toString(uid), Boolean.toString(usage), methodName }, null);
						if (cursor == null) {
							// Can happen if memory low
							Util.log(hook, Log.WARN, "cursor is null");
							Util.logStack(null);
						} else {
							// Get restriction
							if (cursor.moveToNext()) {
								restricted = Boolean.parseBoolean(cursor.getString(cursor
										.getColumnIndex(PrivacyProvider.COL_RESTRICTED)));
								fallback = false;
							} else {
								Util.log(hook, Log.WARN, "cursor is empty");
								Util.logStack(null);
							}
							cursor.close();
						}

						// Send usage data
						UsageData data = null;
						do {
							int size = 0;
							synchronized (mUsageQueue) {
								if (mUsageQueue.size() > 0) {
									data = mUsageQueue.keySet().iterator().next();
									mUsageQueue.remove(data);
									size = mUsageQueue.size();
								} else
									data = null;
							}
							if (data != null) {
								try {
									Util.log(hook, Log.INFO, "Sending usage data=" + data + " size=" + size);
									ContentValues values = new ContentValues();
									values.put(PrivacyProvider.COL_UID, data.getUid());
									values.put(PrivacyProvider.COL_RESTRICTION, data.getRestrictionName());
									values.put(PrivacyProvider.COL_METHOD, data.getMethodName());
									values.put(PrivacyProvider.COL_USED, data.getTimeStamp());
									if (contentResolver.update(PrivacyProvider.URI_USAGE, values, null, null) <= 0)
										Util.log(hook, Log.INFO, "Error updating usage data=" + data);
								} catch (Throwable ex) {
									Util.bug(hook, ex);
								}
							}
						} while (data != null);
					}
				} catch (Throwable ex) {
					Util.bug(hook, ex);
				}

			// Use fallback
			if (fallback) {
				// Queue usage data
				if (usage && uid != PrivacyManager.cUidAndroid) {
					UsageData usageData = new UsageData(uid, restrictionName, methodName);
					synchronized (mUsageQueue) {
						if (mUsageQueue.containsKey(usageData))
							mUsageQueue.remove(usageData);
						mUsageQueue.put(usageData, usageData);
						Util.log(hook, Log.INFO, "Queue usage data=" + usageData + " size=" + mUsageQueue.size());
					}
				}

				// Fallback
				restricted = PrivacyProvider.getRestrictedFallback(hook, uid, restrictionName, methodName);
			}

			// Add to cache
			synchronized (mRestrictionCache) {
				if (mRestrictionCache.containsKey(keyCache))
					mRestrictionCache.remove(keyCache);
				mRestrictionCache.put(keyCache, new CRestriction(restricted));
			}

			// Result
			long ms = System.currentTimeMillis() - start;
			logRestriction(hook, context, uid, "get", restrictionName, methodName, restricted, false, ms);
			return restricted;
		} catch (Throwable ex) {
			// Failsafe
			Util.bug(hook, ex);
			return false;
		}
	}

	public static void setRestricted(XHook hook, Context context, int uid, String restrictionName, String methodName,
			boolean restricted) {
		// Check context
		if (context == null) {
			Util.log(hook, Log.WARN, "context is null");
			return;
		}

		// Check uid
		if (uid == 0) {
			Util.log(hook, Log.WARN, "uid=0");
			return;
		}

		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			Util.log(hook, Log.WARN, "contentResolver is null");
			return;
		}

		// Set restrictions
		ContentValues values = new ContentValues();
		values.put(PrivacyProvider.COL_UID, uid);
		values.put(PrivacyProvider.COL_METHOD, methodName);
		values.put(PrivacyProvider.COL_RESTRICTED, Boolean.toString(restricted));
		if (contentResolver.update(PrivacyProvider.URI_RESTRICTION, values, restrictionName, null) <= 0)
			Util.log(hook, Log.INFO, "Error updating restriction=" + restrictionName);

		// Result
		logRestriction(hook, context, uid, "set", restrictionName, methodName, restricted, false, 0);

		// Identification: do not restrict Google services provider by default
		if (restricted && restrictionName.equals(cIdentification) && methodName == null)
			setRestricted(hook, context, uid, restrictionName, "GservicesProvider", false);

		// Shell: do not restrict load/loadLibrary by default
		if (restricted && restrictionName.equals(cShell) && methodName == null) {
			setRestricted(hook, context, uid, restrictionName, "load", false);
			setRestricted(hook, context, uid, restrictionName, "loadLibrary", false);
		}
	}

	public static List<Boolean> getRestricted(Context context, int uid) {
		List<Boolean> listRestricted = new ArrayList<Boolean>();
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver != null) {
			Cursor cursor = contentResolver.query(PrivacyProvider.URI_RESTRICTION, null, null,
					new String[] { Integer.toString(uid), Boolean.toString(false), null }, null);
			if (cursor != null) {
				while (cursor.moveToNext())
					listRestricted.add(Boolean.parseBoolean(cursor.getString(cursor
							.getColumnIndex(PrivacyProvider.COL_RESTRICTED))));
				cursor.close();
			}
		}
		return listRestricted;
	}

	public static class RestrictionDesc {
		public int uid;
		public boolean restricted;
		public String restrictionName;
		public String methodName;
	}

	public static List<RestrictionDesc> getRestricted(Context context) {
		List<RestrictionDesc> result = new ArrayList<RestrictionDesc>();
		Cursor rCursor = context.getContentResolver().query(PrivacyProvider.URI_RESTRICTION, null, null,
				new String[] { Integer.toString(0), Boolean.toString(false) }, null);
		while (rCursor.moveToNext()) {
			RestrictionDesc restriction = new RestrictionDesc();
			restriction.uid = rCursor.getInt(rCursor.getColumnIndex(PrivacyProvider.COL_UID));
			restriction.restricted = Boolean.parseBoolean(rCursor.getString(rCursor
					.getColumnIndex(PrivacyProvider.COL_RESTRICTED)));
			restriction.restrictionName = rCursor.getString(rCursor.getColumnIndex(PrivacyProvider.COL_RESTRICTION));
			restriction.methodName = rCursor.getString(rCursor.getColumnIndex(PrivacyProvider.COL_METHOD));
			result.add(restriction);
		}
		rCursor.close();
		return result;
	}

	public static void deleteRestrictions(Context context, int uid) {
		context.getContentResolver().delete(PrivacyProvider.URI_RESTRICTION, null,
				new String[] { Integer.toString(uid) });
		Util.log(null, Log.INFO, "Deleted restrictions uid=" + uid);
	}

	// Usage

	public static long getUsed(Context context, int uid, String restrictionName, String methodName) {
		long lastUsage = 0;
		ContentResolver cr = context.getContentResolver();
		Cursor cursor = cr.query(PrivacyProvider.URI_USAGE, null, restrictionName, new String[] {
				Integer.toString(uid), methodName }, null);
		while (cursor.moveToNext()) {
			long usage = cursor.getLong(cursor.getColumnIndex(PrivacyProvider.COL_USED));
			if (usage > lastUsage)
				lastUsage = usage;
		}
		cursor.close();
		boolean used = (lastUsage != 0);
		logRestriction(null, context, uid, "used", restrictionName, methodName, used, false, 0);
		return lastUsage;
	}

	public static void deleteUsageData(Context context, int uid) {
		for (String restrictionName : PrivacyManager.getRestrictions())
			context.getContentResolver().delete(PrivacyProvider.URI_USAGE, restrictionName,
					new String[] { Integer.toString(uid) });
		Util.log(null, Log.INFO, "Deleted usage uid=" + uid);
	}

	// Settings

	public static boolean getSettingBool(XHook hook, Context context, String settingName, boolean defaultValue,
			boolean useCache) {
		return Boolean.parseBoolean(PrivacyManager.getSetting(hook, context, settingName, Boolean
				.toString(defaultValue).toString(), useCache));
	}

	public static String getSetting(XHook hook, Context context, String settingName, String defaultValue,
			boolean useCache) {
		long start = System.currentTimeMillis();

		// Check cache
		if (useCache)
			synchronized (mSettingsCache) {
				if (mSettingsCache.containsKey(settingName)) {
					CSetting entry = mSettingsCache.get(settingName);
					if (entry.isExpired())
						mSettingsCache.remove(settingName);
					else {
						String value = mSettingsCache.get(settingName).getSettingsValue();
						Util.log(hook, Log.INFO, String.format("get setting %s=%s *", settingName, value));
						return value;
					}
				}
			}

		// Get setting
		boolean fallback = true;
		String value = null;
		if (context != null) {
			try {
				ContentResolver contentResolver = context.getContentResolver();
				if (contentResolver == null) {
					Util.log(hook, Log.WARN, "contentResolver is null");
					Util.logStack(hook);
				} else {
					Cursor cursor = contentResolver.query(PrivacyProvider.URI_SETTING, null, settingName, null, null);
					if (cursor == null) {
						// Can happen if memory low
						Util.log(hook, Log.WARN, "cursor is null");
						Util.logStack(null);
					} else {
						if (cursor.moveToNext()) {
							value = cursor.getString(cursor.getColumnIndex(PrivacyProvider.COL_VALUE));
							fallback = false;
						} else {
							Util.log(hook, Log.WARN, "cursor is empty");
						}
						cursor.close();
					}
				}
			} catch (Throwable ex) {
				Util.bug(hook, ex);
				Util.logStack(hook);
			}
		}

		// Use fallback
		if (fallback)
			value = PrivacyProvider.getSettingFallback(settingName, defaultValue);

		// Default value
		if (value == null || value.equals(""))
			value = defaultValue;

		// Add to cache
		synchronized (mSettingsCache) {
			if (mSettingsCache.containsKey(settingName))
				mSettingsCache.remove(settingName);
			mSettingsCache.put(settingName, new CSetting(value));
		}

		long ms = System.currentTimeMillis() - start;
		Util.log(
				hook,
				Log.INFO,
				String.format("get setting %s=%s%s%s", settingName, value, (fallback ? " #" : ""), (ms > 1 ? " " + ms
						+ " ms" : "")));
		return value;
	}

	public static void setSetting(XHook hook, Context context, String settingName, String value) {
		ContentResolver contentResolver = context.getContentResolver();
		ContentValues values = new ContentValues();
		values.put(PrivacyProvider.COL_VALUE, value);
		if (contentResolver.update(PrivacyProvider.URI_SETTING, values, settingName, null) <= 0)
			Util.log(hook, Log.INFO, "Error updating setting=" + settingName);
		Util.log(hook, Log.INFO, String.format("set setting %s=%s", settingName, value));
	}

	public static Map<String, String> getSettings(Context context) {
		Map<String, String> result = new HashMap<String, String>();
		Cursor sCursor = context.getContentResolver().query(PrivacyProvider.URI_SETTING, null, null, null, null);
		while (sCursor.moveToNext()) {
			// Get setting
			String setting = sCursor.getString(sCursor.getColumnIndex(PrivacyProvider.COL_SETTING));
			String value = sCursor.getString(sCursor.getColumnIndex(PrivacyProvider.COL_VALUE));
			result.put(setting, value);
		}
		sCursor.close();
		return result;
	}

	public static void deleteSettings(Context context) {
		context.getContentResolver().delete(PrivacyProvider.URI_SETTING, null, null);
		Util.log(null, Log.INFO, "Deleted settings");
	}

	// Defacing

	public static Object getDefacedProp(String name) {
		// Serial number
		if (name.equals("SERIAL") || name.equals("%serialno") || name.equals("%hostname"))
			return cDeface;

		// MAC addresses
		if (name.equals("MAC") || name.equals("%macaddr")) {
			String mac = getSetting(null, null, cSettingMac, "DE:FA:CE:DE:FA:CE", true);
			StringBuilder sb = new StringBuilder(mac.replace(":", ""));
			while (sb.length() != 12)
				sb.insert(0, '0');
			while (sb.length() > 12)
				sb.deleteCharAt(sb.length() - 1);
			for (int i = 10; i > 0; i -= 2)
				sb.insert(i, ':');
			return sb.toString();
		}

		// IMEI
		if (name.equals("getDeviceId") || name.equals("%imei"))
			return getSetting(null, null, cSettingImei, cDeface, true);

		// Phone
		if (name.equals("PhoneNumber") || name.equals("getLine1AlphaTag") || name.equals("getLine1Number")
				|| name.equals("getMsisdn") || name.equals("getVoiceMailAlphaTag") || name.equals("getVoiceMailNumber"))
			return getSetting(null, null, cSettingPhone, cDeface, true);

		// Android ID
		if (name.equals("ANDROID_ID"))
			return getSetting(null, null, cSettingId, cDeface, true);

		// Telephony manager
		if (name.equals("getIsimDomain"))
			return null;
		if (name.equals("getIsimImpi"))
			return null;
		if (name.equals("getIsimImpu"))
			return null;

		if (name.equals("getNetworkCountryIso")) // MCC: test network
			return getSetting(null, null, cSettingMcc, "001", true);
		if (name.equals("getNetworkOperator")) // MCC+MNC: test network
			return getSetting(null, null, cSettingMcc, "001", true) + getSetting(null, null, cSettingMnc, "01", true);
		if (name.equals("getNetworkOperatorName"))
			return cDeface;

		if (name.equals("getSimCountryIso")) // ISO country code
			return getSetting(null, null, cSettingCountry, "XX", true);
		if (name.equals("getSimOperator"))
			return cDeface;
		if (name.equals("getSimOperatorName")) // MCC+MNC: test network
			return getSetting(null, null, cSettingMcc, "001", true) + getSetting(null, null, cSettingMnc, "01", true);
		if (name.equals("getSimSerialNumber"))
			return getSetting(null, null, cSettingIccId, null, true);

		if (name.equals("getSubscriberId")) // IMSI for a GSM phone
			return getSetting(null, null, cSettingSubscriber, null, true);

		if (name.equals("SSID"))
			return ""; // Hidden network
		if (name.equals("WifiSsid.octets")) {
			int size = 8;
			ByteArrayOutputStream octets = new ByteArrayOutputStream(size);
			for (int i = 0; i < size; i++)
				octets.write(0);
			return octets; // Hidden network
		}

		// Google services framework ID
		if (name.equals("GSF_ID")) {
			long gsfid = 0xDEFACE;
			try {
				gsfid = Long.parseLong(getSetting(null, null, cSettingGsfId, "DEFACE", true), 16);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return gsfid;
		}

		if (name.equals("InetAddress")) {
			try {
				Field unspecified = Inet4Address.class.getDeclaredField("ALL");
				unspecified.setAccessible(true);
				return (InetAddress) unspecified.get(Inet4Address.class);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return null;
			}
		}

		if (name.equals("IPInt"))
			return 127 + (0 << 8) + (0 << 16) + (1 << 24);

		if (name.equals("Bytes3"))
			return new byte[] { (byte) 0xDE, (byte) 0xFA, (byte) 0xCE };

		// Fallback
		return cDeface;
	}

	public static Location getDefacedLocation(Location location) {
		String sLat = getSetting(null, null, PrivacyManager.cSettingLatitude, "", true);
		String sLon = getSetting(null, null, PrivacyManager.cSettingLongitude, "", true);
		if (sLat.equals("") || sLon.equals("")) {
			// Christmas Island
			location.setLatitude(-10.5);
			location.setLongitude(105.667);
		} else {
			// 1 degree ~ 111111 m
			// 1 m ~ 0,000009 degrees = 9e-6
			location.setLatitude(Float.parseFloat(sLat) + (Math.random() * 2.0 - 1.0) * location.getAccuracy() * 9e-6);
			location.setLongitude(Float.parseFloat(sLon) + (Math.random() * 2.0 - 1.0) * location.getAccuracy() * 9e-6);
		}
		return location;
	}

	// Helper methods

	private static void logRestriction(XHook hook, Context context, int uid, String prefix, String restrictionName,
			String methodName, boolean restricted, boolean cached, long ms) {
		Util.log(hook, Log.INFO, String.format("%s %d/%s %s=%b%s%s", prefix, uid, methodName, restrictionName,
				restricted, (cached ? " *" : (context == null ? " #" : "")), (ms > 1 ? " " + ms + " ms" : "")));
	}

	public static boolean hasInternet(Context context, String packageName) {
		// TODO: check if internet permission restricted
		PackageManager pm = context.getPackageManager();
		return (pm.checkPermission("android.permission.INTERNET", packageName) == PackageManager.PERMISSION_GRANTED);
	}

	@SuppressLint("DefaultLocale")
	public static boolean hasPermission(Context context, String packageName, String restrictionName) {
		try {
			List<String> listPermission = mPermissions.get(restrictionName);
			if (listPermission.size() == 0)
				return true;
			PackageManager pm = context.getPackageManager();
			PackageInfo pInfo = pm.getPackageInfo(packageName, PackageManager.GET_PERMISSIONS);
			if (pInfo != null && pInfo.requestedPermissions != null)
				for (String rPermission : pInfo.requestedPermissions)
					for (String permission : listPermission)
						if (permission.equals("")) {
							// No permission required
							return true;
						} else if (rPermission.toLowerCase().contains(permission.toLowerCase()))
							return true;
						else if (permission.contains("."))
							if (pm.checkPermission(permission, packageName) == PackageManager.PERMISSION_GRANTED)
								return true;
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return false;
		}
		return false;
	}

	// Helper classes

	private static class CRestriction {
		private long mTimestamp;
		private boolean mRestricted;

		public CRestriction(boolean restricted) {
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

	private static class CSetting {
		private long mTimestamp;
		private String mValue;

		public CSetting(String settingValue) {
			mTimestamp = new Date().getTime();
			mValue = settingValue;
		}

		public boolean isExpired() {
			return (mTimestamp + cCacheTimeoutMs < new Date().getTime());
		}

		public String getSettingsValue() {
			return mValue;
		}
	}

	private static class UsageData {
		private Integer mUid;
		private String mRestriction;
		private String mMethodName;
		private long mTimeStamp;
		private int mHash;

		public UsageData(int uid, String restrictionName, String methodName) {
			mUid = uid;
			mRestriction = restrictionName;
			mMethodName = methodName;
			mTimeStamp = new Date().getTime();

			mHash = mUid.hashCode();
			if (mRestriction != null)
				mHash = mHash ^ mRestriction.hashCode();
			if (mMethodName != null)
				mHash = mHash ^ mMethodName.hashCode();
		}

		public int getUid() {
			return mUid;
		}

		public String getRestrictionName() {
			return mRestriction;
		}

		public String getMethodName() {
			return mMethodName;
		}

		public long getTimeStamp() {
			return mTimeStamp;
		}

		@Override
		public int hashCode() {
			return mHash;
		}

		@Override
		public boolean equals(Object obj) {
			UsageData other = (UsageData) obj;
			// @formatter:off
			return
				(mUid.equals(other.mUid) &&
				(mRestriction == null ? other.mRestriction == null : mRestriction.equals(other.mRestriction)) &&
				(mMethodName == null ? other.mMethodName == null : mMethodName.equals(other.mMethodName)));
			// @formatter:on
		}

		@Override
		@SuppressLint("DefaultLocale")
		public String toString() {
			return String.format("%d/%s/%s", mUid, mRestriction, mMethodName);
		}
	}
}

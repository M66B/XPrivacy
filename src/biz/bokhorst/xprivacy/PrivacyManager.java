package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;
import java.util.UUID;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.location.Location;
import android.os.Build;
import android.os.Process;
import android.util.Log;
import android.util.SparseArray;

public class PrivacyManager {
	// This should correspond with restrict_<name> in strings.xml
	public static final String cAccounts = "accounts";
	public static final String cBrowser = "browser";
	public static final String cCalendar = "calendar";
	public static final String cCalling = "calling";
	public static final String cClipboard = "clipboard";
	public static final String cContacts = "contacts";
	public static final String cDictionary = "dictionary";
	public static final String cEMail = "email";
	public static final String cIdentification = "identification";
	public static final String cInternet = "internet";
	public static final String cIPC = "ipc";
	public static final String cLocation = "location";
	public static final String cMedia = "media";
	public static final String cMessages = "messages";
	public static final String cNetwork = "network";
	public static final String cNfc = "nfc";
	public static final String cNotifications = "notifications";
	public static final String cOverlay = "overlay";
	public static final String cPhone = "phone";
	public static final String cSensors = "sensors";
	public static final String cShell = "shell";
	public static final String cStorage = "storage";
	public static final String cSystem = "system";
	public static final String cView = "view";

	// This should correspond with the above definitions
	private static final String cRestrictionNames[] = new String[] { cAccounts, cBrowser, cCalendar, cCalling,
			cClipboard, cContacts, cDictionary, cEMail, cIdentification, cInternet, cIPC, cLocation, cMedia, cMessages,
			cNetwork, cNfc, cNotifications, cOverlay, cPhone, cSensors, cShell, cStorage, cSystem, cView };

	// Setting names
	public final static String cSettingSerial = "Serial";
	public final static String cSettingLatitude = "Latitude";
	public final static String cSettingLongitude = "Longitude";
	public final static String cSettingMac = "Mac";
	public final static String cSettingIP = "IP";
	public final static String cSettingImei = "IMEI";
	public final static String cSettingPhone = "Phone";
	public final static String cSettingId = "ID";
	public final static String cSettingGsfId = "GSF_ID";
	public final static String cSettingAdId = "AdId";
	public final static String cSettingMcc = "MCC";
	public final static String cSettingMnc = "MNC";
	public final static String cSettingCountry = "Country";
	public final static String cSettingOperator = "Operator";
	public final static String cSettingIccId = "ICC_ID";
	public final static String cSettingSubscriber = "Subscriber";
	public final static String cSettingSSID = "SSID";
	public final static String cSettingUa = "UA";
	public final static String cSettingFUsed = "FUsed";
	public final static String cSettingFInternet = "FInternet";
	public final static String cSettingFRestriction = "FRestriction";
	public final static String cSettingFRestrictionNot = "FRestrictionNot";
	public final static String cSettingFPermission = "FPermission";
	public final static String cSettingFUser = "FUser";
	public final static String cSettingFSystem = "FSystem";
	public final static String cSettingTheme = "Theme";
	public final static String cSettingSalt = "Salt";
	public final static String cSettingVersion = "Version";
	public final static String cSettingFirstRun = "FirstRun";
	public final static String cSettingTutorialMain = "TutorialMain";
	public final static String cSettingTutorialDetails = "TutorialDetails";
	public final static String cSettingNotify = "Notify";
	public final static String cSettingLog = "Log";
	public final static String cSettingDangerous = "Dangerous";
	public final static String cSettingExperimental = "Experimental";
	public final static String cSettingRandom = "Random@boot";
	public final static String cSettingState = "State";
	public final static String cSettingConfidence = "Confidence";
	public final static String cSettingHttps = "Https";
	public final static String cSettingRegistered = "Registered";
	public final static String cSettingUsage = "UsageData";
	public final static String cSettingSystem = "RestrictSystem";
	public final static String cSettingRestricted = "Retricted";

	public final static String cSettingTemplate = "Template";
	public final static String cSettingAccount = "Account.";
	public final static String cSettingApplication = "Application.";
	public final static String cSettingContact = "Contact.";
	public final static String cSettingRawContact = "RawContact.";

	// Special value names
	public final static String cValueRandom = "#Random#";
	public final static String cValueRandomLegacy = "\nRandom\n";

	// Constants
	public final static int cXposedAppProcessMinVersion = 46;
	public final static boolean cTestVersion = true;

	private final static String cDeface = "DEFACE";
	public final static int cRestrictionCacheTimeoutMs = 15 * 1000;
	public final static int cSettingCacheTimeoutMs = 30 * 1000;
	public final static int cUseProviderAfterMs = 3 * 60 * 1000;

	// Static data
	private static Map<String, List<Hook>> mMethod = new LinkedHashMap<String, List<Hook>>();
	private static Map<String, List<Hook>> mPermission = new LinkedHashMap<String, List<Hook>>();
	private static Map<CSetting, CSetting> mSettingsCache = new HashMap<CSetting, CSetting>();
	private static Map<CRestriction, CRestriction> mRestrictionCache = new HashMap<CRestriction, CRestriction>();
	public static SparseArray<Map<String, Boolean>> mPermissionRestrictionCache = new SparseArray<Map<String, Boolean>>();
	public static SparseArray<Map<Hook, Boolean>> mPermissionHookCache = new SparseArray<Map<Hook, Boolean>>();

	// Meta data

	static {
		List<String> listRestriction = getRestrictions();

		for (Hook hook : Meta.get())
			if (Build.VERSION.SDK_INT >= hook.getSdk()) {
				String restrictionName = hook.getRestrictionName();

				// Check restriction
				if (!listRestriction.contains(restrictionName))
					Util.log(null, Log.WARN, "Not found restriction=" + restrictionName);

				// Enlist method
				if (!mMethod.containsKey(restrictionName))
					mMethod.put(restrictionName, new ArrayList<Hook>());
				mMethod.get(restrictionName).add(hook);

				// Enlist permissions
				String[] permissions = hook.getPermissions();
				if (permissions != null)
					for (String perm : permissions)
						if (!perm.equals("")) {
							String aPermission = (perm.contains(".") ? perm : "android.permission." + perm);
							if (!mPermission.containsKey(aPermission))
								mPermission.put(aPermission, new ArrayList<Hook>());
							if (!mPermission.get(aPermission).contains(hook))
								mPermission.get(aPermission).add(hook);
						}
			}
	}

	public static void registerHook(String restrictionName, String methodName, int sdk) {
		if (restrictionName != null && methodName != null && Build.VERSION.SDK_INT >= sdk) {
			if (!mMethod.containsKey(restrictionName)
					|| !mMethod.get(restrictionName).contains(new Hook(restrictionName, methodName)))
				Util.log(null, Log.WARN, "Missing method " + methodName);
		}
	}

	public static List<String> getRestrictions() {
		return new ArrayList<String>(Arrays.asList(cRestrictionNames));
	}

	public static TreeMap<String, String> getRestrictions(Context context) {
		Collator collator = Collator.getInstance(Locale.getDefault());
		TreeMap<String, String> tmRestriction = new TreeMap<String, String>(collator);
		String packageName = PrivacyManager.class.getPackage().getName();
		for (String restrictionName : getRestrictions()) {
			int stringId = context.getResources().getIdentifier("restrict_" + restrictionName, "string", packageName);
			tmRestriction.put(stringId == 0 ? restrictionName : context.getString(stringId), restrictionName);
		}
		return tmRestriction;
	}

	public static Hook getHook(String restrictionName, String methodName) {
		if (mMethod.containsKey(restrictionName)) {
			Hook md = new Hook(restrictionName, methodName);
			int pos = mMethod.get(restrictionName).indexOf(md);
			return (pos < 0 ? null : mMethod.get(restrictionName).get(pos));
		} else
			return null;
	}

	public static List<Hook> getHooks(String restrictionName) {
		List<Hook> listMethod = new ArrayList<Hook>();
		List<Hook> listMethodOrig = mMethod.get(restrictionName);
		if (listMethodOrig != null)
			listMethod.addAll(listMethodOrig);
		// null can happen when upgrading
		Collections.sort(listMethod);
		return listMethod;
	}

	public static List<String> getPermissions(String restrictionName) {
		List<String> listPermission = new ArrayList<String>();
		for (Hook md : getHooks(restrictionName))
			if (md.getPermissions() != null)
				for (String permission : md.getPermissions())
					if (!listPermission.contains(permission))
						listPermission.add(permission);
		return listPermission;
	}

	// Restrictions

	public static boolean getRestriction(final XHook hook, int uid, String restrictionName, String methodName,
			boolean usage, boolean useCache, String secret) {
		long start = System.currentTimeMillis();
		boolean restricted = false;

		// Check uid
		if (uid <= 0) {
			Util.log(hook, Log.WARN, "uid <= 0");
			Util.logStack(hook);
			return restricted;
		}

		// Check restriction
		if (restrictionName == null || restrictionName.equals("")) {
			Util.log(hook, Log.WARN, "restriction empty method=" + methodName);
			Util.logStack(hook);
			return restricted;
		}

		// Check usage
		if (usage)
			if (methodName == null || methodName.equals("")) {
				Util.log(hook, Log.WARN, "Method empty");
				Util.logStack(hook);
			} else if (PrivacyManager.cTestVersion
					&& getHooks(restrictionName).indexOf(new Hook(restrictionName, methodName)) < 0)
				Util.log(hook, Log.WARN, "Unknown method=" + methodName);

		// Check cache
		boolean cached = false;
		CRestriction key = new CRestriction(uid, restrictionName, methodName);
		if (useCache)
			synchronized (mRestrictionCache) {
				if (mRestrictionCache.containsKey(key)) {
					CRestriction entry = mRestrictionCache.get(key);
					if (!entry.isExpired()) {
						cached = true;
						restricted = entry.isRestricted();
					}
				}
			}

		// Get restriction
		if (!cached)
			try {
				restricted = PrivacyService.getClient().getRestriction(
						new ParcelableRestriction(uid, restrictionName, methodName, false), usage, secret);

				// Add to cache
				key.setRestricted(restricted);
				synchronized (mRestrictionCache) {
					if (mRestrictionCache.containsKey(key))
						mRestrictionCache.remove(key);
					mRestrictionCache.put(key, key);
				}
			} catch (Throwable ex) {
				Util.bug(hook, ex);
			}

		// Result
		long ms = System.currentTimeMillis() - start;
		if (ms > 1)
			Util.log(hook, Log.INFO, String.format("get %d/%s %s=%srestricted%s %d ms", uid, methodName,
					restrictionName, (restricted ? "" : "!"), (cached ? " (cached)" : ""), ms));
		else
			Util.log(hook, Log.INFO, String.format("get %d/%s %s=%srestricted%s", uid, methodName, restrictionName,
					(restricted ? "" : "!"), (cached ? " (cached)" : "")));

		return restricted;
	}

	public static boolean setRestriction(XHook hook, int uid, String restrictionName, String methodName,
			boolean restricted, boolean changeState) {
		// Check uid
		if (uid == 0) {
			Util.log(hook, Log.WARN, "uid=0");
			return false;
		}

		// Set restriction
		try {
			PrivacyService.getClient().setRestriction(
					new ParcelableRestriction(uid, restrictionName, methodName, restricted));
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		// Make exceptions for dangerous methods
		boolean dangerous = PrivacyManager.getSettingBool(hook, 0, PrivacyManager.cSettingDangerous, false, false);
		if (methodName == null)
			if (restricted && !dangerous) {
				for (Hook md : getHooks(restrictionName))
					if (md.isDangerous())
						PrivacyManager.setRestriction(hook, uid, restrictionName, md.getName(), dangerous, changeState);
			}

		// Mark state as restricted
		if (restricted && changeState)
			PrivacyManager.setSetting(hook, uid, PrivacyManager.cSettingState,
					Integer.toString(ActivityMain.STATE_RESTRICTED));

		// Check if restart required
		return shouldRestart(restrictionName, methodName, restricted);
	}

	public static boolean setRestrictionList(List<ParcelableRestriction> listRestriction) {
		boolean restart = false;
		if (listRestriction.size() > 0)
			try {
				for (ParcelableRestriction restriction : listRestriction)
					restart = restart
							|| shouldRestart(restriction.restrictionName, restriction.methodName,
									restriction.restricted);
				PrivacyService.getClient().setRestrictionList(listRestriction);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		return restart;
	}

	public static List<ParcelableRestriction> getRestrictionList(int uid, String restrictionName) {
		try {
			return PrivacyService.getClient().getRestrictionList(
					new ParcelableRestriction(uid, restrictionName, null, false));
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
		return new ArrayList<ParcelableRestriction>();
	}

	private static boolean shouldRestart(String restrictionName, String methodName, boolean restricted) {
		boolean dangerous = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingDangerous, false, false);
		if (methodName == null) {
			for (Hook md : getHooks(restrictionName))
				if (md.isRestartRequired() && !(restricted && !dangerous && md.isDangerous()))
					return true;
			return false;
		} else {
			Hook md = getHook(restrictionName, methodName);
			return (md == null ? false : md.isRestartRequired());
		}
	}

	public static boolean deleteRestrictions(int uid, boolean changeState) {
		// Check if restart required
		boolean restart = false;
		for (String restrictionName : getRestrictions()) {
			for (Hook md : getHooks(restrictionName))
				if (getRestriction(null, uid, restrictionName, md.getName(), false, false, null))
					if (md.isRestartRequired()) {
						restart = true;
						break;
					}
			if (restart)
				break;
		}

		// Delete restrictions
		try {
			PrivacyService.getClient().deleteRestrictions(uid);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		// Mark as new/changed
		if (changeState)
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingState,
					Integer.toString(ActivityMain.STATE_ATTENTION));

		return restart;
	}

	// Usage

	public static long getUsed(int uid, String restrictionName, String methodName) {
		try {
			List<ParcelableRestriction> listRestriction = new ArrayList<ParcelableRestriction>();
			if (restrictionName == null)
				for (String sRestrictionName : PrivacyManager.getRestrictions())
					listRestriction.add(new ParcelableRestriction(uid, sRestrictionName, methodName, false));
			else
				listRestriction.add(new ParcelableRestriction(uid, restrictionName, methodName, false));
			return PrivacyService.getClient().getUsage(listRestriction);
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return 0;
		}
	}

	public static List<ParcelableRestriction> getUsed(Context context, int uid) {
		List<ParcelableRestriction> listUsage = new ArrayList<ParcelableRestriction>();
		try {
			listUsage.addAll(PrivacyService.getClient().getUsageList(uid));
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
		Collections.sort(listUsage, new ParcelableRestrictionCompare());
		return listUsage;
	}

	public static class ParcelableRestrictionCompare implements Comparator<ParcelableRestriction> {
		@Override
		public int compare(ParcelableRestriction one, ParcelableRestriction another) {
			if (one.time < another.time)
				return 1;
			else if (one.time > another.time)
				return -1;
			else
				return 0;
		}
	}

	public static void deleteUsage(int uid) {
		try {
			PrivacyService.getClient().deleteUsage(uid);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	// Settings

	public static boolean getSettingBool(XHook hook, int uid, String name, boolean defaultValue, boolean useCache) {
		return Boolean.parseBoolean(getSetting(hook, uid, name, Boolean.toString(defaultValue), useCache));
	}

	public static String getSetting(XHook hook, int uid, String name, String defaultValue, boolean useCache) {
		long start = System.currentTimeMillis();
		String value = null;

		// Check cache
		boolean cached = false;
		boolean willExpire = false;
		CSetting key = new CSetting(uid, name);
		if (useCache)
			synchronized (mSettingsCache) {
				if (mSettingsCache.containsKey(key)) {
					CSetting entry = mSettingsCache.get(key);
					if (!entry.isExpired()) {
						cached = true;
						value = entry.getValue();
						willExpire = entry.willExpire();
					}
				}
			}

		// Get settings
		if (!cached)
			try {
				IPrivacyService client = PrivacyService.getClient();
				if (client == null)
					value = defaultValue;
				else {
					value = client.getSetting(new ParcelableSetting(Math.abs(uid), name, null)).value;
					if (value == null)
						if (uid > 0)
							value = client.getSetting(new ParcelableSetting(0, name, defaultValue)).value;
						else
							value = defaultValue;
				}

				// Add to cache
				key.setValue(value);
				synchronized (mSettingsCache) {
					if (mSettingsCache.containsKey(key))
						mSettingsCache.remove(key);
					mSettingsCache.put(key, key);
				}
			} catch (Throwable ex) {
				Util.bug(hook, ex);
			}

		long ms = System.currentTimeMillis() - start;
		if (!willExpire && !PrivacyManager.cSettingLog.equals(name))
			if (ms > 1)
				Util.log(hook, Log.INFO, String.format("get setting uid=%d %s=%s%s %d ms", uid, name, value,
						(cached ? " (cached)" : ""), ms));
			else
				Util.log(hook, Log.INFO,
						String.format("get setting uid=%d %s=%s%s", uid, name, value, (cached ? " (cached)" : "")));

		return value;
	}

	public static void setSetting(XHook hook, int uid, String name, String value) {
		try {
			PrivacyService.getClient().setSetting(new ParcelableSetting(uid, name, value));
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	public static void setSettingList(List<ParcelableSetting> listSetting) {
		if (listSetting.size() > 0)
			try {
				PrivacyService.getClient().setSettingList(listSetting);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
	}

	public static List<ParcelableSetting> getSettingList(int uid) {
		try {
			return PrivacyService.getClient().getSettingList(uid);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
		return new ArrayList<ParcelableSetting>();
	}

	public static void deleteSettings(int uid) {
		try {
			PrivacyService.getClient().deleteSettings(uid);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	// Defacing

	@SuppressLint("DefaultLocale")
	public static Object getDefacedProp(int uid, String name) {
		// Serial number
		if (name.equals("SERIAL") || name.equals("%serialno")) {
			String value = getSetting(null, uid, cSettingSerial, cDeface, true);
			return (cValueRandom.equals(value) ? getRandomProp("SERIAL") : value);
		}

		// Host name
		if (name.equals("%hostname"))
			return cDeface;

		// MAC addresses
		if (name.equals("MAC") || name.equals("%macaddr")) {
			String mac = getSetting(null, uid, cSettingMac, "DE:FA:CE:DE:FA:CE", true);
			if (cValueRandom.equals(mac))
				return getRandomProp("MAC");
			StringBuilder sb = new StringBuilder(mac.replace(":", ""));
			while (sb.length() != 12)
				sb.insert(0, '0');
			while (sb.length() > 12)
				sb.deleteCharAt(sb.length() - 1);
			for (int i = 10; i > 0; i -= 2)
				sb.insert(i, ':');
			return sb.toString();
		}

		// cid
		if (name.equals("%cid"))
			return cDeface;

		// IMEI
		if (name.equals("getDeviceId") || name.equals("%imei")) {
			String value = getSetting(null, uid, cSettingImei, "000000000000000", true);
			return (cValueRandom.equals(value) ? getRandomProp("IMEI") : value);
		}

		// Phone
		if (name.equals("PhoneNumber") || name.equals("getLine1AlphaTag") || name.equals("getLine1Number")
				|| name.equals("getMsisdn") || name.equals("getVoiceMailAlphaTag") || name.equals("getVoiceMailNumber")) {
			String value = getSetting(null, uid, cSettingPhone, cDeface, true);
			return (cValueRandom.equals(value) ? getRandomProp("PHONE") : value);
		}

		// Android ID
		if (name.equals("ANDROID_ID")) {
			String value = getSetting(null, uid, cSettingId, cDeface, true);
			return (cValueRandom.equals(value) ? getRandomProp("ANDROID_ID") : value);
		}

		// Telephony manager
		if (name.equals("getGroupIdLevel1"))
			return null;
		if (name.equals("getIsimDomain"))
			return null;
		if (name.equals("getIsimImpi"))
			return null;
		if (name.equals("getIsimImpu"))
			return null;

		if (name.equals("getNetworkCountryIso") || name.equals("gsm.operator.iso-country")) {
			// ISO country code
			String value = getSetting(null, uid, cSettingCountry, "XX", true);
			return (cValueRandom.equals(value) ? getRandomProp("ISO3166") : value);
		}
		if (name.equals("getNetworkOperator") || name.equals("gsm.operator.numeric"))
			// MCC+MNC: test network
			return getSetting(null, uid, cSettingMcc, "001", true) + getSetting(null, uid, cSettingMnc, "01", true);
		if (name.equals("getNetworkOperatorName") || name.equals("gsm.operator.alpha"))
			return getSetting(null, uid, cSettingOperator, cDeface, true);

		if (name.equals("getSimCountryIso") || name.equals("gsm.sim.operator.iso-country")) {
			// ISO country code
			String value = getSetting(null, uid, cSettingCountry, "XX", true);
			return (cValueRandom.equals(value) ? getRandomProp("ISO3166") : value);
		}
		if (name.equals("getSimOperator") || name.equals("gsm.sim.operator.numeric"))
			// MCC+MNC: test network
			return getSetting(null, uid, cSettingMcc, "001", true) + getSetting(null, uid, cSettingMnc, "01", true);
		if (name.equals("getSimOperatorName") || name.equals("gsm.sim.operator.alpha"))
			return getSetting(null, uid, cSettingOperator, cDeface, true);

		if (name.equals("getSimSerialNumber") || name.equals("getIccSerialNumber"))
			return getSetting(null, uid, cSettingIccId, null, true);

		if (name.equals("getSubscriberId")) { // IMSI for a GSM phone
			String value = getSetting(null, uid, cSettingSubscriber, null, true);
			return (cValueRandom.equals(value) ? getRandomProp("SubscriberId") : value);
		}

		if (name.equals("SSID")) {
			// Default hidden network
			String value = getSetting(null, uid, cSettingSSID, "", true);
			return (cValueRandom.equals(value) ? getRandomProp("SSID") : value);
		}

		// Google services framework ID
		if (name.equals("GSF_ID")) {
			long gsfid = 0xDEFACE;
			try {
				String value = getSetting(null, uid, cSettingGsfId, "DEFACE", true);
				if (cValueRandom.equals(value))
					value = getRandomProp(name);
				gsfid = Long.parseLong(value.toLowerCase(), 16);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return gsfid;
		}

		// Advertisement ID
		if (name.equals("AdvertisingId")) {
			String adid = getSetting(null, uid, cSettingAdId, "DEFACE00-0000-0000-0000-000000000000", true);
			if (cValueRandom.equals(adid))
				adid = getRandomProp(name);
			return adid;
		}

		if (name.equals("InetAddress")) {
			// Set address
			String ip = getSetting(null, uid, cSettingIP, null, true);
			if (ip != null)
				try {
					return InetAddress.getByName(ip);
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}

			// Any address (0.0.0.0)
			try {
				Field unspecified = Inet4Address.class.getDeclaredField("ANY");
				unspecified.setAccessible(true);
				return (InetAddress) unspecified.get(Inet4Address.class);
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return null;
			}
		}

		if (name.equals("IPInt")) {
			// Set address
			String ip = getSetting(null, uid, cSettingIP, null, true);
			if (ip != null)
				try {
					InetAddress inet = InetAddress.getByName(ip);
					if (inet.getClass().equals(Inet4Address.class)) {
						byte[] b = inet.getAddress();
						return b[0] + (b[1] << 8) + (b[2] << 16) + (b[3] << 24);
					}
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}

			// Any address (0.0.0.0)
			return 0;
		}

		if (name.equals("Bytes3"))
			return new byte[] { (byte) 0xDE, (byte) 0xFA, (byte) 0xCE };

		if (name.equals("UA"))
			return getSetting(null, uid, cSettingUa,
					"Mozilla/5.0 (Linux; U; Android; en-us) AppleWebKit/999+ (KHTML, like Gecko) Safari/999.9", true);

		// InputDevice
		if (name.equals("DeviceDescriptor"))
			return cDeface;

		// getExtraInfo
		if (name.equals("ExtraInfo"))
			return cDeface;

		// Fallback
		Util.log(null, Log.WARN, "Fallback value name=" + name);
		return cDeface;
	}

	public static Location getDefacedLocation(int uid, Location location) {
		String sLat = getSetting(null, uid, cSettingLatitude, null, true);
		String sLon = getSetting(null, uid, cSettingLongitude, null, true);

		if (cValueRandom.equals(sLat))
			sLat = getRandomProp("LAT");
		if (cValueRandom.equals(sLon))
			sLon = getRandomProp("LON");

		// 1 degree ~ 111111 m
		// 1 m ~ 0,000009 degrees
		// Christmas Island ~ -10.5 / 105.667

		if (sLat == null || "".equals(sLat))
			location.setLatitude(-10.5);
		else
			location.setLatitude(Float.parseFloat(sLat) + (Math.random() * 2.0 - 1.0) * location.getAccuracy() * 9e-6);

		if (sLon == null || "".equals(sLon))
			location.setLongitude(105.667);
		else
			location.setLongitude(Float.parseFloat(sLon) + (Math.random() * 2.0 - 1.0) * location.getAccuracy() * 9e-6);

		return location;
	}

	@SuppressLint("DefaultLocale")
	public static String getRandomProp(String name) {
		Random r = new Random();

		if (name.equals("SERIAL")) {
			long v = r.nextLong();
			return Long.toHexString(v).toUpperCase();
		}

		if (name.equals("MAC")) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < 6; i++) {
				if (i != 0)
					sb.append(':');
				int v = r.nextInt(256);
				if (i == 0)
					v = v & 0xFC; // unicast, globally unique
				sb.append(Integer.toHexString(0x100 | v).substring(1));
			}
			return sb.toString().toUpperCase();
		}

		// IMEI
		if (name.equals("IMEI")) {
			// http://en.wikipedia.org/wiki/Reporting_Body_Identifier
			String[] rbi = new String[] { "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "30", "33",
					"35", "44", "45", "49", "50", "51", "52", "53", "54", "86", "91", "98", "99" };
			String imei = rbi[r.nextInt(rbi.length)];
			while (imei.length() < 14)
				imei += Character.forDigit(r.nextInt(10), 10);
			imei += getLuhnDigit(imei);
			return imei;
		}

		if (name.equals("PHONE")) {
			String phone = "0";
			for (int i = 1; i < 10; i++)
				phone += Character.forDigit(r.nextInt(10), 10);
			return phone;
		}

		if (name.equals("ANDROID_ID")) {
			long v = r.nextLong();
			return Long.toHexString(v);
		}

		if (name.equals("ISO3166")) {
			String letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
			String country = Character.toString(letters.charAt(r.nextInt(letters.length())))
					+ Character.toString(letters.charAt(r.nextInt(letters.length())));
			return country;
		}

		if (name.equals("GSF_ID")) {
			long v = r.nextLong();
			return Long.toHexString(v).toUpperCase();
		}

		if (name.equals("AdvertisingId"))
			return UUID.randomUUID().toString().toUpperCase();

		if (name.equals("LAT")) {
			double d = r.nextDouble() * 180 - 90;
			d = Math.rint(d * 1e7) / 1e7;
			return Double.toString(d);
		}

		if (name.equals("LON")) {
			double d = r.nextDouble() * 360 - 180;
			d = Math.rint(d * 1e7) / 1e7;
			return Double.toString(d);
		}

		if (name.equals("SubscriberId")) {
			String subscriber = "00101";
			while (subscriber.length() < 15)
				subscriber += Character.forDigit(r.nextInt(10), 10);
			return subscriber;
		}

		if (name.equals("SSID")) {
			String ssid = "";
			while (ssid.length() < 6)
				ssid += (char) (r.nextInt(26) + 'A');

			ssid += Character.forDigit(r.nextInt(10), 10);
			ssid += Character.forDigit(r.nextInt(10), 10);
			return ssid;
		}

		return "";
	}

	private static char getLuhnDigit(String x) {
		// http://en.wikipedia.org/wiki/Luhn_algorithm
		int sum = 0;
		for (int i = 0; i < x.length(); i++) {
			int n = Character.digit(x.charAt(x.length() - 1 - i), 10);
			if (i % 2 == 0) {
				n *= 2;
				if (n > 9)
					n -= 9; // n = (n % 10) + 1;
			}
			sum += n;
		}
		return Character.forDigit((sum * 9) % 10, 10);
	}

	// Helper methods

	// TODO: Waiting for SDK 20 ...
	public static final int FIRST_ISOLATED_UID = 99000;
	public static final int LAST_ISOLATED_UID = 99999;
	public static final int FIRST_SHARED_APPLICATION_GID = 50000;
	public static final int LAST_SHARED_APPLICATION_GID = 59999;

	public static boolean isApplication(int uid) {
		uid = Util.getAppId(uid);
		return (uid >= Process.FIRST_APPLICATION_UID && uid <= Process.LAST_APPLICATION_UID);
	}

	public static boolean isShared(int uid) {
		uid = Util.getAppId(uid);
		return (uid >= FIRST_SHARED_APPLICATION_GID && uid <= LAST_SHARED_APPLICATION_GID);
	}

	public static boolean isIsolated(int uid) {
		uid = Util.getAppId(uid);
		return (uid >= FIRST_ISOLATED_UID && uid <= LAST_ISOLATED_UID);
	}

	public static boolean hasPermission(Context context, ApplicationInfoEx appInfo, String restrictionName) {
		int uid = appInfo.getUid();
		if (mPermissionRestrictionCache.get(uid) == null)
			mPermissionRestrictionCache.append(uid, new HashMap<String, Boolean>());
		if (!mPermissionRestrictionCache.get(uid).containsKey(restrictionName)) {
			boolean permission = hasPermission(context, appInfo.getPackageName(),
					PrivacyManager.getPermissions(restrictionName));
			mPermissionRestrictionCache.get(uid).put(restrictionName, permission);
		}
		return mPermissionRestrictionCache.get(uid).get(restrictionName);
	}

	public static boolean hasPermission(Context context, ApplicationInfoEx appInfo, Hook md) {
		int uid = appInfo.getUid();
		if (mPermissionHookCache.get(uid) == null)
			mPermissionHookCache.append(uid, new HashMap<Hook, Boolean>());
		if (!mPermissionHookCache.get(uid).containsKey(md)) {

			List<String> listPermission = (md.getPermissions() == null ? null : Arrays.asList(md.getPermissions()));
			boolean permission = hasPermission(context, appInfo.getPackageName(), listPermission);
			mPermissionHookCache.get(uid).put(md, permission);
		}
		return mPermissionHookCache.get(uid).get(md);
	}

	@SuppressLint("DefaultLocale")
	private static boolean hasPermission(Context context, List<String> listPackage, List<String> listPermission) {
		try {
			if (listPermission == null || listPermission.size() == 0 || listPermission.contains(""))
				return true;
			PackageManager pm = context.getPackageManager();
			for (String packageName : listPackage) {
				PackageInfo pInfo = pm.getPackageInfo(packageName, PackageManager.GET_PERMISSIONS);
				if (pInfo != null && pInfo.requestedPermissions != null)
					for (String rPermission : pInfo.requestedPermissions)
						for (String permission : listPermission)
							if (permission.equals("")) {
								// No permission required
								return true;
							} else if (rPermission.toLowerCase().contains(permission.toLowerCase())) {
								String aPermission = "android.permission." + permission;
								if (!aPermission.equals(rPermission))
									Util.log(null, Log.WARN, "Check permission=" + permission + "/" + rPermission);
								return true;
							} else if (permission.contains("."))
								if (pm.checkPermission(permission, packageName) == PackageManager.PERMISSION_GRANTED)
									return true;
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return false;
		}
		return false;
	}
}

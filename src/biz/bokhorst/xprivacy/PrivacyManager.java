package biz.bokhorst.xprivacy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.lang.reflect.Field;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import android.annotation.SuppressLint;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.location.Location;
import android.os.Build;
import android.os.Environment;
import android.os.Process;
import android.util.Log;

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
			cClipboard, cContacts, cDictionary, cEMail, cIdentification, cInternet, cLocation, cMedia, cMessages,
			cNetwork, cNfc, cPhone, cShell, cStorage, cSystem, cView };

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
	public final static String cSettingFSystem = "FSystem";
	public final static String cSettingTheme = "Theme";
	public final static String cSettingSalt = "Salt";
	public final static String cSettingVersion = "Version";
	public final static String cSettingFirstRun = "FirstRun";

	public final static boolean cExperimental = false;

	private final static String cDeface = "DEFACE";
	public final static int cCacheTimeoutMs = 15 * 1000;

	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

	private static Map<String, List<MethodDescription>> mMethod = new LinkedHashMap<String, List<MethodDescription>>();
	private static Map<String, CRestriction> mRestrictionCache = new HashMap<String, CRestriction>();
	private static Map<String, CSetting> mSettingsCache = new HashMap<String, CSetting>();
	private static Map<UsageData, UsageData> mUsageQueue = new LinkedHashMap<UsageData, UsageData>();

	static {
		// Scan meta data
		try {
			String packageName = PrivacyManager.class.getPackage().getName();
			File in = new File(Environment.getDataDirectory() + File.separator + "data" + File.separator + packageName
					+ File.separator + "meta.xml");
			Util.log(null, Log.INFO, "Reading meta=" + in.getAbsolutePath());
			FileInputStream fis = null;
			try {
				fis = new FileInputStream(in);
				XMLReader xmlReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
				MetaHandler metaHandler = new MetaHandler();
				xmlReader.setContentHandler(metaHandler);
				xmlReader.parse(new InputSource(fis));
			} finally {
				if (fis != null)
					fis.close();
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private static class MetaHandler extends DefaultHandler {
		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if (qName.equals("Meta"))
				;
			else if (qName.equals("Hook")) {
				// Get meta data
				String restrictionName = attributes.getValue("restriction");
				String methodName = attributes.getValue("method");
				String dangerous = attributes.getValue("dangerous");
				String permissions = attributes.getValue("permissions");
				int sdk = Integer.parseInt(attributes.getValue("sdk"));

				// Add meta data
				if (Build.VERSION.SDK_INT >= sdk) {
					boolean danger = (dangerous == null ? false : Boolean.parseBoolean(dangerous));
					String[] permission = (permissions == null ? null : permissions.split(","));
					MethodDescription md = new MethodDescription(methodName, danger, permission, sdk);

					if (!mMethod.containsKey(restrictionName))
						mMethod.put(restrictionName, new ArrayList<MethodDescription>());

					if (!mMethod.get(restrictionName).contains(methodName))
						mMethod.get(restrictionName).add(md);
				}
			} else
				Util.log(null, Log.WARN, "Unknown element=" + qName);
		}
	}

	// Data

	public static void registerMethod(String restrictionName, String methodName, int sdk) {
		if (Build.VERSION.SDK_INT >= sdk) {
			if (!mMethod.containsKey(restrictionName)
					|| !mMethod.get(restrictionName).contains(new MethodDescription(methodName)))
				Util.log(null, Log.WARN, "Missing method " + methodName);
		}
	}

	public static List<String> getRestrictions(boolean dangerous) {
		List<String> listRestriction = new ArrayList<String>(Arrays.asList(cRestrictionNames));
		if (!dangerous)
			for (String restrictionName : cRestrictionNames)
				if (isDangerousRestriction(restrictionName))
					listRestriction.remove(restrictionName);
		return listRestriction;
	}

	public static boolean isDangerousRestriction(String restrictionName) {
		if (restrictionName == null)
			return false;
		if (restrictionName.equals(cInternet) || restrictionName.equals(cStorage) || restrictionName.equals(cSystem))
			return true;
		return false;
	}

	public static boolean isDangerousMethod(String restrictionName, String methodName) {
		MethodDescription md = new MethodDescription(methodName);
		int pos = mMethod.get(restrictionName).indexOf(md);
		md = mMethod.get(restrictionName).get(pos);
		return md.getDangerous();
	}

	public static String getLocalizedName(Context context, String restrictionName) {
		String packageName = PrivacyManager.class.getPackage().getName();
		int stringId = context.getResources().getIdentifier("restrict_" + restrictionName, "string", packageName);
		return (stringId == 0 ? null : context.getString(stringId));
	}

	public static List<MethodDescription> getMethods(String restrictionName) {
		List<MethodDescription> listMethod = new ArrayList<MethodDescription>(mMethod.get(restrictionName));
		Collections.sort(listMethod);
		return listMethod;
	}

	public static List<String> getPermissions(String restrictionName) {
		List<String> listPermission = new ArrayList<String>();
		for (MethodDescription md : getMethods(restrictionName))
			if (md.getPermissions() != null)
				for (String permission : md.getPermissions())
					if (!listPermission.contains(permission))
						listPermission.add(permission);
		return listPermission;
	}

	// Restrictions

	@SuppressLint("DefaultLocale")
	public static boolean getRestricted(final XHook hook, final Context context, int uid, String restrictionName,
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
			if (context != null && uid != cUidAndroid)
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
						} else
							try {
								// Get restriction
								if (cursor.moveToNext()) {
									restricted = Boolean.parseBoolean(cursor.getString(cursor
											.getColumnIndex(PrivacyProvider.COL_RESTRICTED)));
									fallback = false;
								} else {
									Util.log(hook, Log.WARN, "cursor is empty");
									Util.logStack(null);
								}
							} finally {
								cursor.close();
							}

						// Send usage data async
						int qSize = 0;
						synchronized (mUsageQueue) {
							qSize = mUsageQueue.size();
						}
						if (qSize > 0) {
							mExecutor.execute(new Runnable() {
								public void run() {
									Process.setThreadPriority(Process.THREAD_PRIORITY_BACKGROUND);
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
												values.put(PrivacyProvider.COL_RESTRICTED, data.getRestricted());
												values.put(PrivacyProvider.COL_USED, data.getTimeStamp());
												if (context.getContentResolver().update(PrivacyProvider.URI_USAGE,
														values, null, null) <= 0)
													Util.log(hook, Log.INFO, "Error updating usage data=" + data);
											} catch (Throwable ex) {
												Util.bug(hook, ex);
											}
										}
									} while (data != null);
								}
							});
						}
					}
				} catch (Throwable ex) {
					Util.bug(hook, ex);
				}

			// Use fallback
			if (fallback) {
				// Fallback
				restricted = PrivacyProvider.getRestrictedFallback(hook, uid, restrictionName, methodName);

				// Queue usage data
				if (usage && uid != cUidAndroid) {
					UsageData usageData = new UsageData(uid, restrictionName, methodName, restricted);
					synchronized (mUsageQueue) {
						if (mUsageQueue.containsKey(usageData))
							mUsageQueue.remove(usageData);
						mUsageQueue.put(usageData, usageData);
						Util.log(hook, Log.INFO, "Queue usage data=" + usageData + " size=" + mUsageQueue.size());
					}
				}
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

		// Set default exceptions for methods
		if (restricted && methodName == null)
			for (MethodDescription md : getMethods(restrictionName))
				if (isDangerousMethod(restrictionName, md.getMethodName()))
					PrivacyManager.setRestricted(null, context, uid, restrictionName, md.getMethodName(), false);
	}

	public static List<Boolean> getRestricted(Context context, int uid, boolean dangerous) {
		List<Boolean> listRestricted = new ArrayList<Boolean>();
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver != null) {
			Cursor cursor = contentResolver.query(PrivacyProvider.URI_RESTRICTION, null, null,
					new String[] { Integer.toString(uid), Boolean.toString(false), null }, null);
			if (cursor != null)
				try {
					while (cursor.moveToNext()) {
						if (!dangerous) {
							String restrictionName = cursor.getString(cursor
									.getColumnIndex(PrivacyProvider.COL_RESTRICTION));
							if (PrivacyManager.isDangerousRestriction(restrictionName))
								continue;
						}
						listRestricted.add(Boolean.parseBoolean(cursor.getString(cursor
								.getColumnIndex(PrivacyProvider.COL_RESTRICTED))));
					}
				} finally {
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
		if (rCursor != null)
			try {
				while (rCursor.moveToNext()) {
					RestrictionDesc restriction = new RestrictionDesc();
					restriction.uid = rCursor.getInt(rCursor.getColumnIndex(PrivacyProvider.COL_UID));
					restriction.restricted = Boolean.parseBoolean(rCursor.getString(rCursor
							.getColumnIndex(PrivacyProvider.COL_RESTRICTED)));
					restriction.restrictionName = rCursor.getString(rCursor
							.getColumnIndex(PrivacyProvider.COL_RESTRICTION));
					restriction.methodName = rCursor.getString(rCursor.getColumnIndex(PrivacyProvider.COL_METHOD));
					result.add(restriction);
				}
			} finally {
				rCursor.close();
			}
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
		if (cursor != null)
			try {
				while (cursor.moveToNext()) {
					long usage = cursor.getLong(cursor.getColumnIndex(PrivacyProvider.COL_USED));
					if (usage > lastUsage)
						lastUsage = usage;
				}
			} finally {
				cursor.close();
			}
		boolean used = (lastUsage != 0);
		logRestriction(null, context, uid, "used", restrictionName, methodName, used, false, 0);
		return lastUsage;
	}

	public static List<UsageData> getUsed(Context context, int uid) {
		List<UsageData> listUsage = new ArrayList<UsageData>();
		ContentResolver cr = context.getContentResolver();
		Cursor cursor = cr.query(PrivacyProvider.URI_USAGE, null, null, new String[] { Integer.toString(uid), null },
				null);
		if (cursor != null)
			try {
				while (cursor.moveToNext()) {
					int rUid = cursor.getInt(cursor.getColumnIndex(PrivacyProvider.COL_UID));
					String restrictionName = cursor.getString(cursor.getColumnIndex(PrivacyProvider.COL_RESTRICTION));
					String methodName = cursor.getString(cursor.getColumnIndex(PrivacyProvider.COL_METHOD));
					boolean restricted = Boolean.parseBoolean(cursor.getString(cursor
							.getColumnIndex(PrivacyProvider.COL_RESTRICTED)));
					long used = cursor.getLong(cursor.getColumnIndex(PrivacyProvider.COL_USED));
					UsageData usageData = new UsageData(rUid, restrictionName, methodName, restricted, used);
					listUsage.add(usageData);
				}
			} finally {
				cursor.close();
			}
		Collections.sort(listUsage);
		return listUsage;
	}

	// Settings

	public static boolean getSettingBool(XHook hook, Context context, String settingName, boolean defaultValue,
			boolean useCache) {
		return Boolean.parseBoolean(getSetting(hook, context, settingName, Boolean.toString(defaultValue).toString(),
				useCache));
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
					} else
						try {
							if (cursor.moveToNext()) {
								value = cursor.getString(cursor.getColumnIndex(PrivacyProvider.COL_VALUE));
								fallback = false;
							} else {
								Util.log(hook, Log.WARN, "cursor is empty");
							}

						} finally {
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
		if (sCursor != null)
			try {
				while (sCursor.moveToNext()) {
					// Get setting
					String setting = sCursor.getString(sCursor.getColumnIndex(PrivacyProvider.COL_SETTING));
					String value = sCursor.getString(sCursor.getColumnIndex(PrivacyProvider.COL_VALUE));
					result.put(setting, value);
				}
			} finally {
				sCursor.close();
			}
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
		if (name.equals("getGroupIdLevel1"))
			return null;
		if (name.equals("getIsimDomain"))
			return null;
		if (name.equals("getIsimImpi"))
			return null;
		if (name.equals("getIsimImpu"))
			return null;

		if (name.equals("getNetworkCountryIso")) // ISO country code
			return getSetting(null, null, cSettingCountry, "XX", true);
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
		Util.log(null, Log.WARN, "Fallback value name=" + name);
		return cDeface;
	}

	public static Location getDefacedLocation(Location location) {
		String sLat = getSetting(null, null, cSettingLatitude, "", true);
		String sLon = getSetting(null, null, cSettingLongitude, "", true);
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
		PackageManager pm = context.getPackageManager();
		return (pm.checkPermission("android.permission.INTERNET", packageName) == PackageManager.PERMISSION_GRANTED);
	}

	public static boolean hasPermission(Context context, String packageName, String restrictionName) {
		return hasPermission(context, packageName, getPermissions(restrictionName));
	}

	public static boolean hasPermission(Context context, String packageName, MethodDescription md) {
		List<String> listPermission = (md.getPermissions() == null ? null : Arrays.asList(md.getPermissions()));
		return hasPermission(context, packageName, listPermission);
	}

	@SuppressLint("DefaultLocale")
	private static boolean hasPermission(Context context, String packageName, List<String> listPermission) {
		try {
			if (listPermission == null || listPermission.size() == 0 || listPermission.contains(""))
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

	public static class MethodDescription implements Comparable<MethodDescription> {
		private String mMethodName;
		private boolean mDangerous;
		private String[] mPermissions;
		private int mSdk;

		public MethodDescription(String methodName) {
			mMethodName = methodName;
		}

		public MethodDescription(String methodName, boolean dangerous, String[] permissions, int sdk) {
			mMethodName = methodName;
			mDangerous = dangerous;
			mPermissions = permissions;
			mSdk = sdk;
		}

		public String getMethodName() {
			return mMethodName;
		}

		public boolean getDangerous() {
			return mDangerous;
		}

		public String[] getPermissions() {
			return mPermissions;
		}

		public int getSdk() {
			return mSdk;
		}

		@Override
		public int hashCode() {
			return mMethodName.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			MethodDescription other = (MethodDescription) obj;
			return mMethodName.equals(other.mMethodName);
		}

		@Override
		public int compareTo(MethodDescription another) {
			return mMethodName.compareTo(another.mMethodName);
		}
	}

	public static class UsageData implements Comparable<UsageData> {
		private Integer mUid;
		private String mRestriction;
		private String mMethodName;
		private boolean mRestricted;
		private long mTimeStamp;
		private int mHash;

		public UsageData(int uid, String restrictionName, String methodName, boolean restricted) {
			initialize(uid, restrictionName, methodName, restricted, new Date().getTime());
		}

		public UsageData(int uid, String restrictionName, String methodName, boolean restricted, long used) {
			initialize(uid, restrictionName, methodName, restricted, used);
		}

		private void initialize(int uid, String restrictionName, String methodName, boolean restricted, long used) {
			mUid = uid;
			mRestriction = restrictionName;
			mMethodName = methodName;
			mRestricted = restricted;
			mTimeStamp = used;

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

		public boolean getRestricted() {
			return mRestricted;
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
			return String.format("%d/%s/%s=%b", mUid, mRestriction, mMethodName, mRestricted);
		}

		@Override
		public int compareTo(UsageData another) {
			if (mTimeStamp < another.mTimeStamp)
				return 1;
			else if (mTimeStamp > another.mTimeStamp)
				return -1;
			else
				return 0;
		}
	}
}

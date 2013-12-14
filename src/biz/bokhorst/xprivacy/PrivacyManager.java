package biz.bokhorst.xprivacy;

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
import java.util.Random;
import java.util.UUID;
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
import android.os.Binder;
import android.os.Build;
import android.os.Environment;
import android.os.Process;
import android.os.SystemClock;
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
	public static final String cNotifications = "notifications";
	public static final String cOverlay = "overlay";
	public static final String cPhone = "phone";
	public static final String cSensors = "sensors";
	public static final String cStorage = "storage";
	public static final String cShell = "shell";
	public static final String cSystem = "system";
	public static final String cView = "view";

	private static final String cRestrictionNames[] = new String[] { cAccounts, cBrowser, cCalendar, cCalling,
			cClipboard, cContacts, cDictionary, cEMail, cIdentification, cInternet, cLocation, cMedia, cMessages,
			cNetwork, cNfc, cNotifications, cSensors, cOverlay, cPhone, cShell, cStorage, cSystem, cView };

	public final static int cXposedAppProcessMinVersion = 46;
	public final static int cAndroidUid = Process.SYSTEM_UID;

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
	public final static String cSettingDangerous = "Dangerous";
	public final static String cSettingAndroidUsage = "AndroidUsage";
	public final static String cSettingLog = "Log";
	public final static String cSettingRandom = "Random@boot";
	public final static String cSettingState = "State";
	public final static String cSettingConfidence = "Confidence";
	public final static String cSettingTemplate = "Template";

	public final static String cValueRandom = "#Random#";
	public final static String cValueRandomLegacy = "\nRandom\n";

	private final static String cDeface = "DEFACE";
	public final static int cRestrictionCacheTimeoutMs = 15 * 1000;
	public final static int cSettingCacheTimeoutMs = 30 * 1000;
	public final static int cUseProviderAfterMs = 3 * 60 * 1000;

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
				String restart = attributes.getValue("restart");
				String permissions = attributes.getValue("permissions");
				int sdk = Integer.parseInt(attributes.getValue("sdk"));

				// Add meta data
				if (Build.VERSION.SDK_INT >= sdk) {
					boolean danger = (dangerous == null ? false : Boolean.parseBoolean(dangerous));
					boolean restartRequired = (restart == null ? false : Boolean.parseBoolean(restart));
					String[] permission = (permissions == null ? null : permissions.split(","));
					MethodDescription md = new MethodDescription(methodName, danger, restartRequired, permission, sdk);

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
		if (restrictionName != null && methodName != null && Build.VERSION.SDK_INT >= sdk) {
			if (!mMethod.containsKey(restrictionName)
					|| !mMethod.get(restrictionName).contains(new MethodDescription(methodName)))
				Util.log(null, Log.WARN, "Missing method " + methodName);
		}
	}

	public static List<String> getRestrictions() {
		return new ArrayList<String>(Arrays.asList(cRestrictionNames));
	}

	public static MethodDescription getMethod(String restrictionName, String methodName) {
		MethodDescription md = new MethodDescription(methodName);
		int pos = mMethod.get(restrictionName).indexOf(md);
		return (pos < 0 ? null : mMethod.get(restrictionName).get(pos));
	}

	public static String getLocalizedName(Context context, String restrictionName) {
		String packageName = PrivacyManager.class.getPackage().getName();
		int stringId = context.getResources().getIdentifier("restrict_" + restrictionName, "string", packageName);
		return (stringId == 0 ? null : context.getString(stringId));
	}

	public static List<MethodDescription> getMethods(String restrictionName) {
		List<MethodDescription> listMethod = new ArrayList<MethodDescription>();
		List<MethodDescription> listMethodOrig = mMethod.get(restrictionName);
		if (listMethodOrig != null)
			listMethod.addAll(listMethodOrig);
		// null can happen when upgrading
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
	public static boolean getRestricted(final XHook hook, Context context, int uid, String restrictionName,
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
				Util.log(hook, Log.WARN, "restriction empty method=" + methodName);
				Util.logStack(hook);
				return false;
			}

			if (usage)
				if (methodName == null || methodName.equals("")) {
					Util.log(hook, Log.WARN, "method empty");
					Util.logStack(hook);
				} else if (getMethods(restrictionName).indexOf(new MethodDescription(methodName)) < 0)
					Util.log(hook, Log.WARN, "unknown method=" + methodName);

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

			// Check if usage data enabled
			if (!isUsageDataEnabled(uid))
				context = null;

			// Check if restricted
			boolean fallback = true;
			boolean restricted = false;
			if (context != null)
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
						sendUsageData(hook, context);
					}
				} catch (SecurityException ex) {
					Util.bug(hook, ex);
				} catch (Throwable ex) {
					Util.bug(hook, ex);
				}

			// Use fallback
			if (fallback) {
				// Fallback
				restricted = PrivacyProvider.getRestrictedFallback(hook, uid, restrictionName, methodName);

				// Queue usage data
				if (usage) {
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

	public static boolean isUsageDataEnabled(int uid) {
		if (SystemClock.elapsedRealtime() < cUseProviderAfterMs)
			return false;

		if (uid == cAndroidUid)
			return PrivacyManager.getSettingBool(null, null, 0, PrivacyManager.cSettingAndroidUsage, false, false);
		else
			return !isIsolated(uid);
	}

	// Waiting for SDK 20 ...
	public static final int FIRST_ISOLATED_UID = 99000;
	public static final int LAST_ISOLATED_UID = 99999;
	public static final int FIRST_SHARED_APPLICATION_GID = 50000;
	public static final int LAST_SHARED_APPLICATION_GID = 59999;

	public static boolean isApplication(int uid) {
		return (uid >= Process.FIRST_APPLICATION_UID && uid <= Process.LAST_APPLICATION_UID);
	}

	public static boolean isShared(int uid) {
		return (uid >= FIRST_SHARED_APPLICATION_GID && uid <= LAST_SHARED_APPLICATION_GID);
	}

	public static boolean isIsolated(int uid) {
		return (uid >= FIRST_ISOLATED_UID && uid <= LAST_ISOLATED_UID);
	}

	public static void sendUsageData(final XHook hook, Context context) {
		int qSize = 0;
		synchronized (mUsageQueue) {
			qSize = mUsageQueue.size();
		}
		if (qSize > 0) {
			final Context fContext = context;
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
								Util.log(hook, Log.INFO, "Sending usage data=" + data + " size=" + size + " uid="
										+ Binder.getCallingUid());
								ContentValues values = new ContentValues();
								values.put(PrivacyProvider.COL_UID, data.getUid());
								values.put(PrivacyProvider.COL_RESTRICTION, data.getRestrictionName());
								values.put(PrivacyProvider.COL_METHOD, data.getMethodName());
								values.put(PrivacyProvider.COL_RESTRICTED, data.getRestricted());
								values.put(PrivacyProvider.COL_USED, data.getTimeStamp());
								if (fContext.getContentResolver().update(PrivacyProvider.URI_USAGE, values, null, null) <= 0)
									Util.log(hook, Log.INFO, "Error updating usage data=" + data);
								Thread.sleep(500);
							} catch (Throwable ex) {
								Util.bug(hook, ex);
							}
						}
					} while (data != null);
				}
			});
		} else
			Util.log(hook, Log.INFO, "No usage data queued uid=" + Binder.getCallingUid());
	}

	public static boolean setRestricted(XHook hook, Context context, int uid, String restrictionName,
			String methodName, boolean restricted) {
		// Check context
		if (context == null) {
			Util.log(hook, Log.WARN, "context is null");
			return false;
		}

		// Check uid
		if (uid == 0) {
			Util.log(hook, Log.WARN, "uid=0");
			return false;
		}

		// Get content resolver
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver == null) {
			Util.log(hook, Log.WARN, "contentResolver is null");
			return false;
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

		// Mark as restricted
		if (restricted)
			PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingState,
					Integer.toString(ActivityMain.STATE_RESTRICTED));

		boolean dangerous = PrivacyManager.getSettingBool(null, context, 0, PrivacyManager.cSettingDangerous, false,
				false);
		if (methodName == null) {
			// Make exceptions for dangerous methods
			if (restricted && !dangerous) {
				for (MethodDescription md : getMethods(restrictionName))
					if (md.isDangerous())
						PrivacyManager.setRestricted(null, context, uid, restrictionName, md.getName(), dangerous);
			}

			// Check restart
			for (MethodDescription md : getMethods(restrictionName))
				if (md.isRestartRequired() && !(restricted && !dangerous && md.isDangerous()))
					return true;
			return false;
		} else {
			// Check restart
			return getMethod(restrictionName, methodName).isDangerous();
		}
	}

	public static List<Boolean> getRestricted(Context context, int uid, String restrictionName) {
		List<Boolean> listRestricted = new ArrayList<Boolean>();
		ContentResolver contentResolver = context.getContentResolver();
		if (contentResolver != null) {
			Cursor cursor = contentResolver.query(PrivacyProvider.URI_RESTRICTION, null, restrictionName, new String[] {
					Integer.toString(uid), Boolean.toString(false), restrictionName == null ? null : "*" }, null);
			if (cursor != null)
				try {
					while (cursor.moveToNext()) {
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

	public static List<RestrictionDesc> getRestricted(Context context, Runnable progress) {
		List<RestrictionDesc> result = new ArrayList<RestrictionDesc>();
		progress.run(); // 1% for getting the cursor
		Cursor rCursor = context.getContentResolver().query(PrivacyProvider.URI_RESTRICTION, null, null,
				new String[] { Integer.toString(0), Boolean.toString(false) }, null);
		if (rCursor != null)
			try {
				final int max = rCursor.getCount();
				final int step = (max + 95) / 96;
				// 96% left for loading the restrictions

				int current = 0;
				while (rCursor.moveToNext()) {
					current++;
					if (current % step == 0 || current == max)
						progress.run();
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

	public static boolean deleteRestrictions(Context context, int uid) {
		// Check if restart required
		boolean restart = false;
		for (String restrictionName : getRestrictions()) {
			for (MethodDescription md : getMethods(restrictionName))
				if (getRestricted(null, context, uid, restrictionName, md.getName(), false, false))
					if (md.isRestartRequired()) {
						restart = true;
						break;
					}
			if (restart)
				break;
		}

		// Delete restrictions
		context.getContentResolver().delete(PrivacyProvider.URI_RESTRICTION, null,
				new String[] { Integer.toString(uid) });
		Util.log(null, Log.INFO, "Deleted restrictions uid=" + uid);

		// Mark as new/changed
		PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingState,
				Integer.toString(ActivityMain.STATE_ATTENTION));

		return restart;
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

	public static void deleteUsage(Context context, int uid) {
		context.getContentResolver().delete(PrivacyProvider.URI_USAGE, null, new String[] { Integer.toString(uid) });
		Util.log(null, Log.INFO, "Deleted usage data uid=" + uid);
	}

	// Settings

	public static boolean getSettingBool(XHook hook, Context context, int uid, String settingName,
			boolean defaultValue, boolean useCache) {
		return Boolean.parseBoolean(getSetting(hook, context, uid, settingName, Boolean.toString(defaultValue),
				useCache));
	}

	public static String getSetting(XHook hook, Context context, int uid, String settingName, String defaultValue,
			boolean useCache) {
		if (uid == 0)
			return getSetting(hook, context, settingName, defaultValue, useCache);
		else {
			String setting = getSetting(hook, context, String.format("%s.%d", settingName, uid), null, useCache);
			if (setting == null)
				return getSetting(hook, context, settingName, defaultValue, useCache);
			else
				return setting;
		}
	}

	public static String getAppSetting(XHook hook, Context context, int uid, String settingName, String defaultValue,
			boolean useCache) {
		return getSetting(hook, context, String.format("%s.%d", settingName, uid), null, useCache);
	}

	private static String getSetting(XHook hook, Context context, String name, String defaultValue, boolean useCache) {
		long start = System.currentTimeMillis();

		// Check cache
		if (useCache)
			synchronized (mSettingsCache) {
				if (mSettingsCache.containsKey(name)) {
					CSetting entry = mSettingsCache.get(name);
					if (entry.isExpired())
						mSettingsCache.remove(name);
					else {
						String value = mSettingsCache.get(name).getSettingsValue();
						Util.log(hook, Log.INFO, String.format("get setting %s=%s (cached)", name, value));
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
					Cursor cursor = contentResolver.query(PrivacyProvider.URI_SETTING, null, name, null, null);
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
			value = PrivacyProvider.getSettingFallback(name, defaultValue);

		// Default value
		if (value == null)
			value = defaultValue;
		else if (value.equals("") && defaultValue != null)
			value = defaultValue;

		// Add to cache
		synchronized (mSettingsCache) {
			if (mSettingsCache.containsKey(name))
				mSettingsCache.remove(name);
			mSettingsCache.put(name, new CSetting(name, value));
		}

		long ms = System.currentTimeMillis() - start;
		Util.log(
				hook,
				Log.INFO,
				String.format("get setting %s=%s%s%s", name, value, (fallback ? " (file)" : ""), (ms > 1 ? " " + ms
						+ " ms" : "")));
		return value;
	}

	@SuppressLint("DefaultLocale")
	public static void setSetting(XHook hook, Context context, int uid, String settingName, String value) {
		ContentResolver contentResolver = context.getContentResolver();
		ContentValues values = new ContentValues();
		values.put(PrivacyProvider.COL_VALUE, value);
		String sName = (uid == 0 ? settingName : String.format("%s.%d", settingName, uid));
		if (contentResolver.update(PrivacyProvider.URI_SETTING, values, sName, null) <= 0)
			Util.log(hook, Log.INFO, "Error updating setting=" + sName);
		Util.log(hook, Log.INFO, String.format("set setting %s=%s", sName, value));
	}

	public static Map<String, String> getSettings(Context context, Runnable progress) {
		Map<String, String> result = new HashMap<String, String>();
		progress.run(); // 1% for getting the cursor
		Cursor sCursor = context.getContentResolver().query(PrivacyProvider.URI_SETTING, null, null, null, null);
		if (sCursor != null)
			try {
				final int max = sCursor.getCount();
				int current = 0;
				while (sCursor.moveToNext()) {
					current++;
					if (current == max / 2 || current == max)
						progress.run(); // 2% for fetching settings
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

	public static void deleteSettings(Context context, int uid) {
		context.getContentResolver().delete(PrivacyProvider.URI_SETTING, null, new String[] { Integer.toString(uid) });
		Util.log(null, Log.INFO, "Deleted settings uid=" + uid);
	}

	// Defacing

	public static Object getDefacedProp(int uid, String name) {
		// Serial number
		if (name.equals("SERIAL") || name.equals("%serialno")) {
			String value = getSetting(null, null, uid, cSettingSerial, cDeface, true);
			return (cValueRandom.equals(value) ? getRandomProp("SERIAL") : value);
		}

		// Host name
		if (name.equals("%hostname"))
			return cDeface;

		// MAC addresses
		if (name.equals("MAC") || name.equals("%macaddr")) {
			String mac = getSetting(null, null, uid, cSettingMac, "DE:FA:CE:DE:FA:CE", true);
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
			String value = getSetting(null, null, uid, cSettingImei, "000000000000000", true);
			return (cValueRandom.equals(value) ? getRandomProp("IMEI") : value);
		}

		// Phone
		if (name.equals("PhoneNumber") || name.equals("getLine1AlphaTag") || name.equals("getLine1Number")
				|| name.equals("getMsisdn") || name.equals("getVoiceMailAlphaTag") || name.equals("getVoiceMailNumber")) {
			String value = getSetting(null, null, uid, cSettingPhone, cDeface, true);
			return (cValueRandom.equals(value) ? getRandomProp("PHONE") : value);
		}

		// Android ID
		if (name.equals("ANDROID_ID")) {
			String value = getSetting(null, null, uid, cSettingId, cDeface, true);
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

		if (name.equals("getNetworkCountryIso")) {
			// ISO country code
			String value = getSetting(null, null, uid, cSettingCountry, "XX", true);
			return (cValueRandom.equals(value) ? getRandomProp("ISO3166") : value);
		}
		if (name.equals("getNetworkOperator")) // MCC+MNC: test network
			return getSetting(null, null, uid, cSettingMcc, "001", true)
					+ getSetting(null, null, uid, cSettingMnc, "01", true);
		if (name.equals("getNetworkOperatorName"))
			return getSetting(null, null, uid, cSettingOperator, cDeface, true);

		if (name.equals("getSimCountryIso")) {
			// ISO country code
			String value = getSetting(null, null, uid, cSettingCountry, "XX", true);
			return (cValueRandom.equals(value) ? getRandomProp("ISO3166") : value);
		}
		if (name.equals("getSimOperator")) // MCC+MNC: test network
			return getSetting(null, null, uid, cSettingMcc, "001", true)
					+ getSetting(null, null, uid, cSettingMnc, "01", true);
		if (name.equals("getSimOperatorName"))
			return getSetting(null, null, uid, cSettingOperator, cDeface, true);
		if (name.equals("getSimSerialNumber"))
			return getSetting(null, null, uid, cSettingIccId, null, true);

		if (name.equals("getSubscriberId")) { // IMSI for a GSM phone
			String value = getSetting(null, null, uid, cSettingSubscriber, null, true);
			return (cValueRandom.equals(value) ? getRandomProp("SubscriberId") : value);
		}

		if (name.equals("SSID")) {
			// Default hidden network
			String value = getSetting(null, null, uid, cSettingSSID, "", true);
			return (cValueRandom.equals(value) ? getRandomProp("SSID") : value);
		}

		// Google services framework ID
		if (name.equals("GSF_ID")) {
			long gsfid = 0xDEFACE;
			try {
				String value = getSetting(null, null, uid, cSettingGsfId, "DEFACE", true);
				if (cValueRandom.equals(value))
					value = getRandomProp(name);
				gsfid = Long.parseLong(value, 16);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
			return gsfid;
		}

		// Advertisement ID
		if (name.equals("AdvertisingId")) {
			String adid = getSetting(null, null, uid, cSettingAdId, "DEFACE00-0000-0000-0000-000000000000", true);
			if (cValueRandom.equals(adid))
				adid = getRandomProp(name);
			return adid;
		}

		if (name.equals("InetAddress")) {
			// Set address
			String ip = getSetting(null, null, uid, cSettingIP, null, true);
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
			String ip = getSetting(null, null, uid, cSettingIP, null, true);
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
			return getSetting(null, null, uid, cSettingUa,
					"Mozilla/5.0 (Linux; U; Android; en-us) AppleWebKit/999+ (KHTML, like Gecko) Safari/999.9", true);

		// InputDevice
		if (name.equals("DeviceDescriptor"))
			return cDeface;

		// Fallback
		Util.log(null, Log.WARN, "Fallback value name=" + name);
		return cDeface;
	}

	public static Location getDefacedLocation(int uid, Location location) {
		String sLat = getSetting(null, null, uid, cSettingLatitude, null, true);
		String sLon = getSetting(null, null, uid, cSettingLongitude, null, true);

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
			return Long.toHexString(v).toUpperCase();
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
			String subscriber = "001";
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

	// @formatter:off
	private static void logRestriction(
			XHook hook, Context context,
			int uid, String prefix, String restrictionName, String methodName,
			boolean restricted, boolean cached, long ms) {
		Util.log(hook, Log.INFO, String.format("%s %d/%s %s=%srestricted%s%s",
				prefix, uid, methodName, restrictionName,
				(restricted ? "" : "!"),
				(cached ? " (cached)" : (context == null ? " (file)" : "")),
				(ms > 1 ? " " + ms + " ms" : "")));
	}
	// @formatter:on

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
			return (mTimestamp + cRestrictionCacheTimeoutMs < new Date().getTime());
		}

		public boolean isRestricted() {
			return mRestricted;
		}
	}

	private static class CSetting {
		private long mTimestamp;
		private String mName;
		private String mValue;

		public CSetting(String name, String value) {
			mTimestamp = new Date().getTime();
			mName = name;
			mValue = value;
		}

		public boolean isExpired() {
			if (mName.equals(PrivacyManager.cSettingVersion))
				return false;
			if (mName.equals(PrivacyManager.cSettingAndroidUsage))
				return false;
			return (mTimestamp + cSettingCacheTimeoutMs < new Date().getTime());
		}

		public String getSettingsValue() {
			return mValue;
		}
	}

	public static class MethodDescription implements Comparable<MethodDescription> {
		private String mMethodName;
		private boolean mDangerous;
		private boolean mRestart;
		private String[] mPermissions;
		private int mSdk;

		public MethodDescription(String methodName) {
			mMethodName = methodName;
		}

		public MethodDescription(String methodName, boolean dangerous, boolean restart, String[] permissions, int sdk) {
			mMethodName = methodName;
			mDangerous = dangerous;
			mRestart = restart;
			mPermissions = permissions;
			mSdk = sdk;
		}

		public String getName() {
			return mMethodName;
		}

		public boolean isDangerous() {
			return mDangerous;
		}

		public boolean isRestartRequired() {
			return mRestart;
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

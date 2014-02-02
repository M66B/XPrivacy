package biz.bokhorst.xprivacy;

import java.io.File;
import java.lang.reflect.Method;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.content.ContentValues;
import android.content.Context;
import android.content.DialogInterface;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.Resources;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteDoneException;
import android.database.sqlite.SQLiteStatement;
import android.os.Binder;
import android.os.Build;
import android.os.Environment;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.Process;
import android.os.RemoteException;
import android.os.StrictMode;
import android.os.StrictMode.ThreadPolicy;
import android.text.TextUtils;
import android.util.Log;
import android.view.WindowManager;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import de.robv.android.xposed.XposedBridge;

public class PrivacyService {
	private static int mXUid = -1;
	private static String mSecret = null;
	private static List<String> mListError;
	private static IPrivacyService mClient = null;
	private static SQLiteDatabase mDatabase = null;
	private static Thread mWorker = null;
	private static Handler mHandler = null;
	private static boolean mOnDemanding = false;

	private static SQLiteStatement stmtGetRestriction = null;
	private static SQLiteStatement stmtGetSetting = null;
	private static SQLiteStatement stmtGetUsageRestriction = null;
	private static SQLiteStatement stmtGetUsageMethod = null;

	private static int cCurrentVersion = 262;
	private static String cServiceName = "xprivacy" + cCurrentVersion;
	private static String cTableRestriction = "restriction";
	private static String cTableUsage = "usage";
	private static String cTableSetting = "setting";

	private static boolean mUseCache = false;
	private static Map<CSetting, CSetting> mSettingCache = new HashMap<CSetting, CSetting>();
	private static Map<CRestriction, CRestriction> mRestrictionCache = new HashMap<CRestriction, CRestriction>();

	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

	// TODO: define column names
	// sqlite3 /data/data/biz.bokhorst.xprivacy/xprivacy.db

	public static void register(List<String> listError, String secret) {
		// Store secret and errors
		mSecret = secret;
		mListError = listError;

		try {
			// Get memory class to enable/disable caching
			// http://stackoverflow.com/questions/2630158/detect-application-heap-size-in-android
			int memoryClass = (int) (Runtime.getRuntime().maxMemory() / 1024L / 1024L);
			mUseCache = (memoryClass >= 32);
			Util.log(null, Log.WARN, "Memory class=" + memoryClass + " cache=" + mUseCache);

			// Start a worker thread
			mWorker = new Thread(new Runnable() {
				@Override
				public void run() {
					try {
						Looper.prepare();
						mHandler = new Handler();
						Looper.loop();
					} catch (Throwable ex) {
						Util.bug(null, ex);
						mListError.add(ex.toString());
						mListError.add(Log.getStackTraceString(ex));
					}
				}
			});
			mWorker.start();

			// Catch ANR's
			try {
				final Class<?> cam = Class.forName("com.android.server.am.ActivityManagerService");

				for (Method anr : cam.getDeclaredMethods()) {
					// Foreground
					// @formatter:off
					// public long inputDispatchingTimedOut(int pid, final boolean aboveSystem, String reason)
					// public boolean inputDispatchingTimedOut(final ProcessRecord proc, final ActivityRecord activity, final ActivityRecord parent, final boolean aboveSystem, String reason)
					// @formatter:on
					if (anr.getName().equals("inputDispatchingTimedOut")) {
						Util.log(null, Log.WARN, "Hooking " + anr);
						XposedBridge.hookMethod(anr, new XC_MethodHook() {
							@Override
							protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
								try {
									// Delay ANR while on demand dialog open
									Util.log(null, Log.WARN, "Foreground ANR uid=" + getUidANR(param) + " ondemand="
											+ mOnDemanding);
									if (mOnDemanding)
										param.setResult(5 * 1000);
								} catch (Throwable ex) {
									Util.bug(null, ex);
									mListError.add(ex.toString());
									mListError.add(Log.getStackTraceString(ex));
								}
							}
						});
					}

					// Background
					// @formatter:off
					// final void appNotResponding(ProcessRecord app, ActivityRecord activity, ActivityRecord parent, boolean aboveSystem, final String annotation)
					// @formatter:on
					if (anr.getName().equals("appNotResponding")) {
						Util.log(null, Log.WARN, "Hooking " + anr);
						XposedBridge.hookMethod(anr, new XC_MethodHook() {
							@Override
							protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
								try {
									// Ignore ANR while on demand dialog open
									Util.log(null, Log.WARN, "Background ANR uid=" + getUidANR(param) + " ondemand="
											+ mOnDemanding);
									if (mOnDemanding)
										param.setResult(null);
								} catch (Throwable ex) {
									Util.bug(null, ex);
									mListError.add(ex.toString());
									mListError.add(Log.getStackTraceString(ex));
								}
							}
						});
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				mListError.add(ex.toString());
				mListError.add(Log.getStackTraceString(ex));
			}

			// Register privacy service
			// @formatter:off
			// public static void addService(String name, IBinder service)
			// public static void addService(String name, IBinder service, boolean allowIsolated)
			// @formatter:on
			Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
				Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class,
						boolean.class);
				Util.log(null, Log.WARN, "Invoking " + mAddService);
				mAddService.invoke(null, cServiceName, mPrivacyService, true);
			} else {
				Method mAddService = cServiceManager.getDeclaredMethod("addService", String.class, IBinder.class);
				Util.log(null, Log.WARN, "Invoking " + mAddService);
				mAddService.invoke(null, cServiceName, mPrivacyService);
			}
			Util.log(null, Log.WARN, "Service registered name=" + cServiceName);
		} catch (Throwable ex) {
			Util.bug(null, ex);
			mListError.add(ex.toString());
			mListError.add(Log.getStackTraceString(ex));
		}
	}

	private static int getUidANR(MethodHookParam param) throws IllegalAccessException {
		int uid = -1;
		try {
			Class<?> pr = Class.forName("com.android.server.am.ProcessRecord");
			if (param.args.length > 0 && param.args[0].getClass().equals(pr)) {
				uid = (Integer) pr.getDeclaredField("uid").get(param.args[0]);
			}
		} catch (ClassNotFoundException ignored) {
		} catch (NoSuchFieldException ignored) {
		}
		return uid;
	}

	public static boolean checkClient() {
		try {
			IPrivacyService client = getClient();
			if (client != null)
				return (client.getVersion() == cCurrentVersion);
		} catch (RemoteException ex) {
			Util.bug(null, ex);
		}
		return false;
	}

	public static IPrivacyService getClient() {
		if (mClient == null)
			try {
				// public static IBinder getService(String name)
				Class<?> cServiceManager = Class.forName("android.os.ServiceManager");
				Method mGetService = cServiceManager.getDeclaredMethod("getService", String.class);
				mClient = IPrivacyService.Stub.asInterface((IBinder) mGetService.invoke(null, cServiceName));
			} catch (Throwable ex) {
				mClient = null;
				Util.bug(null, ex);
			}

		// Disable disk strict mode
		try {
			ThreadPolicy oldPolicy = StrictMode.getThreadPolicy();
			ThreadPolicy newpolicy = new ThreadPolicy.Builder(oldPolicy).permitDiskReads().permitDiskWrites().build();
			StrictMode.setThreadPolicy(newpolicy);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		return mClient;
	}

	private static final IPrivacyService.Stub mPrivacyService = new IPrivacyService.Stub() {

		// Management

		@Override
		public int getVersion() throws RemoteException {
			return cCurrentVersion;
		}

		@Override
		public List<String> check() throws RemoteException {
			enforcePermission();
			File dbFile = getDbFile();
			if (!dbFile.exists() || !dbFile.canRead() || !dbFile.canWrite())
				throw new InvalidParameterException("Database does not exist or is not accessible");
			synchronized (mListError) {
				return mListError;
			}
		}

		// Restrictions

		@Override
		public void setRestriction(ParcelableRestriction restriction) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();
				// 0 not restricted, ask
				// 1 restricted, ask
				// 2 not restricted, asked
				// 3 restricted, asked

				db.beginTransaction();
				try {
					// Create category record
					if (restriction.methodName == null) {
						ContentValues cvalues = new ContentValues();
						cvalues.put("uid", restriction.uid);
						cvalues.put("restriction", restriction.restrictionName);
						cvalues.put("method", "");
						cvalues.put("restricted", (restriction.restricted ? 1 : 0) + (restriction.asked ? 2 : 0));
						db.insertWithOnConflict(cTableRestriction, null, cvalues, SQLiteDatabase.CONFLICT_REPLACE);
					}

					// Create method exception record
					if (restriction.methodName != null) {
						ContentValues mvalues = new ContentValues();
						mvalues.put("uid", restriction.uid);
						mvalues.put("restriction", restriction.restrictionName);
						mvalues.put("method", restriction.methodName);
						mvalues.put("restricted", (restriction.restricted ? 0 : 1) + (restriction.asked ? 2 : 0));
						db.insertWithOnConflict(cTableRestriction, null, mvalues, SQLiteDatabase.CONFLICT_REPLACE);
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Clear cache
				if (mUseCache)
					synchronized (mRestrictionCache) {
						CRestriction key = new CRestriction(new ParcelableRestriction(restriction.uid,
								restriction.restrictionName, null));
						if (mRestrictionCache.containsKey(key))
							mRestrictionCache.remove(key);
						for (Hook hook : PrivacyManager.getHooks(restriction.restrictionName)) {
							key = new CRestriction(new ParcelableRestriction(restriction.uid,
									restriction.restrictionName, hook.getName()));
							if (mRestrictionCache.containsKey(key))
								mRestrictionCache.remove(key);
						}
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
		}

		@Override
		public void setRestrictionList(List<ParcelableRestriction> listRestriction) throws RemoteException {
			// Permission enforced by setRestriction
			for (ParcelableRestriction restriction : listRestriction)
				setRestriction(restriction);
		}

		@Override
		public ParcelableRestriction getRestriction(final ParcelableRestriction restriction, boolean usage,
				String secret) throws RemoteException {
			final ParcelableRestriction result = new ParcelableRestriction(restriction.uid,
					restriction.restrictionName, restriction.methodName);

			try {
				// No permissions enforced, but usage data requires a secret

				// Check for self
				if (Util.getAppId(restriction.uid) == getXUid()) {
					if (PrivacyManager.cIdentification.equals(restriction.restrictionName)
							&& "getString".equals(restriction.methodName))
						return result;
					if (PrivacyManager.cIPC.equals(restriction.restrictionName))
						return result;
					else if (PrivacyManager.cStorage.equals(restriction.restrictionName))
						return result;
					else if (PrivacyManager.cSystem.equals(restriction.restrictionName))
						return result;
					else if (PrivacyManager.cView.equals(restriction.restrictionName))
						return result;
				}

				// Get meta data
				Hook hook = PrivacyManager.getHook(restriction.restrictionName, restriction.methodName);

				// Check for system component
				if (usage && !PrivacyManager.isApplication(restriction.uid))
					if (!getSettingBool(0, PrivacyManager.cSettingSystem, false))
						return result;

				// Check if restrictions enabled
				if (usage && !getSettingBool(restriction.uid, PrivacyManager.cSettingRestricted, true))
					return result;

				// Check cache
				boolean cached = false;
				if (mUseCache) {
					CRestriction key = new CRestriction(restriction);
					synchronized (mRestrictionCache) {
						if (mRestrictionCache.containsKey(key)) {
							cached = true;
							ParcelableRestriction cache = mRestrictionCache.get(key).getRestriction();
							result.restricted = cache.restricted;
							result.asked = cache.asked;
						}
					}
				}

				if (!cached) {
					// No permissions required
					SQLiteDatabase db = getDatabase();

					// Precompile statement when needed
					if (stmtGetRestriction == null) {
						String sql = "SELECT restricted FROM " + cTableRestriction
								+ " WHERE uid=? AND restriction=? AND method=?";
						stmtGetRestriction = db.compileStatement(sql);
					}

					// Execute statement
					db.beginTransaction();
					try {
						try {
							synchronized (stmtGetRestriction) {
								stmtGetRestriction.clearBindings();
								stmtGetRestriction.bindLong(1, restriction.uid);
								stmtGetRestriction.bindString(2, restriction.restrictionName);
								stmtGetRestriction.bindString(3, "");
								long state = stmtGetRestriction.simpleQueryForLong();
								result.restricted = ((state & 1) != 0);
								result.asked = ((state & 2) != 0);
							}
						} catch (SQLiteDoneException ignored) {
						}

						if (restriction.methodName != null)
							try {
								synchronized (stmtGetRestriction) {
									stmtGetRestriction.clearBindings();
									stmtGetRestriction.bindLong(1, restriction.uid);
									stmtGetRestriction.bindString(2, restriction.restrictionName);
									stmtGetRestriction.bindString(3, restriction.methodName);
									long state = stmtGetRestriction.simpleQueryForLong();
									if (result.restricted)
										result.restricted = ((state & 1) == 0);
									if (!result.asked)
										result.asked = ((state & 2) != 0);
								}
							} catch (SQLiteDoneException ignored) {
								// no change
							}

						db.setTransactionSuccessful();
					} finally {
						db.endTransaction();
					}

					// Fallback
					if (!result.restricted && usage && PrivacyManager.isApplication(restriction.uid)
							&& !getSettingBool(0, PrivacyManager.cSettingMigrated, false)) {
						if (hook != null && !hook.isDangerous()) {
							result.restricted = PrivacyProvider.getRestrictedFallback(null, restriction.uid,
									restriction.restrictionName, restriction.methodName);
							Util.log(null, Log.WARN, "Fallback uid=" + restriction.uid + " "
									+ restriction.restrictionName + restriction.methodName + "=" + result.restricted);
						}
					}

					// Update cache
					if (mUseCache) {
						CRestriction key = new CRestriction(result);
						synchronized (mRestrictionCache) {
							if (mRestrictionCache.containsKey(key))
								mRestrictionCache.remove(key);
							mRestrictionCache.put(key, key);
						}
					}
				}

				// Media: notify user
				if (result.restricted && usage && PrivacyManager.cMedia.equals(restriction.restrictionName))
					notifyRestricted(restriction);

				// Ask to restrict
				if (!result.asked && usage && PrivacyManager.isApplication(restriction.uid))
					result.restricted = onDemandDialog(hook, restriction);

				// Log usage
				if (usage && hook.hasUsageData() && restriction.methodName != null)
					if (getSettingBool(0, PrivacyManager.cSettingUsage, true)) {
						// Check secret
						boolean allowed = true;
						if (Util.getAppId(Binder.getCallingUid()) != getXUid()) {
							if (mSecret == null || !mSecret.equals(secret)) {
								allowed = false;
								Util.log(null, Log.WARN, "Invalid secret");
							}
						}

						if (allowed) {
							mExecutor.execute(new Runnable() {
								public void run() {
									try {
										SQLiteDatabase db = getDatabase();

										db.beginTransaction();
										try {
											ContentValues values = new ContentValues();
											values.put("uid", restriction.uid);
											values.put("restriction", restriction.restrictionName);
											values.put("method", restriction.methodName);
											values.put("restricted", result.restricted);
											values.put("time", new Date().getTime());
											db.insertWithOnConflict(cTableUsage, null, values,
													SQLiteDatabase.CONFLICT_REPLACE);

											db.setTransactionSuccessful();
										} finally {
											db.endTransaction();
										}
									} catch (Throwable ex) {
										Util.bug(null, ex);
										synchronized (mListError) {
											mListError.add(ex.toString());
											mListError.add(Log.getStackTraceString(ex));
										}
									}
								}
							});
						}
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
			}
			return result;
		}

		@Override
		public List<ParcelableRestriction> getRestrictionList(ParcelableRestriction selector) throws RemoteException {
			List<ParcelableRestriction> result = new ArrayList<ParcelableRestriction>();
			try {
				enforcePermission();

				if (selector.restrictionName == null)
					for (String sRestrictionName : PrivacyManager.getRestrictions()) {
						ParcelableRestriction restriction = new ParcelableRestriction(selector.uid, sRestrictionName,
								null, false);
						restriction.restricted = getRestriction(restriction, false, mSecret).restricted;
						result.add(restriction);
					}
				else
					for (Hook md : PrivacyManager.getHooks(selector.restrictionName)) {
						ParcelableRestriction restriction = new ParcelableRestriction(selector.uid,
								selector.restrictionName, md.getName(), false);
						restriction.restricted = getRestriction(restriction, false, mSecret).restricted;
						result.add(restriction);
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
			return result;
		}

		@Override
		public void deleteRestrictions(int uid) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					db.delete(cTableRestriction, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Restrictions deleted uid=" + uid);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Clear cache
				if (mUseCache)
					synchronized (mRestrictionCache) {
						mRestrictionCache.clear();
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
		}

		// Usage

		@Override
		public long getUsage(List<ParcelableRestriction> listRestriction) throws RemoteException {
			long lastUsage = 0;
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				// Precompile statement when needed
				if (stmtGetUsageRestriction == null) {
					String sql = "SELECT MAX(time) FROM " + cTableUsage + " WHERE uid=? AND restriction=?";
					stmtGetUsageRestriction = db.compileStatement(sql);
				}
				if (stmtGetUsageMethod == null) {
					String sql = "SELECT MAX(time) FROM " + cTableUsage + " WHERE uid=? AND restriction=? AND method=?";
					stmtGetUsageMethod = db.compileStatement(sql);
				}

				db.beginTransaction();
				try {
					for (ParcelableRestriction restriction : listRestriction) {
						if (restriction.methodName == null)
							try {
								synchronized (stmtGetUsageRestriction) {
									stmtGetUsageRestriction.clearBindings();
									stmtGetUsageRestriction.bindLong(1, restriction.uid);
									stmtGetUsageRestriction.bindString(2, restriction.restrictionName);
									lastUsage = Math.max(lastUsage, stmtGetUsageRestriction.simpleQueryForLong());
								}
							} catch (SQLiteDoneException ignored) {
							}
						else
							try {
								synchronized (stmtGetUsageMethod) {
									stmtGetUsageMethod.clearBindings();
									stmtGetUsageMethod.bindLong(1, restriction.uid);
									stmtGetUsageMethod.bindString(2, restriction.restrictionName);
									stmtGetUsageMethod.bindString(3, restriction.methodName);
									lastUsage = Math.max(lastUsage, stmtGetUsageMethod.simpleQueryForLong());
								}
							} catch (SQLiteDoneException ignored) {
							}
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
			return lastUsage;
		}

		@Override
		public List<ParcelableRestriction> getUsageList(int uid) throws RemoteException {
			List<ParcelableRestriction> result = new ArrayList<ParcelableRestriction>();
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					Cursor cursor;
					if (uid == 0)
						cursor = db.query(cTableUsage, new String[] { "uid", "restriction", "method", "restricted",
								"time" }, null, new String[] {}, null, null, null);
					else
						cursor = db.query(cTableUsage, new String[] { "uid", "restriction", "method", "restricted",
								"time" }, "uid=?", new String[] { Integer.toString(uid) }, null, null, null);
					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (usage data)");
					else
						try {
							while (cursor.moveToNext()) {
								ParcelableRestriction data = new ParcelableRestriction();
								data.uid = cursor.getInt(0);
								data.restrictionName = cursor.getString(1);
								data.methodName = cursor.getString(2);
								data.restricted = (cursor.getInt(3) > 0);
								data.time = cursor.getLong(4);
								result.add(data);
							}
						} finally {
							cursor.close();
						}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
			return result;
		}

		@Override
		public void deleteUsage(int uid) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					if (uid == 0)
						db.delete(cTableUsage, null, new String[] {});
					else
						db.delete(cTableUsage, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Usage data deleted uid=" + uid);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
		}

		// Settings

		@Override
		public void setSetting(ParcelableSetting setting) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					if (setting.value == null)
						db.delete(cTableSetting, "uid=? AND name=?", new String[] { Integer.toString(setting.uid),
								setting.name });
					else {
						// Create record
						ContentValues values = new ContentValues();
						values.put("uid", setting.uid);
						values.put("name", setting.name);
						values.put("value", setting.value);

						// Insert/update record
						db.insertWithOnConflict(cTableSetting, null, values, SQLiteDatabase.CONFLICT_REPLACE);
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Update cache
				if (mUseCache) {
					CSetting key = new CSetting(setting.uid, setting.name);
					key.setValue(setting.value);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							mSettingCache.remove(key);
						if (setting.value != null)
							mSettingCache.put(key, key);
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
		}

		@Override
		public void setSettingList(List<ParcelableSetting> listSetting) throws RemoteException {
			// Permission enforced by setSetting
			for (ParcelableSetting setting : listSetting)
				setSetting(setting);
		}

		@Override
		@SuppressLint("DefaultLocale")
		public ParcelableSetting getSetting(ParcelableSetting setting) throws RemoteException {
			ParcelableSetting result = new ParcelableSetting(setting.uid, setting.name, setting.value);
			try {
				// No permissions enforced

				// Check cache
				if (mUseCache && setting.value != null) {
					CSetting key = new CSetting(setting.uid, setting.name);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key)) {
							result.value = mSettingCache.get(key).getValue();
							return result;
						}
					}
				}

				// No persmissions required
				SQLiteDatabase db = getDatabase();

				// Fallback
				if (!PrivacyManager.cSettingMigrated.equals(setting.name)
						&& !getSettingBool(0, PrivacyManager.cSettingMigrated, false)) {
					if (setting.uid == 0)
						result.value = PrivacyProvider.getSettingFallback(setting.name, null, false);
					if (result.value == null) {
						result.value = PrivacyProvider.getSettingFallback(
								String.format("%s.%d", setting.name, setting.uid), setting.value, false);
						return result;
					}
				}

				// Precompile statement when needed
				if (stmtGetSetting == null) {
					String sql = "SELECT value FROM " + cTableSetting + " WHERE uid=? AND name=?";
					stmtGetSetting = db.compileStatement(sql);
				}

				// Execute statement
				db.beginTransaction();
				try {
					try {
						synchronized (stmtGetSetting) {
							stmtGetSetting.clearBindings();
							stmtGetSetting.bindLong(1, setting.uid);
							stmtGetSetting.bindString(2, setting.name);
							String value = stmtGetSetting.simpleQueryForString();
							if (value != null)
								result.value = value;
						}
					} catch (SQLiteDoneException ignored) {
					}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Add to cache
				if (mUseCache && result.value != null) {
					CSetting key = new CSetting(setting.uid, setting.name);
					key.setValue(result.value);
					synchronized (mSettingCache) {
						if (mSettingCache.containsKey(key))
							mSettingCache.remove(key);
						mSettingCache.put(key, key);
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
			}
			return result;
		}

		@Override
		public List<ParcelableSetting> getSettingList(int uid) throws RemoteException {
			List<ParcelableSetting> listSetting = new ArrayList<ParcelableSetting>();
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					Cursor cursor = db.query(cTableSetting, new String[] { "name", "value" }, "uid=?",
							new String[] { Integer.toString(uid) }, null, null, null);
					if (cursor == null)
						Util.log(null, Log.WARN, "Database cursor null (settings)");
					else
						try {
							while (cursor.moveToNext())
								listSetting.add(new ParcelableSetting(uid, cursor.getString(0), cursor.getString(1)));
						} finally {
							cursor.close();
						}

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
			return listSetting;
		}

		@Override
		public void deleteSettings(int uid) throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					db.delete(cTableSetting, "uid=?", new String[] { Integer.toString(uid) });
					Util.log(null, Log.WARN, "Settings deleted uid=" + uid);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Clear cache
				if (mUseCache)
					synchronized (mSettingCache) {
						mSettingCache.clear();
					}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
		}

		@Override
		public void clear() throws RemoteException {
			try {
				enforcePermission();
				SQLiteDatabase db = getDatabase();

				db.beginTransaction();
				try {
					db.execSQL("DELETE FROM restriction");
					db.execSQL("DELETE FROM setting");
					db.execSQL("DELETE FROM usage");
					Util.log(null, Log.WARN, "Database cleared");

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}

				// Clear caches
				if (mUseCache) {
					synchronized (mRestrictionCache) {
						mRestrictionCache.clear();
					}
					synchronized (mSettingCache) {
						mSettingCache.clear();
					}
					Util.log(null, Log.WARN, "Cache cleared");
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
				throw new RemoteException(ex.toString());
			}
		}

		// Helper methods

		private Boolean onDemandDialog(Hook hook, final ParcelableRestriction restriction) {
			final ParcelableRestriction result = new ParcelableRestriction(restriction.uid,
					restriction.restrictionName, null, false);
			try {
				// Without handler nothing can be done
				if (mHandler == null)
					return false;

				if (restriction.methodName == null)
					return false;

				// Check if enabled
				if (!getSettingBool(0, PrivacyManager.cSettingOnDemand, true))
					return false;
				if (!getSettingBool(restriction.uid, PrivacyManager.cSettingOnDemand, false))
					return false;

				// Skip dangerous methods
				boolean dangerous = getSettingBool(0, PrivacyManager.cSettingDangerous, false);
				if (!dangerous && hook.isDangerous())
					return false;

				// Get am context
				final Context context = getContext();
				if (context == null)
					return false;

				// Go ask
				Util.log(null, Log.WARN, "On demand uid=" + restriction.uid + " " + restriction.restrictionName + "/"
						+ restriction.methodName);
				synchronized (this) {
					mOnDemanding = true;

					// Create semaphore
					final Semaphore semaphore = new Semaphore(1);
					semaphore.acquireUninterruptibly();

					Util.log(null, Log.WARN, "On demanding uid=" + restriction.uid + " " + restriction.restrictionName
							+ "/" + restriction.methodName);

					// Run dialog in looper
					mHandler.post(new Runnable() {
						@Override
						public void run() {
							try {
								// Get resources
								String self = PrivacyService.class.getPackage().getName();
								Resources resources = context.getPackageManager().getResourcesForApplication(self);

								// Build message
								ApplicationInfoEx appInfo = new ApplicationInfoEx(context, restriction.uid);
								int stringId = resources.getIdentifier("restrict_" + restriction.restrictionName,
										"string", self);
								String message = String.format(resources.getString(R.string.msg_ondemand),
										TextUtils.join(", ", appInfo.getApplicationName()),
										resources.getString(stringId), restriction.methodName);

								int hmargin = resources.getDimensionPixelSize(R.dimen.activity_horizontal_margin);
								int vmargin = resources.getDimensionPixelSize(R.dimen.activity_vertical_margin);

								// Build view
								LinearLayout llContainer = new LinearLayout(context);
								llContainer.setOrientation(LinearLayout.VERTICAL);
								LinearLayout.LayoutParams llContainerParams = new LinearLayout.LayoutParams(
										LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.MATCH_PARENT);
								llContainer.setLayoutParams(llContainerParams);
								llContainer.setPadding(hmargin, vmargin, hmargin, vmargin);

								// Container for icon & message
								LinearLayout llApplication = new LinearLayout(context);
								llApplication.setOrientation(LinearLayout.HORIZONTAL);
								LinearLayout.LayoutParams llApplicationParams = new LinearLayout.LayoutParams(
										LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.MATCH_PARENT);
								llApplication.setLayoutParams(llApplicationParams);
								{
									// Application icon
									ImageView ivApp = new ImageView(context);
									ivApp.setImageDrawable(appInfo.getIcon(context));
									llApplication.addView(ivApp);

									// Message
									TextView tvMessage = new TextView(context);
									tvMessage.setText(message);
									LinearLayout.LayoutParams tvMessageParams = new LinearLayout.LayoutParams(
											LinearLayout.LayoutParams.WRAP_CONTENT,
											LinearLayout.LayoutParams.WRAP_CONTENT);
									tvMessageParams.setMargins(hmargin / 2, 0, 0, 0);
									tvMessage.setLayoutParams(tvMessageParams);
									llApplication.addView(tvMessage);
								}
								llContainer.addView(llApplication);

								// Category check box
								final CheckBox cbCategory = new CheckBox(context);
								cbCategory.setText(resources.getString(R.string.menu_category));
								cbCategory.setChecked(true);
								LinearLayout.LayoutParams llCategoryParams = new LinearLayout.LayoutParams(
										LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
								llCategoryParams.setMargins(0, vmargin / 2, 0, 0);
								cbCategory.setLayoutParams(llCategoryParams);
								llContainer.addView(cbCategory);

								// Ask
								AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
								alertDialogBuilder.setTitle(resources.getString(R.string.app_name));
								alertDialogBuilder.setView(llContainer);
								alertDialogBuilder.setIcon(resources.getDrawable(R.drawable.ic_launcher));
								alertDialogBuilder.setPositiveButton(resources.getString(R.string.title_deny),
										new DialogInterface.OnClickListener() {
											@Override
											public void onClick(DialogInterface dialog, int which) {
												// Deny
												result.restricted = true;
												onDemandChoice(restriction, cbCategory.isChecked(), true);
												semaphore.release();
											}
										});
								alertDialogBuilder.setNeutralButton(resources.getString(R.string.title_denyonce),
										new DialogInterface.OnClickListener() {
											@Override
											public void onClick(DialogInterface dialog, int which) {
												// Deny
												result.restricted = true;
												semaphore.release();
											}
										});
								alertDialogBuilder.setNegativeButton(resources.getString(R.string.title_allow),
										new DialogInterface.OnClickListener() {
											@Override
											public void onClick(DialogInterface dialog, int which) {
												// Allow
												result.restricted = false;
												onDemandChoice(restriction, cbCategory.isChecked(), false);
												semaphore.release();
											}
										});
								AlertDialog alertDialog = alertDialogBuilder.create();
								alertDialog.getWindow().setType(WindowManager.LayoutParams.TYPE_SYSTEM_ALERT);
								alertDialog.setCancelable(false);
								alertDialog.setCanceledOnTouchOutside(false);
								alertDialog.show();
							} catch (Throwable ex) {
								Util.bug(null, ex);
								synchronized (mListError) {
									mListError.add(ex.toString());
									mListError.add(Log.getStackTraceString(ex));
								}
								semaphore.release();
							}
						}
					});

					// Wait for dialog
					semaphore.acquireUninterruptibly();
					mOnDemanding = false;
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
			}
			return result.restricted;
		}

		private void onDemandChoice(ParcelableRestriction restriction, boolean category, boolean restricted) {
			try {
				if (category || restricted) {
					// Set category restricted, but if the user has requested
					// blocking a single function, keep asking for the others
					ParcelableRestriction result = new ParcelableRestriction(restriction.uid,
							restriction.restrictionName, null, restricted, category);
					setRestriction(result);

					// Make exceptions for dangerous methods
					boolean dangerous = getSettingBool(0, PrivacyManager.cSettingDangerous, false);
					if (restricted && !dangerous) {
						result.restricted = dangerous;
						result.asked = true;

						for (Hook hook : PrivacyManager.getHooks(restriction.restrictionName))
							if (hook.isDangerous()) {
								result.methodName = hook.getName();
								setRestriction(result);
							}
					}

					// Restrict the requested function & don't ask again
					if (!category) {
						result.methodName = restriction.methodName;
						result.restricted = true;
						result.asked = true;
						setRestriction(result);
					}
				} else {
					// Leave the category, add an exception for the function
					ParcelableRestriction result = new ParcelableRestriction(restriction.uid,
							restriction.restrictionName, restriction.methodName, restricted, true);
					setRestriction(result);
				}

				// Mark state as changed
				setSetting(new ParcelableSetting(restriction.uid, PrivacyManager.cSettingState,
						Integer.toString(ActivityMain.STATE_CHANGED)));

				// Update modification time
				setSetting(new ParcelableSetting(restriction.uid, PrivacyManager.cSettingModifyTime,
						Long.toString(System.currentTimeMillis())));
			} catch (Throwable ex) {
				Util.bug(null, ex);
				synchronized (mListError) {
					mListError.add(ex.toString());
					mListError.add(Log.getStackTraceString(ex));
				}
			}
		}

		private void notifyRestricted(ParcelableRestriction restriction) {
			final Context context = getContext();
			if (context != null && mHandler != null)
				mHandler.post(new Runnable() {
					@Override
					public void run() {
						try {
							// Get resources
							String self = PrivacyService.class.getPackage().getName();
							Resources resources = context.getPackageManager().getResourcesForApplication(self);

							// Notify user
							Toast.makeText(context, resources.getString(R.string.msg_restrictedby), Toast.LENGTH_LONG)
									.show();
						} catch (NameNotFoundException ex) {
							Util.bug(null, ex);
						}
					}
				});
		}

		private boolean getSettingBool(int uid, String name, boolean defaultValue) throws RemoteException {
			String value = getSetting(new ParcelableSetting(uid, name, Boolean.toString(defaultValue))).value;
			return Boolean.parseBoolean(value);
		}
	};

	private static void enforcePermission() {
		int callingUid = Util.getAppId(Binder.getCallingUid());
		if (callingUid != getXUid() && callingUid != Process.SYSTEM_UID) {
			String callingPkg = null;
			Context context = getContext();
			if (context != null) {
				PackageManager pm = context.getPackageManager();
				if (pm != null)
					callingPkg = TextUtils.join(", ", pm.getPackagesForUid(Binder.getCallingUid()));
			}
			throw new SecurityException("xuid=" + mXUid + " calling=" + Binder.getCallingUid() + "/" + callingPkg);
		}
	}

	private static Context getContext() {
		// public static ActivityManagerService self()
		// frameworks/base/services/java/com/android/server/am/ActivityManagerService.java
		try {
			Class<?> cam = Class.forName("com.android.server.am.ActivityManagerService");
			Object am = cam.getMethod("self").invoke(null);
			if (am == null)
				return null;
			return (Context) cam.getDeclaredField("mContext").get(am);
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return null;
		}
	}

	private static int getXUid() {
		if (mXUid < 0)
			try {
				Context context = getContext();
				if (context != null) {
					PackageManager pm = context.getPackageManager();
					if (pm != null) {
						String self = PrivacyService.class.getPackage().getName();
						ApplicationInfo xInfo = pm.getApplicationInfo(self, 0);
						mXUid = xInfo.uid;
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		return mXUid;
	}

	private static File getDbFile() {
		return new File(Environment.getDataDirectory() + File.separator + "xprivacy" + File.separator + "xprivacy.db");
	}

	public static void setupDatabase() {
		// This is run from Zygote with root permissions
		try {
			File dbFile = getDbFile();

			// Create database folder
			dbFile.getParentFile().mkdirs();

			// Set database file permissions
			// Owner: rwx (system)
			// Group: --- (system)
			// World: ---
			Util.setPermission(dbFile.getParentFile().getAbsolutePath(), 0770, Process.SYSTEM_UID, Process.SYSTEM_UID);
			File[] files = dbFile.getParentFile().listFiles();
			if (files != null)
				for (File file : files)
					Util.setPermission(file.getAbsolutePath(), 0770, Process.SYSTEM_UID, Process.SYSTEM_UID);

			// Move database from app folder
			File folder = new File(Environment.getDataDirectory() + File.separator + "data" + File.separator
					+ PrivacyService.class.getPackage().getName());
			File[] oldFiles = folder.listFiles();
			if (oldFiles != null)
				for (File file : oldFiles)
					if (file.getName().startsWith("xprivacy.db")) {
						File target = new File(dbFile.getParentFile() + File.separator + file.getName());
						boolean status = file.renameTo(target);
						Util.log(null, Log.WARN, "Moving " + file + " to " + target + " ok=" + status);
					}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private static SQLiteDatabase getDatabase() {
		// Create/upgrade database when needed
		if (mDatabase == null) {
			File dbFile = getDbFile();
			SQLiteDatabase db = SQLiteDatabase.openOrCreateDatabase(dbFile, null);

			// Update migration status
			if (db.getVersion() > 1) {
				Util.log(null, Log.WARN, "Updating migration status");
				db.beginTransaction();
				try {
					ContentValues values = new ContentValues();
					values.put("uid", 0);
					values.put("name", PrivacyManager.cSettingMigrated);
					values.put("value", Boolean.toString(true));
					db.insertWithOnConflict(cTableSetting, null, values, SQLiteDatabase.CONFLICT_REPLACE);

					db.setTransactionSuccessful();
				} finally {
					db.endTransaction();
				}
			}

			// Upgrade database if needed
			if (db.needUpgrade(1)) {
				db.beginTransaction();
				try {
					// http://www.sqlite.org/lang_createtable.html
					Util.log(null, Log.WARN, "Creating database");
					db.execSQL("CREATE TABLE restriction (uid INTEGER NOT NULL, restriction TEXT NOT NULL, method TEXT NOT NULL, restricted INTEGER NOT NULL)");
					db.execSQL("CREATE TABLE setting (uid INTEGER NOT NULL, name TEXT NOT NULL, value TEXT)");
					db.execSQL("CREATE TABLE usage (uid INTEGER NOT NULL, restriction TEXT NOT NULL, method TEXT NOT NULL, restricted INTEGER NOT NULL, time INTEGER NOT NULL)");
					db.execSQL("CREATE UNIQUE INDEX idx_restriction ON restriction(uid, restriction, method)");
					db.execSQL("CREATE UNIQUE INDEX idx_setting ON setting(uid, name)");
					db.execSQL("CREATE UNIQUE INDEX idx_usage ON usage(uid, restriction, method)");
					db.setVersion(1);
					db.setTransactionSuccessful();
				} catch (Throwable ex) {
					Util.bug(null, ex);
				} finally {
					db.endTransaction();
				}

			}

			if (db.needUpgrade(2))
				// Old migrated indication
				db.setVersion(2);

			if (db.needUpgrade(3)) {
				db.beginTransaction();
				try {
					db.execSQL("DELETE FROM usage WHERE method=''");
					db.setVersion(3);
					db.setTransactionSuccessful();
				} catch (Throwable ex) {
					Util.bug(null, ex);
				} finally {
					db.endTransaction();
				}
			}

			if (db.needUpgrade(4)) {
				db.beginTransaction();
				try {
					db.execSQL("DELETE FROM setting WHERE value IS NULL");
					db.setVersion(4);
					db.setTransactionSuccessful();
				} catch (Throwable ex) {
					Util.bug(null, ex);
				} finally {
					db.endTransaction();
				}
			}

			if (db.needUpgrade(5)) {
				db.beginTransaction();
				try {
					db.execSQL("DELETE FROM setting WHERE value = ''");
					db.execSQL("DELETE FROM setting WHERE name = 'Random@boot' AND value = 'false'");
					db.setVersion(5);
					db.setTransactionSuccessful();
				} catch (Throwable ex) {
					Util.bug(null, ex);
				} finally {
					db.endTransaction();
				}
			}

			if (db.needUpgrade(6)) {
				db.beginTransaction();
				try {
					db.execSQL("DELETE FROM setting WHERE name LIKE 'OnDemand.%'");
					db.setVersion(6);
					db.setTransactionSuccessful();
				} catch (Throwable ex) {
					Util.bug(null, ex);
				} finally {
					db.endTransaction();
				}
			}

			Util.log(null, Log.WARN, "Database version=" + db.getVersion());
			mDatabase = db;
		}

		return mDatabase;
	}
}

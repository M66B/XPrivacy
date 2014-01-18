package biz.bokhorst.xprivacy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import javax.xml.parsers.SAXParserFactory;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.json.JSONArray;
import org.json.JSONObject;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xmlpull.v1.XmlSerializer;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.database.Cursor;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.provider.ContactsContract;
import android.provider.Settings.Secure;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;
import android.util.SparseArray;
import android.util.Xml;
import android.widget.Toast;

public class ActivityShare extends Activity {
	private LocalBroadcastManager mBroadcastManager;

	public static final String cFileName = "FileName";
	public static final String cUidList = "UidList";
	public static final String cErrorMessage = "ErrorMessage";
	public static final String HTTP_BASE_URL = "http://crowd.xprivacy.eu/";
	public static final String HTTPS_BASE_URL = "https://crowd.xprivacy.eu/";
	public static final String cProgressReport = "ProgressReport";
	public static final String cProgressMessage = "ProgressMessage";
	public static final String cProgressValue = "ProgressValue";
	public static final String cProgressMax = "ProgressMax";

	public static final int cSubmitLimit = 10;
	public static final int cProtocolVersion = 4;

	public static final String ACTION_EXPORT = "biz.bokhorst.xprivacy.action.EXPORT";
	public static final String ACTION_IMPORT = "biz.bokhorst.xprivacy.action.IMPORT";
	public static final String ACTION_FETCH = "biz.bokhorst.xprivacy.action.FETCH";
	public static final String ACTION_SUBMIT = "biz.bokhorst.xprivacy.action.SUBMIT";

	public static final int TIMEOUT_MILLISEC = 45000;

	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors(),
			new PriorityThreadFactory());

	private static class PriorityThreadFactory implements ThreadFactory {
		@Override
		public Thread newThread(Runnable r) {
			Thread t = new Thread(r);
			t.setPriority(Thread.NORM_PRIORITY);
			return t;
		}
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		Bundle extras = getIntent().getExtras();
		mBroadcastManager = LocalBroadcastManager.getInstance(this);

		if (Util.isProEnabled() || Util.hasProLicense(this) != null) {
			// Import
			if (getIntent().getAction().equals(ACTION_IMPORT)) {
				int[] uid = (extras != null && extras.containsKey(cUidList) ? extras.getIntArray(cUidList) : new int[0]);
				String fileName = (extras != null && extras.containsKey(cFileName) ? extras.getString(cFileName)
						: getFileName(false));
				new ImportTask().executeOnExecutor(mExecutor, new File(fileName), uid);
			}

			// Export
			else if (getIntent().getAction().equals(ACTION_EXPORT)) {
				String fileName = (extras != null && extras.containsKey(cFileName) ? extras.getString(cFileName)
						: getFileName(false));
				new ExportTask().executeOnExecutor(mExecutor, new File(fileName));
			}
		}

		if (Util.hasProLicense(this) != null) {
			// Fetch
			if (getIntent().getAction().equals(ACTION_FETCH)) {
				if (extras != null && extras.containsKey(cUidList)) {
					int[] uid = extras.getIntArray(cUidList);
					new FetchTask().executeOnExecutor(mExecutor, uid);
				}
			}
		}

		// Submit
		if (getIntent().getAction().equals(ACTION_SUBMIT)) {
			if (extras != null && extras.containsKey(cUidList)) {
				int[] uid = extras.getIntArray(cUidList);
				if (uid.length <= cSubmitLimit) {
					new SubmitTask().executeOnExecutor(mExecutor, uid);
				} else {
					Intent intent = new Intent();
					intent.putExtra(cErrorMessage, getString(R.string.msg_limit, ActivityShare.cSubmitLimit + 1));
					setResult(1, intent);
					finish();
				}
			}
		}
	}

	// Tasks

	private class ExportTask extends AsyncTask<File, String, String> {
		private File mFile;

		@Override
		protected String doInBackground(File... params) {
			try {
				mFile = params[0];
				Util.log(null, Log.INFO, "Exporting " + mFile);
				String android_id = Secure.getString(getContentResolver(), Secure.ANDROID_ID);

				FileOutputStream fos = new FileOutputStream(mFile);
				try {
					// Start serialization
					XmlSerializer serializer = Xml.newSerializer();
					serializer.setOutput(fos, "UTF-8");
					serializer.startDocument(null, Boolean.valueOf(true));
					serializer.setFeature("http://xmlpull.org/v1/doc/features.html#indent-output", true);
					serializer.startTag(null, "XPrivacy");

					// Process package map
					for (PackageInfo pInfo : getPackageManager().getInstalledPackages(0)) {
						serializer.startTag(null, "PackageInfo");
						serializer.attribute(null, "Id", Integer.toString(pInfo.applicationInfo.uid));
						serializer.attribute(null, "Name", pInfo.packageName);
						serializer.endTag(null, "PackageInfo");
					}

					// Process global settings
					Map<String, String> mapGlobalSetting = PrivacyManager.getSettings(0);
					for (String name : mapGlobalSetting.keySet()) {
						String value = mapGlobalSetting.get(name);

						// Serialize setting
						serializer.startTag(null, "Setting");
						serializer.attribute(null, "Id", "");
						serializer.attribute(null, "Name", name);
						if (value != null)
							serializer.attribute(null, "Value", value);
						serializer.endTag(null, "Setting");
					}

					// Build list of distinct uids
					List<Integer> listUid = new ArrayList<Integer>();
					for (PackageInfo pInfo : getPackageManager().getInstalledPackages(0))
						if (!listUid.contains(pInfo.applicationInfo.uid))
							listUid.add(pInfo.applicationInfo.uid);

					// Process application settings
					for (int uid : listUid) {
						Map<String, String> mapAppSetting = PrivacyManager.getSettings(uid);
						for (String name : mapAppSetting.keySet()) {
							// Bind accounts/contacts to same device
							if (name.startsWith(PrivacyManager.cSettingAccount)
									|| name.startsWith(PrivacyManager.cSettingContact)
									|| name.startsWith(PrivacyManager.cSettingRawContact)) {
								name += "." + android_id;
							}

							String value = mapAppSetting.get(name);

							// Serialize setting
							serializer.startTag(null, "Setting");
							serializer.attribute(null, "Id", Integer.toString(uid));
							serializer.attribute(null, "Name", name);
							if (value != null)
								serializer.attribute(null, "Value", value);
							serializer.endTag(null, "Setting");
						}
					}

					// Process restrictions
					for (int uid : listUid) {
						for (String restrictionName : PrivacyManager.getRestrictions()) {
							// Category
							boolean crestricted = PrivacyManager.getRestricted(null, uid, restrictionName, null, false,
									false);
							if (crestricted) {
								serializer.startTag(null, "Restriction");
								serializer.attribute(null, "Id", Integer.toString(uid));
								serializer.attribute(null, "Name", restrictionName);
								serializer.attribute(null, "Restricted", Boolean.toString(crestricted));
								serializer.endTag(null, "Restriction");

								// Methods
								for (PrivacyManager.MethodDescription md : PrivacyManager.getMethods(restrictionName)) {
									boolean mrestricted = PrivacyManager.getRestricted(null, uid, restrictionName,
											md.getName(), false, false);
									if (!mrestricted || md.isDangerous()) {
										serializer.startTag(null, "Restriction");
										serializer.attribute(null, "Id", Integer.toString(uid));
										serializer.attribute(null, "Name", restrictionName);
										serializer.attribute(null, "Method", md.getName());
										serializer.attribute(null, "Restricted", Boolean.toString(mrestricted));
										serializer.endTag(null, "Restriction");
									}
								}
							}
						}
					}

					// End serialization
					serializer.endTag(null, "XPrivacy");
					serializer.endDocument();
					serializer.flush();
				} finally {
					fos.close();
				}

				// Display message
				Util.log(null, Log.INFO, "Exporting finished");
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex.getMessage();
			}
		}

		@Override
		protected void onProgressUpdate(String... values) {
			int progress = 0;
			int max = 1;
			if (values.length > 2) {
				progress = Integer.parseInt(values[1]);
				max = Integer.parseInt(values[2]);
			}
			notify(values[0], true, progress, max);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(String result) {
			notify(result == null ? getString(R.string.msg_done) : null, false, 0, 1);

			Intent intent = new Intent();
			intent.putExtra(cFileName, mFile.getAbsolutePath());
			intent.putExtra(cErrorMessage, result);
			setResult(result == null ? 0 : 1, intent);
			finish();
			super.onPostExecute(result);
		}

		private void notify(String text, boolean ongoing, int progress, int max) {
			// Send progress info to main activity
			Intent progressIntent = new Intent(cProgressReport);
			progressIntent.putExtra(cProgressMessage, String.format("%s: %s", getString(R.string.menu_export), text));
			progressIntent.putExtra(cProgressMax, max);
			progressIntent.putExtra(cProgressValue, progress);
			mBroadcastManager.sendBroadcast(progressIntent);
		}
	}

	private class ImportTask extends AsyncTask<Object, String, String> {
		private File mFile;
		private int mProgressMax;
		private int mProgressCurrent;

		@Override
		protected String doInBackground(Object... params) {
			try {
				mFile = (File) params[0];

				List<Integer> listUidSelected = new ArrayList<Integer>();
				for (int uid : (int[]) params[1])
					listUidSelected.add(uid);

				// Parse XML
				Util.log(null, Log.INFO, "Importing " + mFile);
				FileInputStream fis = null;
				Map<String, Map<String, List<ImportHandler.MethodDescription>>> mapPackage;
				try {
					fis = new FileInputStream(mFile);
					XMLReader xmlReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
					ImportHandler importHandler = new ImportHandler(listUidSelected);
					xmlReader.setContentHandler(importHandler);
					xmlReader.parse(new InputSource(fis));
					mapPackage = importHandler.getPackageMap();
				} finally {
					if (fis != null)
						fis.close();
				}

				// Progress
				mProgressMax = mapPackage.size();
				mProgressCurrent = 0;

				// Process result (legacy)
				for (String packageName : mapPackage.keySet()) {
					mProgressCurrent++;
					try {
						publishProgress(packageName, Integer.toString(mProgressCurrent));

						// Get uid
						int uid = getPackageManager().getPackageInfo(packageName, 0).applicationInfo.uid;

						if (listUidSelected.size() == 0 || listUidSelected.contains(uid)) {
							Util.log(null, Log.INFO, "Importing " + packageName);

							// Reset existing restrictions
							PrivacyManager.deleteRestrictions(uid, true);

							// Set imported restrictions
							for (String restrictionName : mapPackage.get(packageName).keySet()) {
								PrivacyManager.setRestricted(null, uid, restrictionName, null, true, true);
								for (ImportHandler.MethodDescription md : mapPackage.get(packageName).get(
										restrictionName))
									PrivacyManager.setRestricted(null, uid, restrictionName, md.getMethodName(),
											md.isRestricted(), true);
							}
						}
					} catch (NameNotFoundException ex) {
						Util.log(null, Log.WARN, "Not found package=" + packageName);
					}
				}

				// Display message
				Util.log(null, Log.INFO, "Importing finished");
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex.getMessage();
			}
		}

		@Override
		protected void onProgressUpdate(String... values) {
			int progress = 0;
			if (values.length > 1)
				progress = Integer.parseInt(values[1]);
			notify(values[0], true, progress);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(String result) {
			notify(result == null ? getString(R.string.msg_done) : result, false, 0);

			Intent intent = new Intent();
			intent.putExtra(cFileName, mFile.getAbsolutePath());
			intent.putExtra(cErrorMessage, result);
			setResult(result == null ? 0 : 1, intent);
			finish();
			super.onPostExecute(result);
		}

		private void notify(String text, boolean ongoing, int progress) {
			// Send progress info to main activity
			Intent progressIntent = new Intent(cProgressReport);
			progressIntent.putExtra(cProgressMessage, String.format("%s: %s", getString(R.string.menu_import), text));
			progressIntent.putExtra(cProgressMax, mProgressMax);
			progressIntent.putExtra(cProgressValue, progress);
			mBroadcastManager.sendBroadcast(progressIntent);
		}
	}

	private class ImportHandler extends DefaultHandler {
		private List<Integer> mListUidSelected;
		private SparseArray<String> mMapId = new SparseArray<String>();
		private Map<String, Integer> mMapUid = new HashMap<String, Integer>();
		private Map<String, Map<String, List<MethodDescription>>> mMapPackage = new HashMap<String, Map<String, List<MethodDescription>>>();
		private String android_id = Secure.getString(getContentResolver(), Secure.ANDROID_ID);
		private int mProgress = 0;
		private List<Integer> mListSettingId = new ArrayList<Integer>();
		private List<Integer> mListRestrictionId = new ArrayList<Integer>();

		public ImportHandler(List<Integer> listUidSelected) {
			mListUidSelected = listUidSelected;
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			try {
				if (qName.equals("XPrivacy")) {
					// Root
					mProgress = 0;
				} else if (qName.equals("PackageInfo")) {
					// Package info
					int id = Integer.parseInt(attributes.getValue("Id"));
					String name = attributes.getValue("Name");
					mMapId.put(id, name);
				} else if (qName.equals("Setting")) {
					// Setting
					String id = attributes.getValue("Id");
					String name = attributes.getValue("Name");
					String value = attributes.getValue("Value");

					// Import accounts/contacts only for same device
					if (name.startsWith(PrivacyManager.cSettingAccount)
							|| name.startsWith(PrivacyManager.cSettingContact)
							|| name.startsWith(PrivacyManager.cSettingRawContact))
						if (name.endsWith("." + android_id))
							name = name.replace("." + android_id, "");
						else
							return;

					if (id == null) {
						// Legacy
						Util.log(null, Log.WARN, "Legacy " + name + "=" + value);
						PrivacyManager.setSetting(null, 0, name, value);
					} else if ("".equals(id))
						// Global setting
						// TODO: clear global settings
						PrivacyManager.setSetting(null, 0, name, value);
					else {
						// Application setting
						int iid = Integer.parseInt(id);
						int uid = getUid(iid);
						if (uid >= 0 && mListUidSelected.size() == 0 || mListUidSelected.contains(uid)) {
							// Clear existing settings
							if (!mListSettingId.contains(iid)) {
								mListSettingId.add(iid);
								PrivacyManager.deleteSettings(uid);
							}

							// Set setting
							PrivacyManager.setSetting(null, uid, name, value);
						}
					}
				} else if (qName.equals("Package")) {
					// Restriction (legacy)
					String packageName = attributes.getValue("Name");
					String restrictionName = attributes.getValue("Restriction");
					String methodName = attributes.getValue("Method");
					boolean restricted = Boolean.parseBoolean(attributes.getValue("Restricted"));
					Util.log(null, Log.WARN, "Legacy package=" + packageName + " " + restrictionName + "/" + methodName
							+ "=" + restricted);

					// Map package restriction
					if (!mMapPackage.containsKey(packageName))
						mMapPackage.put(packageName, new HashMap<String, List<MethodDescription>>());
					if (!mMapPackage.get(packageName).containsKey(restrictionName))
						mMapPackage.get(packageName).put(restrictionName, new ArrayList<MethodDescription>());
					if (methodName != null) {
						MethodDescription md = new MethodDescription(methodName, restricted);
						mMapPackage.get(packageName).get(restrictionName).add(md);
					}
				} else if (qName.equals("Restriction")) {
					// Restriction (new style)
					int id = Integer.parseInt(attributes.getValue("Id"));
					String restrictionName = attributes.getValue("Name");
					String methodName = attributes.getValue("Method");
					boolean restricted = Boolean.parseBoolean(attributes.getValue("Restricted"));

					// Get uid
					int uid = getUid(id);
					if (uid >= 0 && mListUidSelected.size() == 0 || mListUidSelected.contains(uid)) {
						// Clear existing restrictions
						if (!mListRestrictionId.contains(id)) {
							mListRestrictionId.add(id);
							reportProgress(id);
							PrivacyManager.deleteRestrictions(uid, false);
						}

						// Set restriction
						PrivacyManager.setRestricted(null, uid, restrictionName, methodName, restricted, false);
					}
				} else
					Util.log(null, Log.ERROR, "Unknown element name=" + qName);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		private int getUid(int id) {
			String packageName = mMapId.get(id);
			if (packageName == null) {
				Util.log(null, Log.WARN, "Unknown id=" + id);
				return -1;
			} else if (!mMapUid.containsKey(packageName))
				try {
					int newuid = ActivityShare.this.getPackageManager().getPackageInfo(packageName, 0).applicationInfo.uid;
					mMapUid.put(packageName, newuid);
				} catch (NameNotFoundException ex) {
					// Do not lookup again
					mMapUid.put(packageName, -1);
					Util.log(null, Log.WARN, "Unknown package name=" + packageName);
				}
			return (mMapUid.containsKey(packageName) ? mMapUid.get(packageName) : -1);
		}

		public Map<String, Map<String, List<MethodDescription>>> getPackageMap() {
			return mMapPackage;
		}

		public class MethodDescription {
			private String mMethodName;
			private boolean mRestricted;

			public MethodDescription(String methodName, boolean restricted) {
				mMethodName = methodName;
				mRestricted = restricted;
			}

			public String getMethodName() {
				return mMethodName;
			}

			public boolean isRestricted() {
				return mRestricted;
			}
		}

		private void reportProgress(int id) {
			// Send progress info to main activity
			Intent progressIntent = new Intent(cProgressReport);
			progressIntent.putExtra(cProgressMessage,
					String.format("%s: %s", getString(R.string.menu_import), mMapId.get(id)));
			progressIntent.putExtra(cProgressMax, mMapId.size());
			progressIntent.putExtra(cProgressValue, ++mProgress);
			mBroadcastManager.sendBroadcast(progressIntent);
		}
	}

	private class FetchTask extends AsyncTask<int[], String, String> {
		private int mProgressMax;
		private int mProgressCurrent;

		@Override
		@SuppressLint("DefaultLocale")
		protected String doInBackground(int[]... params) {
			try {
				// Get data
				List<ApplicationInfoEx> lstApp = new ArrayList<ApplicationInfoEx>();
				for (int uid : params[0])
					lstApp.add(new ApplicationInfoEx(ActivityShare.this, uid));

				String[] license = Util.getProLicenseUnchecked();
				String android_id = Secure.getString(ActivityShare.this.getContentResolver(), Secure.ANDROID_ID);
				PackageInfo pXPrivacyInfo = getPackageManager().getPackageInfo(getPackageName(), 0);

				String confidence = PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingConfidence, "", false);

				// Initialize progress
				mProgressMax = lstApp.size();
				mProgressCurrent = 0;

				// Process applications
				for (ApplicationInfoEx appInfo : lstApp) {
					mProgressCurrent++;
					if (!appInfo.isSystem() || lstApp.size() == 1) {
						publishProgress(appInfo.getPackageName().get(0), Integer.toString(mProgressCurrent));

						JSONArray appName = new JSONArray();
						for (String name : appInfo.getApplicationName())
							appName.put(name);

						JSONArray pkgName = new JSONArray();
						for (String name : appInfo.getPackageName())
							pkgName.put(name);

						JSONArray pkgVersion = new JSONArray();
						for (String version : appInfo.getPackageVersionName(ActivityShare.this))
							pkgVersion.put(version);

						// Encode package
						JSONObject jRoot = new JSONObject();
						jRoot.put("protocol_version", cProtocolVersion);
						jRoot.put("android_id", Util.md5(android_id).toLowerCase());
						jRoot.put("android_sdk", Build.VERSION.SDK_INT);
						jRoot.put("xprivacy_version", pXPrivacyInfo.versionCode);
						jRoot.put("application_name", appName);
						jRoot.put("package_name", pkgName);
						jRoot.put("package_version", pkgVersion);
						jRoot.put("email", license[1]);
						jRoot.put("signature", license[2]);
						jRoot.put("confidence", confidence);

						// Fetch
						HttpParams httpParams = new BasicHttpParams();
						HttpConnectionParams.setConnectionTimeout(httpParams, TIMEOUT_MILLISEC);
						HttpConnectionParams.setSoTimeout(httpParams, TIMEOUT_MILLISEC);
						HttpClient httpclient = new DefaultHttpClient(httpParams);

						HttpPost httpost = new HttpPost(getBaseURL(null) + "?format=json&action=fetch");
						httpost.setEntity(new ByteArrayEntity(jRoot.toString().getBytes("UTF-8")));
						httpost.setHeader("Accept", "application/json");
						httpost.setHeader("Content-type", "application/json");
						HttpResponse response = httpclient.execute(httpost);
						StatusLine statusLine = response.getStatusLine();

						if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
							// Succeeded
							ByteArrayOutputStream out = new ByteArrayOutputStream();
							response.getEntity().writeTo(out);
							out.close();

							// Deserialize
							JSONObject status = new JSONObject(out.toString("UTF-8"));
							if (status.getBoolean("ok")) {
								JSONArray settings = status.getJSONArray("settings");
								if (settings.length() > 0) {
									// Delete existing restrictions
									PrivacyManager.deleteRestrictions(appInfo.getUid(), true);

									// Set fetched restrictions
									for (int i = 0; i < settings.length(); i++) {
										JSONObject entry = settings.getJSONObject(i);
										String restrictionName = entry.getString("restriction");
										String methodName = entry.has("method") ? entry.getString("method") : null;
										int voted_restricted = entry.getInt("restricted");
										int voted_not_restricted = entry.getInt("not_restricted");
										boolean restricted = (voted_restricted > voted_not_restricted);
										if (methodName == null || restricted)
											if (methodName == null
													|| PrivacyManager.getMethod(restrictionName, methodName) != null)
												PrivacyManager.setRestricted(null, appInfo.getUid(), restrictionName,
														methodName, restricted, true);
									}
								} else
									publishProgress(getString(R.string.msg_no_restrictions),
											Integer.toString(mProgressCurrent));
							} else
								throw new Exception(status.getString("error"));
						} else {
							// Failed
							response.getEntity().getContent().close();
							throw new IOException(statusLine.getReasonPhrase());
						}
					}
				}
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex.getMessage();
			}
		}

		@Override
		protected void onProgressUpdate(String... values) {
			int progress = 0;
			if (values.length > 1)
				progress = Integer.parseInt(values[1]);
			notify(values[0], true, progress);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(String result) {
			notify(result == null ? getString(R.string.msg_done) : result, false, 0);

			Intent intent = new Intent();
			intent.putExtra(cErrorMessage, result);
			setResult(result == null ? 0 : 1, intent);
			finish();
			super.onPostExecute(result);
		}

		private void notify(String text, boolean ongoing, int progress) {
			// Send progress info to main activity
			Intent progressIntent = new Intent(cProgressReport);
			progressIntent.putExtra(cProgressMessage, String.format("%s: %s", getString(R.string.menu_fetch), text));
			progressIntent.putExtra(cProgressMax, mProgressMax);
			progressIntent.putExtra(cProgressValue, progress);
			mBroadcastManager.sendBroadcast(progressIntent);
		}
	}

	@SuppressLint("DefaultLocale")
	private class SubmitTask extends AsyncTask<int[], String, String> {
		private int mProgressMax;
		private int mProgressCurrent;

		@Override
		protected String doInBackground(int[]... params) {
			try {
				// Get data
				List<ApplicationInfoEx> lstApp = new ArrayList<ApplicationInfoEx>();
				for (int uid : params[0])
					lstApp.add(new ApplicationInfoEx(ActivityShare.this, uid));

				// Initialize progress
				mProgressMax = lstApp.size();
				mProgressCurrent = 0;

				for (ApplicationInfoEx appInfo : lstApp) {
					// Update progess
					mProgressCurrent++;
					publishProgress(appInfo.getPackageName().get(0), Integer.toString(mProgressCurrent));

					// Check if any account allowed
					boolean allowedAccounts = false;
					AccountManager accountManager = AccountManager.get(ActivityShare.this);
					for (Account account : accountManager.getAccounts()) {
						String sha1 = Util.sha1(account.name + account.type);
						boolean allowed = PrivacyManager.getSettingBool(null, appInfo.getUid(),
								PrivacyManager.cSettingAccount + sha1, false, false);
						if (allowed) {
							allowedAccounts = true;
							break;
						}
					}

					// Check if any application allowed
					boolean allowedApplications = false;
					for (ApplicationInfoEx aAppInfo : ApplicationInfoEx.getXApplicationList(ActivityShare.this, null))
						for (String packageName : aAppInfo.getPackageName()) {
							boolean allowed = PrivacyManager.getSettingBool(null, aAppInfo.getUid(),
									PrivacyManager.cSettingApplication + packageName, false, false);
							if (allowed) {
								allowedApplications = true;
								break;
							}
						}

					// Check if any contact allowed
					boolean allowedContacts = false;
					Cursor cursor = getContentResolver().query(ContactsContract.Contacts.CONTENT_URI,
							new String[] { ContactsContract.Contacts._ID }, null, null, null);
					if (cursor != null)
						try {
							while (cursor.moveToNext()) {
								long id = cursor.getLong(cursor.getColumnIndex(ContactsContract.Contacts._ID));
								boolean allowed = PrivacyManager.getSettingBool(null, appInfo.getUid(),
										PrivacyManager.cSettingContact + id, false, false);
								if (allowed) {
									allowedContacts = true;
									break;
								}
							}
						} finally {
							cursor.close();
						}

					// Encode restrictions
					JSONArray jSettings = new JSONArray();
					for (String restrictionName : PrivacyManager.getRestrictions()) {
						boolean restricted = PrivacyManager.getRestricted(null, appInfo.getUid(), restrictionName,
								null, false, false);
						// Category
						long used = PrivacyManager.getUsed(appInfo.getUid(), restrictionName, null);
						JSONObject jRestriction = new JSONObject();
						jRestriction.put("restriction", restrictionName);
						jRestriction.put("restricted", restricted);
						jRestriction.put("used", used);
						if (restrictionName.equals(PrivacyManager.cAccounts))
							jRestriction.put("allowed", allowedAccounts ? 1 : 0);
						else if (restrictionName.equals(PrivacyManager.cSystem))
							jRestriction.put("allowed", allowedApplications ? 1 : 0);
						else if (restrictionName.equals(PrivacyManager.cContacts))
							jRestriction.put("allowed", allowedContacts ? 1 : 0);
						jSettings.put(jRestriction);

						// Methods
						for (PrivacyManager.MethodDescription md : PrivacyManager.getMethods(restrictionName)) {
							boolean mRestricted = restricted
									&& PrivacyManager.getRestricted(null, appInfo.getUid(), restrictionName,
											md.getName(), false, false);
							long mUsed = PrivacyManager.getUsed(appInfo.getUid(), restrictionName, md.getName());
							JSONObject jMethod = new JSONObject();
							jMethod.put("restriction", restrictionName);
							jMethod.put("method", md.getName());
							jMethod.put("restricted", mRestricted);
							jMethod.put("used", mUsed);
							jSettings.put(jMethod);
						}
					}

					// Get data
					String[] license = Util.getProLicenseUnchecked();
					PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
					String android_id = Secure.getString(ActivityShare.this.getContentResolver(), Secure.ANDROID_ID);

					JSONArray appName = new JSONArray();
					for (String name : appInfo.getApplicationName())
						appName.put(name);

					JSONArray pkgName = new JSONArray();
					for (String name : appInfo.getPackageName())
						pkgName.put(name);

					JSONArray pkgVersionName = new JSONArray();
					for (String version : appInfo.getPackageVersionName(ActivityShare.this))
						pkgVersionName.put(version);

					JSONArray pkgVersionCode = new JSONArray();
					for (Integer version : appInfo.getPackageVersionCode(ActivityShare.this))
						pkgVersionCode.put((int) version);

					// Encode package
					JSONObject jRoot = new JSONObject();
					jRoot.put("protocol_version", cProtocolVersion);
					jRoot.put("android_id", Util.md5(android_id).toLowerCase());
					jRoot.put("android_sdk", Build.VERSION.SDK_INT);
					jRoot.put("xprivacy_version", pInfo.versionCode);
					jRoot.put("application_name", appName);
					jRoot.put("package_name", pkgName);
					jRoot.put("package_version_name", pkgVersionName);
					jRoot.put("package_version_code", pkgVersionCode);
					jRoot.put("settings", jSettings);
					if (license != null) {
						jRoot.put("email", license[1]);
						jRoot.put("signature", license[2]);
					}

					// Submit
					HttpParams httpParams = new BasicHttpParams();
					HttpConnectionParams.setConnectionTimeout(httpParams, ActivityShare.TIMEOUT_MILLISEC);
					HttpConnectionParams.setSoTimeout(httpParams, ActivityShare.TIMEOUT_MILLISEC);
					HttpClient httpclient = new DefaultHttpClient(httpParams);

					HttpPost httpost = new HttpPost(getBaseURL(null) + "?format=json&action=submit");
					httpost.setEntity(new ByteArrayEntity(jRoot.toString().getBytes("UTF-8")));
					httpost.setHeader("Accept", "application/json");
					httpost.setHeader("Content-type", "application/json");
					HttpResponse response = httpclient.execute(httpost);
					StatusLine statusLine = response.getStatusLine();

					if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
						// Succeeded
						ByteArrayOutputStream out = new ByteArrayOutputStream();
						response.getEntity().writeTo(out);
						out.close();
						JSONObject status = new JSONObject(out.toString("UTF-8"));
						if (status.getBoolean("ok")) {
							// Mark as shared
							PrivacyManager.setSetting(null, appInfo.getUid(), PrivacyManager.cSettingState,
									Integer.toString(ActivityMain.STATE_SHARED));
						} else {
							// Mark as unregistered
							PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingRegistered,
									Boolean.toString(false));
							throw new Exception(status.getString("error"));
						}
					} else {
						// Failed
						response.getEntity().getContent().close();
						throw new IOException(statusLine.getReasonPhrase());
					}
				}
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex.getMessage();
			}
		}

		@Override
		protected void onProgressUpdate(String... values) {
			int progress = 0;
			if (values.length > 1)
				progress = Integer.parseInt(values[1]);
			notify(values[0], true, progress);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(String result) {
			notify(result == null ? getString(R.string.msg_done) : result, false, 0);

			Intent intent = new Intent();
			intent.putExtra(cErrorMessage, result);
			setResult(result == null ? 0 : 1, intent);
			finish();
			super.onPostExecute(result);
		}

		private void notify(String text, boolean ongoing, int progress) {
			// Send progress info to main activity
			Intent progressIntent = new Intent(cProgressReport);
			progressIntent.putExtra(cProgressMessage, String.format("%s: %s", getString(R.string.menu_submit), text));
			progressIntent.putExtra(cProgressMax, mProgressMax);
			progressIntent.putExtra(cProgressValue, progress);
			mBroadcastManager.sendBroadcast(progressIntent);
		}
	}

	public static boolean registerDevice(final Context context) {
		if (Util.hasProLicense(context) == null
				&& !PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingRegistered, false, false)) {
			// Get accounts
			final List<Account> listAccount = new ArrayList<Account>();
			List<CharSequence> listName = new ArrayList<CharSequence>();
			for (Account account : AccountManager.get(context).getAccounts())
				if ("com.google".equals(account.type)) {
					listAccount.add(account);
					listName.add(String.format("%s (%s)", account.name, account.type));
				}

			// Build dialog
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
			alertDialogBuilder.setTitle(context.getString(R.string.msg_register));
			alertDialogBuilder.setIcon(Util.getThemed(context, R.attr.icon_launcher));
			alertDialogBuilder.setSingleChoiceItems(listName.toArray(new CharSequence[0]), -1,
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							Account account = listAccount.get(which);
							new RegisterTask(context).executeOnExecutor(mExecutor, account.name);
						}
					});
			alertDialogBuilder.setPositiveButton(context.getString(R.string.msg_done),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							// Do nothing
						}
					});

			// Show dialog
			AlertDialog alertDialog = alertDialogBuilder.create();
			alertDialog.show();

			return false;
		}
		return true;
	}

	@SuppressLint("DefaultLocale")
	private static class RegisterTask extends AsyncTask<String, String, String> {
		private Context mContext;

		public RegisterTask(Context context) {
			mContext = context;
		}

		protected String doInBackground(String... params) {
			try {
				String android_id = Secure.getString(mContext.getContentResolver(), Secure.ANDROID_ID);

				// Encode message
				JSONObject jRoot = new JSONObject();
				jRoot.put("protocol_version", cProtocolVersion);
				jRoot.put("email", params[0]);
				jRoot.put("android_id", Util.md5(android_id).toLowerCase());

				// Submit
				HttpParams httpParams = new BasicHttpParams();
				HttpConnectionParams.setConnectionTimeout(httpParams, ActivityShare.TIMEOUT_MILLISEC);
				HttpConnectionParams.setSoTimeout(httpParams, ActivityShare.TIMEOUT_MILLISEC);
				HttpClient httpclient = new DefaultHttpClient(httpParams);

				HttpPost httpost = new HttpPost(getBaseURL(null) + "device?format=json&action=register");
				httpost.setEntity(new ByteArrayEntity(jRoot.toString().getBytes("UTF-8")));
				httpost.setHeader("Accept", "application/json");
				httpost.setHeader("Content-type", "application/json");
				HttpResponse response = httpclient.execute(httpost);
				StatusLine statusLine = response.getStatusLine();

				if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
					// Succeeded
					ByteArrayOutputStream out = new ByteArrayOutputStream();
					response.getEntity().writeTo(out);
					out.close();
					JSONObject status = new JSONObject(out.toString("UTF-8"));
					if (status.getBoolean("ok")) {
						// Mark as registered
						PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingRegistered, Boolean.toString(true));
						return null;
					} else
						throw new Exception(status.getString("error"));
				} else {
					// Failed
					response.getEntity().getContent().close();
					throw new IOException(statusLine.getReasonPhrase());
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex.getMessage();
			}
		}

		@Override
		protected void onPostExecute(String result) {
			String message = (result == null ? mContext.getString(R.string.msg_registered) : result);
			Toast toast = Toast.makeText(mContext, message, Toast.LENGTH_LONG);
			toast.show();
		}
	}

	// Helper methods

	public static String getBaseURL(Context context) {
		if (PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingHttps, true, true))
			return HTTPS_BASE_URL;
		else
			return HTTP_BASE_URL;
	}

	public static String getFileName(boolean multiple) {
		File folder = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator
				+ ".xprivacy");
		folder.mkdir();
		String fileName;
		if (multiple) {
			SimpleDateFormat format = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.ROOT);
			fileName = String.format("XPrivacy_%s.xml", format.format(new Date()));
		} else
			fileName = "XPrivacy.xml";
		return new File(folder + File.separator + fileName).getAbsolutePath();
	}
}

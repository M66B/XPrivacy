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

import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.provider.Settings.Secure;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;
import android.util.SparseArray;
import android.util.Xml;

public class ActivityShare extends Activity {
	private LocalBroadcastManager mBroadcastManager;

	public static final String cFileName = "FileName";
	public static final String cUid = "Uid";
	public static final String cErrorMessage = "ErrorMessage";
	public static final String BASE_URL = "http://updates.faircode.eu/test.php";
	public static final String cProgressReport = "ProgressReport";
	public static final String cProgressMessage = "ProgressMessage";
	public static final String cProgressValue = "ProgressValue";
	public static final String cProgressMax = "ProgressMax";

	public static final String ACTION_EXPORT = "biz.bokhorst.xprivacy.action.EXPORT";
	public static final String ACTION_IMPORT = "biz.bokhorst.xprivacy.action.IMPORT";
	public static final String ACTION_FETCH = "biz.bokhorst.xprivacy.action.FETCH";

	public static final int TIMEOUT_MILLISEC = 30000;

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

		if (Util.hasProLicense(this) != null) {
			Bundle extras = getIntent().getExtras();

			mBroadcastManager = LocalBroadcastManager.getInstance(this);

			// Import
			if (getIntent().getAction().equals(ACTION_IMPORT)) {
				String fileName = (extras.containsKey(cFileName) ? extras.getString(cFileName) : getFileName(false));
				ImportTask importTask = new ImportTask();
				importTask.executeOnExecutor(mExecutor, new File(fileName));
			}

			// Export
			if (getIntent().getAction().equals(ACTION_EXPORT)) {
				String fileName = (extras.containsKey(cFileName) ? extras.getString(cFileName) : getFileName(false));
				ExportTask exportTask = new ExportTask();
				exportTask.executeOnExecutor(mExecutor, new File(fileName));
			}

			// Fetch
			if (getIntent().getAction().equals(ACTION_FETCH)) {
				int uid = 0;
				if (extras != null && extras.containsKey(cUid))
					uid = extras.getInt(cUid);
				FetchTask fetchTask = new FetchTask();
				fetchTask.executeOnExecutor(mExecutor, uid);
			}
		}
	}

	// Tasks

	private class ExportTask extends AsyncTask<File, String, String> {
		private File mFile;
		private int mProgressCurrent = 0;

		@Override
		protected String doInBackground(File... params) {
			try {
				// Serialize
				mFile = params[0];
				Util.log(null, Log.INFO, "Exporting " + mFile);

				FileOutputStream fos = new FileOutputStream(mFile);
				try {
					XmlSerializer serializer = Xml.newSerializer();
					serializer.setOutput(fos, "UTF-8");
					serializer.startDocument(null, Boolean.valueOf(true));
					serializer.setFeature("http://xmlpull.org/v1/doc/features.html#indent-output", true);
					serializer.startTag(null, "XPrivacy");

					// Progress
					publishProgress(getString(R.string.msg_loading));
					Util.log(null, Log.INFO, "Exporting settings");
					Runnable progress = new Runnable() {
						@Override
						public void run() {
							// This should be called exactly 100 times
							publishProgress(getString(R.string.msg_loading), Integer.toString(++mProgressCurrent),
									"100");
						}
					};

					// Process packages
					for (PackageInfo pInfo : getPackageManager().getInstalledPackages(0)) {
						serializer.startTag(null, "PackageInfo");
						serializer.attribute(null, "Id", Integer.toString(pInfo.applicationInfo.uid));
						serializer.attribute(null, "Name", pInfo.packageName);
						serializer.endTag(null, "PackageInfo");
					}

					// Process settings
					String android_id = Secure.getString(getContentResolver(), Secure.ANDROID_ID);
					Map<String, String> mapSetting = PrivacyManager.getSettings(ActivityShare.this, progress);
					for (String name : mapSetting.keySet()) {
						// Decode name
						String[] component = name.split("\\.");

						// Template
						if (component[0].equals(PrivacyManager.cSettingTemplate))
							component = new String[] { name };

						// Get id
						int id = -1;
						try {
							if (component.length == 2)
								id = Integer.parseInt(component[1]);
						} catch (NumberFormatException ex) {
							Util.bug(null, ex);
						}

						if ((component.length == 2 && id < 0) || component.length > 2)
							Util.log(null, Log.WARN, "Legacy name=" + name + " value=" + mapSetting.get(name));
						else {
							// Bind accounts/contacts to same device
							if (component[0].startsWith("Account.") || component[0].startsWith("Contact.")
									|| component[0].startsWith("RawContact.")) {
								component[0] += "." + android_id;
							}

							// Serialize setting
							serializer.startTag(null, "Setting");
							serializer.attribute(null, "Id", id >= 0 ? component[1] : "");
							serializer.attribute(null, "Name", component[0]);
							serializer.attribute(null, "Value", mapSetting.get(name));
							serializer.endTag(null, "Setting");
						}
					}

					// Process restrictions
					List<PrivacyManager.RestrictionDesc> listRestriction = PrivacyManager.getRestricted(
							ActivityShare.this, progress);

					mProgressCurrent = 0;
					for (PrivacyManager.RestrictionDesc restrictionDesc : listRestriction) {
						if ((++mProgressCurrent % (listRestriction.size() / 10 + 1)) == 0)
							publishProgress(getString(R.string.menu_export), Integer.toString(mProgressCurrent),
									Integer.toString(listRestriction.size()));
						serializer.startTag(null, "Restriction");
						serializer.attribute(null, "Id", Integer.toString(restrictionDesc.uid));
						serializer.attribute(null, "Name", restrictionDesc.restrictionName);
						if (restrictionDesc.methodName != null)
							serializer.attribute(null, "Method", restrictionDesc.methodName);
						serializer.attribute(null, "Restricted", Boolean.toString(restrictionDesc.restricted));
						serializer.endTag(null, "Restriction");
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

	private class ImportTask extends AsyncTask<File, String, String> {
		private File mFile;
		private int mProgressMax;
		private int mProgressCurrent;

		@Override
		protected String doInBackground(File... params) {
			try {
				mFile = params[0];

				// Parse XML
				Util.log(null, Log.INFO, "Importing " + mFile);
				FileInputStream fis = null;
				Map<String, Map<String, List<ImportHandler.MethodDescription>>> mapPackage;
				try {
					fis = new FileInputStream(mFile);
					XMLReader xmlReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
					ImportHandler importHandler = new ImportHandler();
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
						Util.log(null, Log.INFO, "Importing " + packageName);

						// Get uid
						int uid = getPackageManager().getPackageInfo(packageName, 0).applicationInfo.uid;

						// Reset existing restrictions
						PrivacyManager.deleteRestrictions(ActivityShare.this, uid);

						// Set imported restrictions
						for (String restrictionName : mapPackage.get(packageName).keySet()) {
							PrivacyManager.setRestricted(null, ActivityShare.this, uid, restrictionName, null, true);
							for (ImportHandler.MethodDescription md : mapPackage.get(packageName).get(restrictionName))
								PrivacyManager.setRestricted(null, ActivityShare.this, uid, restrictionName,
										md.getMethodName(), md.isRestricted());
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
		private SparseArray<String> mMapId = new SparseArray<String>();
		private Map<String, Integer> mMapUid = new HashMap<String, Integer>();
		private Map<String, Map<String, List<MethodDescription>>> mMapPackage = new HashMap<String, Map<String, List<MethodDescription>>>();
		private String android_id = Secure.getString(getContentResolver(), Secure.ANDROID_ID);
		private int mProgress = 0;
		private int mCurrent = 0;

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			try {
				if (qName.equals("XPrivacy")) {
					// Ignore
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
					if (name.startsWith("Account.") || name.startsWith("Contact.") || name.startsWith("RawContact."))
						if (name.endsWith("." + android_id))
							name = name.replace("." + android_id, "");
						else
							return;

					if (id == null) { // Legacy
						Util.log(null, Log.WARN, "Legacy " + name + "=" + value);
						PrivacyManager.setSetting(null, ActivityShare.this, 0, name, value);
					} else if ("".equals(id)) // Global setting
						PrivacyManager.setSetting(null, ActivityShare.this, 0, name, value);
					else { // Application setting
						int uid = getUid(Integer.parseInt(id));
						if (uid >= 0)
							PrivacyManager.setSetting(null, ActivityShare.this, uid, name, value);
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

					// Progress report
					if (id != mCurrent) {
						mCurrent = id;
						reportProgress();
					}

					// Get uid
					int uid = getUid(id);
					if (uid >= 0)
						PrivacyManager.setRestricted(null, ActivityShare.this, uid, restrictionName, methodName,
								restricted);
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

		private void reportProgress() {
			// Send progress info to main activity
			Intent progressIntent = new Intent(cProgressReport);
			progressIntent.putExtra(cProgressMessage,
					String.format("%s: %s", getString(R.string.menu_import), mMapId.get(mCurrent)));
			progressIntent.putExtra(cProgressMax, mMapId.size());
			progressIntent.putExtra(cProgressValue, ++mProgress);
			mBroadcastManager.sendBroadcast(progressIntent);
		}
	}

	private class FetchTask extends AsyncTask<Integer, String, String> {
		private int mProgressMax;
		private int mProgressCurrent;

		@Override
		protected String doInBackground(Integer... params) {
			try {
				// Get data
				List<ApplicationInfoEx> lstApp;
				if (params[0] == 0)
					lstApp = ApplicationInfoEx.getXApplicationList(ActivityShare.this, null);
				else {
					lstApp = new ArrayList<ApplicationInfoEx>();
					lstApp.add(new ApplicationInfoEx(ActivityShare.this, params[0]));
				}
				String android_id = Secure.getString(ActivityShare.this.getContentResolver(), Secure.ANDROID_ID);
				String[] license = Util.getProLicense();
				PackageInfo pXPrivacyInfo = getPackageManager().getPackageInfo(getPackageName(), 0);

				String confidence = PrivacyManager.getSetting(null, ActivityShare.this, 0,
						PrivacyManager.cSettingConfidence, "", false);

				// Set some numbers for the progress bar
				mProgressMax = lstApp.size();
				mProgressCurrent = 0;

				// Process applications
				for (ApplicationInfoEx appInfo : lstApp) {
					mProgressCurrent++;
					if (!appInfo.isSystem() || params[0] != null) {
						publishProgress(appInfo.getPackageName().get(0), Integer.toString(mProgressCurrent));

						JSONArray appName = new JSONArray();
						for (String name : appInfo.getApplicationName())
							appName.put(name);

						JSONArray pkgName = new JSONArray();
						for (String name : appInfo.getPackageName())
							pkgName.put(name);

						JSONArray pkgVersion = new JSONArray();
						for (String version : appInfo.getPackageVersion(ActivityShare.this))
							pkgVersion.put(version);

						// Encode package
						JSONObject jRoot = new JSONObject();
						jRoot.put("protocol_version", 3);
						jRoot.put("android_id", android_id);
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

						HttpPost httpost = new HttpPost(BASE_URL + "?format=json&action=fetch");
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
									PrivacyManager.deleteRestrictions(ActivityShare.this, appInfo.getUid());

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
												PrivacyManager.setRestricted(null, ActivityShare.this,
														appInfo.getUid(), restrictionName, methodName, restricted);
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

	// Helper methods

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

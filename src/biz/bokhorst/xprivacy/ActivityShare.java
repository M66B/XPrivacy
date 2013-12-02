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
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.provider.Settings.Secure;
import android.support.v4.app.NotificationCompat;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;
import android.util.Xml;
import android.widget.Toast;

public class ActivityShare extends Activity {
	private LocalBroadcastManager mBroadcastManager;

	public static final String cFileName = "FileName";
	public static final String cPackageName = "PackageName";
	public static final String BASE_URL = "http://updates.faircode.eu/xprivacy";
	public static final String cProgressReport = "ProgressReport";
	public static final String cProgressMessage = "ProgressMessage";
	public static final String cProgressValue = "ProgressValue";
	public static final String cProgressMax = "ProgressMax";

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
			if (getIntent().getAction().equals("biz.bokhorst.xprivacy.action.IMPORT")) {
				String fileName = (extras.containsKey(cFileName) ? extras.getString(cFileName) : getFileName(false));
				ImportTask importTask = new ImportTask();
				importTask.executeOnExecutor(mExecutor, new File(fileName));
			}

			// Export
			if (getIntent().getAction().equals("biz.bokhorst.xprivacy.action.EXPORT")) {
				String fileName = (extras.containsKey(cFileName) ? extras.getString(cFileName) : getFileName(false));
				ExportTask exportTask = new ExportTask();
				exportTask.executeOnExecutor(mExecutor, new File(fileName));
			}

			// Fetch
			if (getIntent().getAction().equals("biz.bokhorst.xprivacy.action.FETCH")) {
				String packageName = null;
				if (extras != null && extras.containsKey(cPackageName))
					packageName = extras.getString(cPackageName);
				FetchTask fetchTask = new FetchTask();
				fetchTask.executeOnExecutor(mExecutor, packageName);
			}
		}
	}

	// Tasks

	private class ExportTask extends AsyncTask<File, String, String> {
		private File mFile;
		private int mCurrent = 0;
		private final static int NOTIFY_ID = 1;

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

					// Process settings
					publishProgress(getString(R.string.msg_loading));
					Util.log(null, Log.INFO, "Exporting settings");

					// Progress updater
					Runnable progress = new Runnable() {
						@Override
						public void run() {
							// This should be called exactly 100 times
							mCurrent++;
							publishProgress(getString(R.string.msg_loading), Integer.toString(mCurrent), "100");
						}
					};

					String android_id = Secure.getString(getContentResolver(), Secure.ANDROID_ID);
					Map<String, String> mapSetting = PrivacyManager.getSettings(ActivityShare.this, progress);
					for (String setting : mapSetting.keySet()) {
						String value = mapSetting.get(setting);

						// Bound accounts/contacts to same device
						if (setting.startsWith("Account.") || setting.startsWith("Contact.")
								|| setting.startsWith("RawContact.")) {
							setting += "." + android_id;
						}

						// Serialize setting
						serializer.startTag(null, "Setting");
						serializer.attribute(null, "Name", setting);
						serializer.attribute(null, "Value", value);
						serializer.endTag(null, "Setting");
					}

					// Process restrictions
					List<PrivacyManager.RestrictionDesc> listRestriction = PrivacyManager.getRestricted(
							ActivityShare.this, progress);
					Map<String, List<PrivacyManager.RestrictionDesc>> mapRestriction = new HashMap<String, List<PrivacyManager.RestrictionDesc>>();
					for (PrivacyManager.RestrictionDesc restriction : listRestriction) {
						String[] packages = getPackageManager().getPackagesForUid(restriction.uid);
						if (packages == null)
							Util.log(null, Log.WARN, "No packages for uid=" + restriction.uid);
						else
							for (String packageName : packages) {
								if (!mapRestriction.containsKey(packageName))
									mapRestriction.put(packageName, new ArrayList<PrivacyManager.RestrictionDesc>());
								mapRestriction.get(packageName).add(restriction);
							}
					}

					// Set some numbers for the progress bar
					final String max = Integer.toString(mapRestriction.size());
					mCurrent = 0;

					// Process result
					for (String packageName : mapRestriction.keySet()) {
						mCurrent++;
						publishProgress(packageName, Integer.toString(mCurrent), max);
						Util.log(null, Log.INFO, "Exporting " + packageName);
						for (PrivacyManager.RestrictionDesc restrictionDesc : mapRestriction.get(packageName)) {
							serializer.startTag(null, "Package");
							serializer.attribute(null, "Name", packageName);
							serializer.attribute(null, "Restriction", restrictionDesc.restrictionName);
							if (restrictionDesc.methodName != null)
								serializer.attribute(null, "Method", restrictionDesc.methodName);
							serializer.attribute(null, "Restricted", Boolean.toString(restrictionDesc.restricted));
							serializer.endTag(null, "Package");
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
				return getString(R.string.msg_done);
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
			notify(result, false, 0, 1);
			Intent intent = new Intent();
			intent.putExtra(cFileName, mFile.getAbsolutePath());
			setResult(result.equals(getString(R.string.msg_done)) ? 0 : 1, intent);
			finish();

			Toast toast = Toast.makeText(ActivityShare.this, mFile.getAbsolutePath(), Toast.LENGTH_LONG);
			toast.show();
			super.onPostExecute(result);
		}

		private void notify(String text, boolean ongoing, int progress, int max) {
			// Send progress info to main activity
			Intent progressIntent = new Intent(cProgressReport);
			progressIntent.putExtra(cProgressMessage, String.format("%s: %s", getString(R.string.menu_export), text));
			progressIntent.putExtra(cProgressMax, max);
			progressIntent.putExtra(cProgressValue, progress);
			mBroadcastManager.sendBroadcast(progressIntent);

			// Create/update the progress notification
			NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(ActivityShare.this);
			notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
			notificationBuilder.setContentTitle(getString(R.string.menu_export));
			notificationBuilder.setContentText(text);
			notificationBuilder.setWhen(System.currentTimeMillis());
			if (ongoing)
				notificationBuilder.setOngoing(true);
			else {
				// Build result intent
				Intent resultIntent = new Intent(ActivityShare.this, ActivityMain.class);
				resultIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

				// Build pending intent
				PendingIntent pendingIntent = PendingIntent.getActivity(ActivityShare.this, NOTIFY_ID, resultIntent,
						PendingIntent.FLAG_UPDATE_CURRENT);

				notificationBuilder.setAutoCancel(true);
				notificationBuilder.setContentIntent(pendingIntent);
			}
			Notification notification = notificationBuilder.build();

			NotificationManager notificationManager = (NotificationManager) ActivityShare.this
					.getSystemService(Context.NOTIFICATION_SERVICE);
			notificationManager.notify(NOTIFY_ID, notification);
		}
	}

	private class ImportTask extends AsyncTask<File, String, String> {
		private File mFile;
		private int mProgressMax;
		private int mCurrent;
		private final static int NOTIFY_ID = 2;

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

				// Set some numbers for the progress bar
				mProgressMax = mapPackage.size();
				mCurrent = 0;

				// Process result
				for (String packageName : mapPackage.keySet()) {
					mCurrent++;
					try {
						publishProgress(packageName, Integer.toString(mCurrent));
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
				return getString(R.string.msg_done);
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
			notify(result, false, 0);
			Intent intent = new Intent();
			intent.putExtra(cFileName, mFile.getAbsolutePath());
			setResult(result.equals(getString(R.string.msg_done)) ? 0 : 1, intent);
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

			// Create/update the progress notification
			NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(ActivityShare.this);
			notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
			notificationBuilder.setContentTitle(getString(R.string.menu_import));
			notificationBuilder.setContentText(text);
			notificationBuilder.setWhen(System.currentTimeMillis());
			if (ongoing)
				notificationBuilder.setOngoing(true);
			else {
				// Build result intent
				Intent resultIntent = new Intent(ActivityShare.this, ActivityMain.class);
				resultIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

				// Build pending intent
				PendingIntent pendingIntent = PendingIntent.getActivity(ActivityShare.this, NOTIFY_ID, resultIntent,
						PendingIntent.FLAG_UPDATE_CURRENT);

				notificationBuilder.setAutoCancel(true);
				notificationBuilder.setContentIntent(pendingIntent);
			}
			Notification notification = notificationBuilder.build();

			NotificationManager notificationManager = (NotificationManager) ActivityShare.this
					.getSystemService(Context.NOTIFICATION_SERVICE);
			notificationManager.notify(NOTIFY_ID, notification);
		}
	}

	private class ImportHandler extends DefaultHandler {
		private Map<String, Map<String, List<MethodDescription>>> mMapPackage = new HashMap<String, Map<String, List<MethodDescription>>>();
		private String android_id = Secure.getString(getContentResolver(), Secure.ANDROID_ID);

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if (qName.equals("Setting")) {
				// Setting
				String setting = attributes.getValue("Name");
				String value = attributes.getValue("Value");

				// Import accounts/contacts only for same device
				if (setting.startsWith("Account.") || setting.startsWith("Contact.")
						|| setting.startsWith("RawContact."))
					if (setting.endsWith("." + android_id))
						setting = setting.replace("." + android_id, "");
					else
						return;

				PrivacyManager.setSetting(null, ActivityShare.this, 0, setting, value);
			} else if (qName.equals("Package")) {
				// Restriction
				String packageName = attributes.getValue("Name");
				String restrictionName = attributes.getValue("Restriction");
				String methodName = attributes.getValue("Method");
				boolean restricted = Boolean.parseBoolean(attributes.getValue("Restricted"));

				// Map package restriction
				if (!mMapPackage.containsKey(packageName))
					mMapPackage.put(packageName, new HashMap<String, List<MethodDescription>>());
				if (!mMapPackage.get(packageName).containsKey(restrictionName))
					mMapPackage.get(packageName).put(restrictionName, new ArrayList<MethodDescription>());
				if (methodName != null) {
					MethodDescription md = new MethodDescription(methodName, restricted);
					mMapPackage.get(packageName).get(restrictionName).add(md);
				}
			}
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
	}

	private class FetchTask extends AsyncTask<String, String, Object> {
		private final static int NOTIFY_ID = 3;
		private int mProgressMax;
		private int mCurrent;

		@Override
		protected Object doInBackground(String... params) {
			try {
				// Get data
				List<ApplicationInfoEx> lstApp;
				if (params[0] == null)
					lstApp = ApplicationInfoEx.getXApplicationList(ActivityShare.this, null);
				else {
					lstApp = new ArrayList<ApplicationInfoEx>();
					lstApp.add(new ApplicationInfoEx(ActivityShare.this, params[0]));
				}
				String android_id = Secure.getString(ActivityShare.this.getContentResolver(), Secure.ANDROID_ID);
				String[] license = Util.getProLicense();
				PackageInfo pXPrivacyInfo = getPackageManager().getPackageInfo(getPackageName(), 0);

				// Set some numbers for the progress bar
				mProgressMax = lstApp.size();
				mCurrent = 0;

				// Process applications
				for (ApplicationInfoEx appInfo : lstApp) {
					mCurrent++;
					if (!appInfo.isSystem() || params[0] != null) {
						publishProgress(appInfo.getPackageName(), Integer.toString(mCurrent));

						// Encode package
						JSONObject jRoot = new JSONObject();
						jRoot.put("protocol_version", 3);
						jRoot.put("android_id", android_id);
						jRoot.put("android_sdk", Build.VERSION.SDK_INT);
						jRoot.put("xprivacy_version", pXPrivacyInfo.versionCode);
						jRoot.put("application_name", appInfo.getFirstApplicationName());
						jRoot.put("package_name", appInfo.getPackageName());
						jRoot.put("package_version", appInfo.getVersion(ActivityShare.this));
						jRoot.put("email", license[1]);
						jRoot.put("signature", license[2]);

						// Fetch
						int TIMEOUT_MILLISEC = 45000; // 45 seconds
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
											PrivacyManager.setRestricted(null, ActivityShare.this, appInfo.getUid(),
													restrictionName, methodName, restricted);
									}
								} else
									publishProgress(getString(R.string.msg_no_restrictions), Integer.toString(mCurrent));
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
				return ex;
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
		protected void onPostExecute(Object result) {
			notify(result == null ? getString(R.string.msg_done) : ((Throwable) result).getMessage(), false, 0);
			Intent intent = new Intent();
			setResult(0, intent);
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

			// Create/update the progress notification
			NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(ActivityShare.this);
			notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
			notificationBuilder.setContentTitle(getString(R.string.menu_fetch));
			notificationBuilder.setContentText(text);
			notificationBuilder.setWhen(System.currentTimeMillis());
			if (ongoing)
				notificationBuilder.setOngoing(true);
			else {
				// Build result intent
				Intent resultIntent = new Intent(ActivityShare.this, ActivityMain.class);
				resultIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

				// Build pending intent
				PendingIntent pendingIntent = PendingIntent.getActivity(ActivityShare.this, NOTIFY_ID, resultIntent,
						PendingIntent.FLAG_UPDATE_CURRENT);

				notificationBuilder.setAutoCancel(true);
				notificationBuilder.setContentIntent(pendingIntent);
			}
			Notification notification = notificationBuilder.build();

			NotificationManager notificationManager = (NotificationManager) ActivityShare.this
					.getSystemService(Context.NOTIFICATION_SERVICE);
			notificationManager.notify(NOTIFY_ID, notification);
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

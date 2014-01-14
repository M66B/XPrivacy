package biz.bokhorst.xprivacy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.Collator;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
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
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.json.JSONArray;
import org.json.JSONException;
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
import android.app.ProgressDialog;
import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.database.Cursor;
import android.graphics.Color;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.provider.ContactsContract;
import android.provider.Settings.Secure;
import android.text.TextUtils;
import android.util.Log;
import android.util.SparseArray;
import android.util.Xml;
import android.view.ContextMenu;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

public class ActivityShare extends Activity {
	private int mThemeId;
	private AppListAdapter mAppAdapter;
	private SparseArray<AppHolder> mAppsByUid;
	private boolean mRunning = false;
	private boolean mAbort = false;
	private int mProgressCurrent;
	private int mProgressWidth = 0;

	private static final int STATE_WAITING = 0;
	private static final int STATE_RUNNING = 1;
	private static final int STATE_SUCCESS = 2;
	private static final int STATE_FAILURE = 3;

	public static final String cFileName = "FileName";
	public static final String cUidList = "UidList";
	public static final String cInteractive = "Interactive";
	public static final String cErrorMessage = "ErrorMessage";
	public static final String BASE_URL = "https://crowd.xprivacy.eu/";
	public static final String cProgressReport = "ProgressReport";
	public static final String cProgressMessage = "ProgressMessage";
	public static final String cProgressValue = "ProgressValue";
	public static final String cProgressMax = "ProgressMax";

	public static final int cSubmitLimit = 2;

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

		final Bundle extras = getIntent().getExtras();
		final int[] uids = (extras != null && extras.containsKey(cUidList) ? extras.getIntArray(cUidList) : new int[0]);

		final String action = getIntent().getAction();

		// Check whether we need a ui, if not, leave the theme declared in the manifest
		if (extras.containsKey(cInteractive) && extras.getBoolean(cInteractive, false)) {
			// Set theme
			String themeName = PrivacyManager.getSetting(null, this, 0, PrivacyManager.cSettingTheme, "", false);
			mThemeId = (themeName.equals("Dark") ? R.style.CustomTheme : R.style.CustomTheme_Light);
			setTheme(mThemeId);

			// Set layout and title
			setContentView(R.layout.sharelist);
			getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_HIDDEN);

			if (action.equals(ACTION_IMPORT))
				setTitle(getString(R.string.menu_import));
			else if (action.equals(ACTION_EXPORT))
				setTitle(getString(R.string.menu_export));
			else if (action.equals(ACTION_FETCH))
				setTitle(getString(R.string.menu_fetch));
			else if (action.equals(ACTION_SUBMIT))
				setTitle(getString(R.string.menu_submit));

			if (action.equals(ACTION_IMPORT) || action.equals(ACTION_FETCH)) {
				if (Util.hasProLicense(this) == null)
					finish();
			}

			final String fileName = (extras != null && extras.containsKey(cFileName) ? extras.getString(cFileName)
					: getFileName(false));
			// TODO if there is a file chooser, launch it
			// TODO add result receiver

			// Start task to get app list, but not if action is EXPORT
			if (!action.equals(ACTION_EXPORT)) {
				AppListTask appListTask = new AppListTask();
				appListTask.executeOnExecutor(mExecutor, uids);
			}

			TextView tvDescription = (TextView) findViewById(R.id.tvDescription);
			View llDescription = findViewById(R.id.llDescription);
			if (action.equals(ACTION_EXPORT)) {
				tvDescription.setText("Backup all settings to " + fileName);
				llDescription.setVisibility(View.VISIBLE);
			} else if (action.equals(ACTION_IMPORT)) {
				tvDescription.setText("Import settings from " + fileName);
				llDescription.setVisibility(View.VISIBLE);
				//if (file picker available) {
					Button btnChange = (Button) findViewById(R.id.btnChange);
					btnChange.setVisibility(View.VISIBLE);
					// TODO onclick listener to launch file picker
				//}
			}

			// Set button actions
			final Button btnOk = (Button) findViewById(R.id.btnOk);
			final Button btnCancel = (Button) findViewById(R.id.btnCancel);
			
			btnOk.setOnClickListener(new Button.OnClickListener(){

				@Override
				public void onClick(View v) {
					btnOk.setEnabled(false);

					// If action is EXPORT mAppAdapter hasn't been initialised
					int[] uids = mAppAdapter == null ? new int[0] : mAppAdapter.getUids();

					// Import
					if (action.equals(ACTION_IMPORT)) {
						new ImportTask().executeOnExecutor(mExecutor, new File(fileName), uids);
					}

					// Export
					else if (action.equals(ACTION_EXPORT)) {
						new ExportTask().executeOnExecutor(mExecutor, new File(fileName));
					}

					// Fetch
					else if (action.equals(ACTION_FETCH)) {
						if (uids.length > 0) {
							new FetchTask().executeOnExecutor(mExecutor, uids);
						}
					}

					// Submit
					else if (action.equals(ACTION_SUBMIT)) {
						if (uids.length > 0) {
							if (uids.length <= cSubmitLimit) {
								new SubmitTask().executeOnExecutor(mExecutor, uids);
							} else {
								String message = getString(R.string.msg_limit, ActivityShare.cSubmitLimit + 1);
								Toast.makeText(ActivityShare.this, message, Toast.LENGTH_SHORT).show();
								btnOk.setEnabled(false);
							}
						}
					}

					// Unknown action
					else {
						Util.log(null, Log.WARN, "Unknown share action: " + action);
					}
				}
			});

			btnCancel.setOnClickListener(new Button.OnClickListener(){
				@Override
				public void onClick(View v) {
					if (mRunning) {
						mAbort = true;
						Toast.makeText(ActivityShare.this, "Aborting", Toast.LENGTH_SHORT).show(); // TODO string resource
					} else {
						finish();
					}
				}
			});

			// Allow users to remove apps from list
			ListView lvShare = (ListView) findViewById(R.id.lvShare);
			registerForContextMenu(lvShare);

		} else if (action.equals(ACTION_EXPORT)) {
			// Set theme to NoDisplay
			setTheme(android.R.style.Theme_NoDisplay);
			// Get on with exporting
			String fileName = (extras != null && extras.containsKey(cFileName) ? extras.getString(cFileName)
					: getFileName(false));
			new ExportTask().executeOnExecutor(mExecutor, new File(fileName));
			setTitle(getString(R.string.menu_export));
		}
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
		super.onCreateContextMenu(menu, v, menuInfo);

		if (v.getId() == R.id.lvShare) {
			menu.add("Exclude"); // TODO string resource
		}
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		if (!mRunning && item.getTitle().equals("Exclude")) { // TODO string resource
			// remove app from list
			AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();
			mAppAdapter.remove(mAppAdapter.getItem(info.position));

			if (mAppAdapter.getCount() < cSubmitLimit + 1 && getTitle().equals(getString(R.string.menu_submit))) {
					Button btnOk = (Button) findViewById(R.id.btnOk);
					btnOk.setEnabled(true);
			}
			return true;
		} else {
			return super.onContextItemSelected(item);
		}
	}

	// App info and share state

	private class AppHolder implements Comparable<AppHolder> {
		public int state = STATE_WAITING;
		public ApplicationInfoEx appInfo;
		public String message = null;

		public AppHolder(int uid) throws NameNotFoundException {
			appInfo = new ApplicationInfoEx(ActivityShare.this, uid);
		}

		@Override
		public int compareTo(AppHolder other) {
			return this.appInfo.compareTo(other.appInfo);
		}
	}

	// Adapters

	@SuppressLint("DefaultLocale")
	private class AppListAdapter extends ArrayAdapter<AppHolder> {
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		public List<AppHolder> mAppsWaiting;
		private List<AppHolder> mAppsDone;
		private ChangeNotifier changeNotifier = new ChangeNotifier();

		private class ChangeNotifier implements Runnable {
			int mScrollTo = -1;

			@Override
			public void run() {
				notifyDataSetChanged();
				if (mScrollTo >= 0) {
					ListView lvShare = (ListView) findViewById(R.id.lvShare);
					lvShare.smoothScrollToPosition(mScrollTo);
				}
			}

			public void setScrollTo(int position) {
				mScrollTo = position;
			}
		};

		public AppListAdapter(Context context, int resource, List<AppHolder> objects) {
			super(context, resource, objects);
			mAppsWaiting = new ArrayList<AppHolder>();
			mAppsWaiting.addAll(objects);
			mAppsDone = new ArrayList<AppHolder>();
		}

		public int[] getUids() {
			int[] uids = new int[this.getCount()];
			for (int i = 0; i < this.getCount(); i++) {
				uids[i] = this.getItem(i).appInfo.getUid();
			}
			return uids;
		}

		public void setState(int uid, int state) {
			AppHolder app = mAppsByUid.get(uid);
			// Make sure apps done or in progress are listed first
			// All operations except importing treat the apps in the listed order
			if (getTitle().equals(getString(R.string.menu_import)) && mAppsWaiting.contains(app)) {
				mAppsWaiting.remove(app);
				mAppsDone.add(app);
				this.setNotifyOnChange(false);
				this.clear();
				this.addAll(mAppsDone);
				this.addAll(mAppsWaiting);
				// If I separate out the app currently in progress, I could sort the done ones in the same way as the waiting ones were.
				// We'd then have in order: a sorted list of the done apps, mAppCurrent, then all the waiting apps in order
			}
			// Set state for this app
			app.state = state;
			changeNotifier.setScrollTo(mAppAdapter.getPosition(app));
			runOnUiThread(changeNotifier);
		}

		private class ViewHolder {
			private View row;
			private int position;
			public ImageView imgIcon;
			public TextView tvName;
			public ImageView imgResult;
			public ProgressBar pbRunning;
			public TextView tvMessage;

			public ViewHolder(View theRow, int thePosition) {
				row = theRow;
				position = thePosition;
				imgIcon = (ImageView) row.findViewById(R.id.imgIcon);
				tvName = (TextView) row.findViewById(R.id.tvApp);
				imgResult = (ImageView) row.findViewById(R.id.imgResult);
				pbRunning = (ProgressBar) row.findViewById(R.id.pbRunning);
				tvMessage = (TextView) row.findViewById(R.id.tvMessage);
			}
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			ViewHolder holder;
			if (convertView == null) {
				convertView = mInflater.inflate(R.layout.shareentry, null);
				holder = new ViewHolder(convertView, position);
				convertView.setTag(holder);
			} else {
				holder = (ViewHolder) convertView.getTag();
				holder.position = position;
			}

			// Get info
			final AppHolder xApp = getItem(holder.position);

			// Set background color
			if (xApp.appInfo.isSystem())
				holder.row.setBackgroundColor(getResources().getColor(
						Util.getThemed(ActivityShare.this, R.attr.color_dangerous)));
			else
				holder.row.setBackgroundColor(Color.TRANSPARENT);

			// Display icon
			holder.imgIcon.setImageDrawable(xApp.appInfo.getIcon(ActivityShare.this));
			holder.imgIcon.setVisibility(View.VISIBLE);

			// Set app name
			holder.tvName.setText(xApp.appInfo.toString());

			// Show app share state
			holder.tvMessage.setText("");
			switch (xApp.state) {
			case STATE_WAITING:
				holder.imgResult.setVisibility(View.INVISIBLE);
				holder.pbRunning.setVisibility(View.INVISIBLE);
				break;
			case STATE_RUNNING:
				holder.imgResult.setVisibility(View.INVISIBLE);
				holder.pbRunning.setVisibility(View.VISIBLE);
				if (xApp.message != null)
					holder.tvMessage.setText(xApp.message);
				break;
			case STATE_SUCCESS:
				holder.imgResult.setBackgroundResource(R.drawable.share_success);
				holder.imgResult.setVisibility(View.VISIBLE);
				holder.pbRunning.setVisibility(View.INVISIBLE);
				if (xApp.message != null)
					holder.tvMessage.setText(xApp.message);
				break;
			case STATE_FAILURE:
				holder.imgResult.setBackgroundResource(R.drawable.share_failure);
				holder.imgResult.setVisibility(View.VISIBLE);
				holder.pbRunning.setVisibility(View.INVISIBLE);
				if (xApp.message != null)
					holder.tvMessage.setText(xApp.message);
				break;
			}

			return convertView;
		}
	}

	// Tasks

	private class AppListTask extends AsyncTask<int[], Integer, List<AppHolder>> {
		private ProgressDialog mProgressDialog;

		@Override
		protected List<AppHolder> doInBackground(int[]... params) {
			List<AppHolder> apps = new ArrayList<AppHolder>();
			mAppsByUid = new SparseArray<AppHolder>();
			// TODO what if uids.length is zero? Which will only happen via Tasker etc.
			// We can't really ask Tasker to provide a list of uids, and someone might want to trigger an import for all apps.
			// - If action is IMPORT we should probably add all apps
			// - If action is FETCH we should probably add all non-system apps
			// We should probably add a Toast in those cases. "Adding all non-system apps" or "Adding all apps" 
			for (int i = 0; i < params[0].length; i++) {
				try {
					int uid = params[0][i];
					AppHolder app = new AppHolder(uid);
					apps.add(app);
					mAppsByUid.put(uid, app);
				} catch (NameNotFoundException ex) {
					Util.bug(null, ex);
				}
			}

			Collections.sort(apps);
			// TODO sort according to preferences
			// TODO add sort options to actionbar just like in ActivityMain
			return apps;
		}

		@Override
		protected void onPreExecute() {
			super.onPreExecute();

			// Show progress dialog
			ListView lvShare = (ListView) findViewById(R.id.lvShare);
			mProgressDialog = new ProgressDialog(lvShare.getContext());
			mProgressDialog.setMessage(getString(R.string.msg_loading));
			mProgressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			mProgressDialog.setProgressNumberFormat(null);
			mProgressDialog.setCancelable(false);
			mProgressDialog.show();
		}

		@Override
		protected void onPostExecute(List<AppHolder> listApp) {
			super.onPostExecute(listApp);

			// Display app list
			mAppAdapter = new AppListAdapter(ActivityShare.this, R.layout.shareentry, listApp);
			ListView lvShare = (ListView) findViewById(R.id.lvShare);
			lvShare.setAdapter(mAppAdapter);

			// Dismiss progress dialog
			try {
				mProgressDialog.dismiss();
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}
	}

	private class ExportTask extends AsyncTask<File, Integer, String> {
		private File mFile;

		@Override
		protected String doInBackground(File... params) {
			mProgressCurrent = 0;
			try {
				// Serialize
				mFile = params[0];
				Util.log(null, Log.INFO, "Exporting " + mFile);

				FileOutputStream fos = new FileOutputStream(mFile); // FileNotFoundException
				try {
					XmlSerializer serializer = Xml.newSerializer();
					serializer.setOutput(fos, "UTF-8"); // IOException, IllegalArgumentException, IllegalStateException
					serializer.startDocument(null, Boolean.valueOf(true));
					serializer.setFeature("http://xmlpull.org/v1/doc/features.html#indent-output", true);
					serializer.startTag(null, "XPrivacy");

					// Progress
					Util.log(null, Log.INFO, "Exporting settings");
					Runnable progress = new Runnable() {
						@Override
						public void run() {
							// This should be called exactly 100 times
							publishProgress(++mProgressCurrent, 100);
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

					for (PrivacyManager.RestrictionDesc restrictionDesc : listRestriction) {
						if ((++mProgressCurrent % (listRestriction.size() / 10 + 1)) == 0)
							publishProgress(mProgressCurrent, listRestriction.size() + 1);
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
				// IOException, IllegalArgumentException, IllegalStateException
				if (ex instanceof FileNotFoundException)
					return "Cannot create file";
				else if (ex instanceof IOException)
					return "Error writing to file";
				else
					return ex.getMessage();
			}
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(String result) {
			done(result);
			super.onPostExecute(result);
		}
	}

	private class ImportTask extends AsyncTask<Object, Integer, String> {
		private File mFile;

		@Override
		protected String doInBackground(Object... params) {
			try {
				mFile = (File) params[0];

				List<Integer> listUidSelected = new ArrayList<Integer>();
				for (int uid : (int[]) params[1])
					listUidSelected.add(uid);

				// Progress
				mProgressCurrent = 0;
				final int max = listUidSelected.size();
				Runnable progress = new Runnable() {
					@Override
					public void run() {
						publishProgress(++mProgressCurrent, max);
					}
				};

				// Parse XML
				Util.log(null, Log.INFO, "Importing " + mFile);
				FileInputStream fis = null;
				Map<String, Map<String, List<ImportHandler.MethodDescription>>> mapPackage;
				try {
					fis = new FileInputStream(mFile); // FileNotFoundException
					XMLReader xmlReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
					ImportHandler importHandler = new ImportHandler(listUidSelected, progress);
					xmlReader.setContentHandler(importHandler);
					xmlReader.parse(new InputSource(fis)); // IOException, SAXException
					mapPackage = importHandler.getPackageMap();
				} finally {
					if (fis != null)
						fis.close();
				}

				// Progress
				int progressMax = mapPackage.size();

				// Process result (legacy)
				for (String packageName : mapPackage.keySet()) {
					try {
						publishProgress(++mProgressCurrent, progressMax + 1);

						// Get uid
						int uid = getPackageManager().getPackageInfo(packageName, 0).applicationInfo.uid;

						if (listUidSelected.size() == 0 || listUidSelected.contains(uid)) {
							Util.log(null, Log.INFO, "Importing " + packageName);
							mAppAdapter.setState(uid, STATE_RUNNING);

							// Reset existing restrictions
							PrivacyManager.deleteRestrictions(ActivityShare.this, uid, true);

							// Set imported restrictions
							for (String restrictionName : mapPackage.get(packageName).keySet()) {
								PrivacyManager.setRestricted(null, ActivityShare.this, uid, restrictionName, null, true,
										true);
								for (ImportHandler.MethodDescription md : mapPackage.get(packageName).get(restrictionName))
									PrivacyManager.setRestricted(null, ActivityShare.this, uid, restrictionName,
											md.getMethodName(), md.isRestricted(), true);
							}
							mAppAdapter.setState(uid, STATE_SUCCESS);
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
				// FileNotFoundException, IOException, SAXException
				if (ex instanceof FileNotFoundException)
					return "File not found";
				else if (ex instanceof IOException)
					return "Error reading file";
				else if (ex instanceof SAXException)
					return "File is damaged";
				else
					return ex.getMessage();
			}
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(String result) {
			// Mark as failed the apps that weren't found
			for (AppHolder app : mAppAdapter.mAppsWaiting)
				app.state = STATE_FAILURE;
			mAppAdapter.notifyDataSetChanged();
			done(result);
			super.onPostExecute(result);
		}
	}

	private class ImportHandler extends DefaultHandler {
		private List<Integer> mListUidSelected;
		private SparseArray<String> mMapId = new SparseArray<String>();
		private Map<String, Integer> mMapUid = new HashMap<String, Integer>();
		private Map<String, Map<String, List<MethodDescription>>> mMapPackage = new HashMap<String, Map<String, List<MethodDescription>>>();
		private String android_id = Secure.getString(getContentResolver(), Secure.ANDROID_ID);
		private Runnable mProgress;
		private List<Integer> mImportedUids = new ArrayList<Integer>();

		public ImportHandler(List<Integer> listUidSelected, Runnable progress) {
			mListUidSelected = listUidSelected;
			mProgress = progress;
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {
			try {
				if (qName.equals("XPrivacy")) {
					// Ignore
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
						if (uid >= 0 && mListUidSelected.size() == 0 || mListUidSelected.contains(uid))
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

					// Get uid
					int uid = getUid(id);
					if (uid >= 0 && mListUidSelected.size() == 0 || mListUidSelected.contains(uid)) {
						// Progress report and pre-import cleanup
						if (!mImportedUids.contains(uid)) {
							// Mark the app we have just imported as a success
							if (mImportedUids.size() > 0) {
								int lastUid = mImportedUids.get(mImportedUids.size() - 1);
								mAppAdapter.setState(lastUid, STATE_SUCCESS);
							}
							// Mark the next one as in progress
							mImportedUids.add(uid);
							mAppAdapter.setState(uid, STATE_RUNNING);
							runOnUiThread(mProgress);
							PrivacyManager.deleteRestrictions(ActivityShare.this, uid, false);
						}

						PrivacyManager.setRestricted(null, ActivityShare.this, uid, restrictionName, methodName,
								restricted, false);
					}
				} else
					Util.log(null, Log.ERROR, "Unknown element name=" + qName);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) {
			if (qName.equals("XPrivacy")) {
				// Mark the last app imported as a success
				if (mImportedUids.size() > 0) {
					int lastUid = mImportedUids.get(mImportedUids.size() - 1);
					mAppAdapter.setState(lastUid, STATE_SUCCESS);
				}
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
	}

	private class FetchTask extends AsyncTask<int[], Integer, String> {

		@Override
		@SuppressLint("DefaultLocale")
		protected String doInBackground(int[]... params) {
			try {
				// Get data
				List<ApplicationInfoEx> lstApp = new ArrayList<ApplicationInfoEx>();
				for (int uid : params[0])
					lstApp.add(new ApplicationInfoEx(ActivityShare.this, uid)); // NameNotFoundException
					// This error probably should be caught here. TODO

				String android_id = Secure.getString(ActivityShare.this.getContentResolver(), Secure.ANDROID_ID);
				String[] license = Util.getProLicense();
				PackageInfo pXPrivacyInfo = getPackageManager().getPackageInfo(getPackageName(), 0); // NameNotFoundException

				String confidence = PrivacyManager.getSetting(null, ActivityShare.this, 0,
						PrivacyManager.cSettingConfidence, "", false);

				// Initialize progress
				mProgressCurrent = 0;

				// Process applications
				for (ApplicationInfoEx appInfo : lstApp) {
					publishProgress(++mProgressCurrent, lstApp.size() + 1);
					if (!appInfo.isSystem() || lstApp.size() == 1) {
						mAppAdapter.setState(appInfo.getUid(), STATE_RUNNING);

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
						jRoot.put("protocol_version", 4); // JSONException
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

						HttpPost httpost = new HttpPost(BASE_URL + "?format=json&action=fetch");
						httpost.setEntity(new ByteArrayEntity(jRoot.toString().getBytes("UTF-8"))); // UnsupportedEncodingException
						httpost.setHeader("Accept", "application/json");
						httpost.setHeader("Content-type", "application/json");
						HttpResponse response = httpclient.execute(httpost); // ClientProtocolException
						StatusLine statusLine = response.getStatusLine();

						if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
							// Succeeded
							ByteArrayOutputStream out = new ByteArrayOutputStream();
							response.getEntity().writeTo(out); // IOException
							out.close(); // IOException

							// Deserialize
							JSONObject status = new JSONObject(out.toString("UTF-8")); // UnsupportedEncodingException
							if (status.getBoolean("ok")) { // JSONException
								JSONArray settings = status.getJSONArray("settings"); // JSONException
								if (settings.length() > 0) {
									// Delete existing restrictions
									PrivacyManager.deleteRestrictions(ActivityShare.this, appInfo.getUid(), true);

									// Set fetched restrictions
									for (int i = 0; i < settings.length(); i++) {
										JSONObject entry = settings.getJSONObject(i); // JSONException
										String restrictionName = entry.getString("restriction");
										String methodName = entry.has("method") ? entry.getString("method") : null;
										int voted_restricted = entry.getInt("restricted");
										int voted_not_restricted = entry.getInt("not_restricted");
										boolean restricted = (voted_restricted > voted_not_restricted);
										if (methodName == null || restricted)
											if (methodName == null
													|| PrivacyManager.getMethod(restrictionName, methodName) != null)
												PrivacyManager
														.setRestricted(null, ActivityShare.this, appInfo.getUid(),
																restrictionName, methodName, restricted, true);
									}

									mAppAdapter.setState(appInfo.getUid(), STATE_SUCCESS);
								} else
									mAppAdapter.setState(appInfo.getUid(), STATE_FAILURE);
							} else
								throw new Exception(status.getString("error")); // JSONException, Exception
						} else {
							// Failed
							mAppAdapter.setState(appInfo.getUid(), STATE_FAILURE);
							response.getEntity().getContent().close(); // IOException
							throw new IOException(statusLine.getReasonPhrase()); // IOException
						}
					}
				}
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				// NameNotFoundException, JSONException, UnsupportedEncodingException, IOException, ClientProtocolException, Exception
				if (ex instanceof IOException || ex instanceof ClientProtocolException)
					return "Error connecting to server";
				else if (ex instanceof JSONException)
					return "Bad data received";
				else
					return ex.getMessage();
			}
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(String result) {
			done(result);
			super.onPostExecute(result);
		}
	}

	@SuppressLint("DefaultLocale")
	private class SubmitTask extends AsyncTask<int[], Integer, String> {

		@Override
		protected String doInBackground(int[]... params) {
			try {
				// Get data
				List<ApplicationInfoEx> lstApp = new ArrayList<ApplicationInfoEx>();
				for (int uid : params[0])
					lstApp.add(new ApplicationInfoEx(ActivityShare.this, uid)); // NameNotFoundException
					// Catch this error here? TODO

				// Initialize progress
				mProgressCurrent = 0;

				for (ApplicationInfoEx appInfo : lstApp) {
					// Update progess
					publishProgress(++mProgressCurrent, lstApp.size() + 1);
					mAppAdapter.setState(appInfo.getUid(), STATE_RUNNING);

					// Check if any account allowed
					boolean allowedAccounts = false;
					AccountManager accountManager = AccountManager.get(ActivityShare.this);
					for (Account account : accountManager.getAccounts()) {
						String sha1 = Util.sha1(account.name + account.type); // UnsupportedEncodingException
						boolean allowed = PrivacyManager.getSettingBool(null, ActivityShare.this, 0,
								String.format("Account.%d.%s", appInfo.getUid(), sha1), false, false);
						if (allowed) {
							allowedAccounts = true;
							break;
						}
					}

					// Check if any application allowed
					boolean allowedApplications = false;
					for (ApplicationInfoEx aAppInfo : ApplicationInfoEx.getXApplicationList(ActivityShare.this, null))
						for (String packageName : aAppInfo.getPackageName()) {
							boolean allowed = PrivacyManager.getSettingBool(null, ActivityShare.this, 0,
									String.format("Application.%d.%s", aAppInfo.getUid(), packageName), false, false);
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
								boolean allowed = PrivacyManager.getSettingBool(null, ActivityShare.this, 0,
										String.format("Contact.%d.%d", appInfo.getUid(), id), false, false);
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
						boolean restricted = PrivacyManager.getRestricted(null, ActivityShare.this, appInfo.getUid(),
								restrictionName, null, false, false);
						// Category
						long used = PrivacyManager.getUsed(ActivityShare.this, appInfo.getUid(), restrictionName, null);
						JSONObject jRestriction = new JSONObject();
						jRestriction.put("restriction", restrictionName); // JSONException
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
									&& PrivacyManager.getRestricted(null, ActivityShare.this, appInfo.getUid(),
											restrictionName, md.getName(), false, false);
							long mUsed = PrivacyManager.getUsed(ActivityShare.this, appInfo.getUid(), restrictionName,
									md.getName());
							JSONObject jMethod = new JSONObject();
							jMethod.put("restriction", restrictionName); // JSONException
							jMethod.put("method", md.getName());
							jMethod.put("restricted", mRestricted);
							jMethod.put("used", mUsed);
							jSettings.put(jMethod);
						}
					}

					// Get data
					PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0); // NameNotFoundException
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
					jRoot.put("protocol_version", 4); // JSONException
					jRoot.put("android_id", Util.md5(android_id).toLowerCase());
					jRoot.put("android_sdk", Build.VERSION.SDK_INT);
					jRoot.put("xprivacy_version", pInfo.versionCode);
					jRoot.put("application_name", appName);
					jRoot.put("package_name", pkgName);
					jRoot.put("package_version_name", pkgVersionName);
					jRoot.put("package_version_code", pkgVersionCode);
					jRoot.put("settings", jSettings);

					// Submit
					HttpParams httpParams = new BasicHttpParams();
					HttpConnectionParams.setConnectionTimeout(httpParams, ActivityShare.TIMEOUT_MILLISEC);
					HttpConnectionParams.setSoTimeout(httpParams, ActivityShare.TIMEOUT_MILLISEC);
					HttpClient httpclient = new DefaultHttpClient(httpParams);

					HttpPost httpost = new HttpPost(ActivityShare.BASE_URL + "?format=json&action=submit");
					httpost.setEntity(new ByteArrayEntity(jRoot.toString().getBytes("UTF-8"))); // UnsupportedEncodingException
					httpost.setHeader("Accept", "application/json");
					httpost.setHeader("Content-type", "application/json");
					HttpResponse response = httpclient.execute(httpost); // ClientProtocolException
					StatusLine statusLine = response.getStatusLine();

					if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
						// Succeeded
						ByteArrayOutputStream out = new ByteArrayOutputStream();
						response.getEntity().writeTo(out); // IOException
						out.close();
						JSONObject status = new JSONObject(out.toString("UTF-8")); // UnsupportedEncodingException
						if (status.getBoolean("ok")) { // JSONException
							// Mark as shared
							PrivacyManager.setSetting(null, ActivityShare.this, appInfo.getUid(),
									PrivacyManager.cSettingState, Integer.toString(ActivityMain.STATE_SHARED));
							mAppAdapter.setState(appInfo.getUid(), STATE_SUCCESS);
						} else {
							mAppAdapter.setState(appInfo.getUid(), STATE_FAILURE);
							throw new Exception(status.getString("error")); // JSONException, Exception
						}
					} else {
						// Failed
						mAppAdapter.setState(appInfo.getUid(), STATE_FAILURE);
						response.getEntity().getContent().close(); // IOException
						throw new IOException(statusLine.getReasonPhrase());
					}
				}
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				// NameNotFoundException, UnsupportedEncodingException, JSONException, ClientProtocolException, IOException, Exception
				if (ex instanceof ClientProtocolException || ex instanceof IOException)
					return "Error connecting to server";
				else
					return ex.getMessage();
			}
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(String result) {
			done(result);
			super.onPostExecute(result);
		}
	}

	// Helper methods

	private void blueStreakOfProgress(Integer current, Integer max) {
		// Set up the progress bar
		if (mProgressWidth == 0) {
			final View vShareProgressEmpty = (View) findViewById(R.id.vShareProgressEmpty);
			mProgressWidth = vShareProgressEmpty.getMeasuredWidth();
		}
		// Display stuff
		if (max == 0)
			max = 1;
		int width = (int) ((float) mProgressWidth) * current / max;
		Util.log(null, Log.WARN, "Progress width " + width);

		View vShareProgressFull = (View) findViewById(R.id.vShareProgressFull);
		vShareProgressFull.getLayoutParams().width = width;
		vShareProgressFull.invalidate();
	}

	private void done(String result) {
		// Check result string and display toast with error
		// TODO it might be better to put this in a dialog box asking whether to send debugging info
		if (result != null)
			Toast.makeText(this, result, Toast.LENGTH_LONG).show();
		// Reset progress bar
		blueStreakOfProgress(0, 1);
		mRunning = false;
		// Change ok button to "Close"
		final Button btnOk = (Button) findViewById(R.id.btnOk);
		btnOk.setText("Close"); // TODO string resource
		btnOk.setEnabled(true);
		btnOk.setOnClickListener(new Button.OnClickListener(){
			@Override
			public void onClick(View v) {
				finish();
			}
		});
		// Remove cancel button
		final Button btnCancel = (Button) findViewById(R.id.btnCancel);
		btnCancel.setVisibility(View.GONE);
		// TODO a nice touch would be to make the cancel button open the main list with only the failed apps in view.
		// I'm not sure what text to put on it though; "Examine failed" might do, if it isn't too long.
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

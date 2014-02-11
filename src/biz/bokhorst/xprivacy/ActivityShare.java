package biz.bokhorst.xprivacy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
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
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xmlpull.v1.XmlSerializer;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.annotation.SuppressLint;
import android.app.ProgressDialog;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.database.Cursor;
import android.graphics.Color;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.provider.ContactsContract;
import android.provider.Settings.Secure;
import android.util.Log;
import android.util.SparseArray;
import android.util.Xml;
import android.view.ContextMenu;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.EditText;
import android.widget.Toast;

public class ActivityShare extends ActivityBase {
	private int mThemeId;
	private int mActionId;
	private AppListAdapter mAppAdapter;
	private SparseArray<AppHolder> mAppsByUid;
	private boolean mRunning = false;
	private boolean mAbort = false;
	private int mProgressCurrent;
	private int mProgressWidth = 0;
	private String mFileName;
	public boolean mSomeRestricted = false;
	private boolean mInteractive = false;

	private static final int STATE_WAITING = 0;
	private static final int STATE_RUNNING = 1;
	private static final int STATE_SUCCESS = 2;
	private static final int STATE_FAILURE = 3;

	private static final int ACTIVITY_IMPORT_SELECT = 0;

	public static final String cUidList = "UidList";
	public static final String cRestriction = "Restriction";
	public static final String cInteractive = "Interactive";
	public static final String HTTP_BASE_URL = "http://crowd.xprivacy.eu/";
	public static final String HTTPS_BASE_URL = "https://crowd.xprivacy.eu/";

	public static final int cSubmitLimit = 10;
	public static final int cProtocolVersion = 4;

	public static final String ACTION_EXPORT = "biz.bokhorst.xprivacy.action.EXPORT";
	public static final String ACTION_IMPORT = "biz.bokhorst.xprivacy.action.IMPORT";
	public static final String ACTION_FETCH = "biz.bokhorst.xprivacy.action.FETCH";
	public static final String ACTION_SUBMIT = "biz.bokhorst.xprivacy.action.SUBMIT";
	public static final String ACTION_TOGGLE = "biz.bokhorst.xprivacy.action.TOGGLE";

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

		// Get data
		final Bundle extras = getIntent().getExtras();
		final String action = getIntent().getAction();
		final int[] uids = (extras != null && extras.containsKey(cUidList) ? extras.getIntArray(cUidList) : new int[0]);
		final String restriction = (extras != null ? extras.getString(cRestriction) : null);
		// TODO: show localized restriction name

		// License check
		if (action.equals(ACTION_IMPORT) || action.equals(ACTION_EXPORT)) {
			if (!Util.isProEnabled() && Util.hasProLicense(this) == null) {
				Util.viewUri(this, ActivityMain.cProUri);
				finish();
				return;
			}
		} else if (action.equals(ACTION_FETCH)) {
			if (Util.hasProLicense(this) == null) {
				Util.viewUri(this, ActivityMain.cProUri);
				finish();
				return;
			}
		}

		// Registration check
		if (action.equals(ACTION_SUBMIT) && !registerDevice(this)) {
			finish();
			return;
		}

		// Check whether we need a ui
		if (extras != null && extras.containsKey(cInteractive) && extras.getBoolean(cInteractive, false))
			mInteractive = true;

		// Set theme
		String themeName = PrivacyManager.getSetting(0, PrivacyManager.cSettingTheme, "", false);
		mThemeId = (themeName.equals("Dark") ? R.style.CustomTheme : R.style.CustomTheme_Light);
		setTheme(mThemeId);

		// Set layout
		setContentView(R.layout.sharelist);

		// Set title
		if (action.equals(ACTION_TOGGLE)) {
			mActionId = R.string.menu_restriction_all;
			setTitle(R.string.menu_restriction_all);
		} else if (action.equals(ACTION_IMPORT)) {
			mActionId = R.string.menu_import;
			setTitle(R.string.menu_import);
		} else if (action.equals(ACTION_EXPORT)) {
			mActionId = R.string.menu_export;
			setTitle(R.string.menu_export);
		} else if (action.equals(ACTION_FETCH)) {
			mActionId = R.string.menu_fetch;
			setTitle(R.string.menu_fetch);
		} else if (action.equals(ACTION_SUBMIT)) {
			mActionId = R.string.menu_submit;
			setTitle(R.string.menu_submit);
		} else {
			finish();
			return;
		}

		// App list
		ListView lvShare = (ListView) findViewById(R.id.lvShare);
		AppListTask appListTask = new AppListTask();
		appListTask.executeOnExecutor(mExecutor, uids, restriction);

		// Allow users to remove apps from list
		// TODO: replace by swipe left/right
		// http://stackoverflow.com/questions/18585345/swipe-to-delete-a-listview-item
		if (mInteractive)
			registerForContextMenu(lvShare);

		// Import/export filename
		if (action.equals(ACTION_EXPORT) || action.equals(ACTION_IMPORT)) {
			// Check for availability of sharing intent
			Intent file = new Intent(Intent.ACTION_GET_CONTENT);
			file.setType("file/*");
			boolean hasIntent = Util.isIntentAvailable(ActivityShare.this, file);

			// Get file name
			if (action.equals(ACTION_EXPORT))
				mFileName = getFileName(this, hasIntent);
			else
				mFileName = (hasIntent ? null : getFileName(this, false));
			if (mFileName == null)
				fileChooser();
			else
				showFileName();
		} else {
			TextView tvDescription = (TextView) findViewById(R.id.tvDescription);
			tvDescription.setText(getBaseURL(ActivityShare.this));
		}

		// Reference buttons
		final Button btnOk = (Button) findViewById(R.id.btnOk);
		final Button btnCancel = (Button) findViewById(R.id.btnCancel);

		if (mInteractive) {
			// Enable ok (showFileName does this for export/import)
			if (action.equals(ACTION_SUBMIT) || action.equals(ACTION_FETCH) || action.equals(ACTION_TOGGLE))
				btnOk.setEnabled(true);

			// Listen for ok
			btnOk.setOnClickListener(new Button.OnClickListener() {
				@Override
				public void onClick(View v) {
					btnOk.setEnabled(false);

					// Toggle
					if (action.equals(ACTION_TOGGLE)) {
						mRunning = true;
						new ToggleTask().executeOnExecutor(mExecutor, restriction);
					}

					// Import
					if (action.equals(ACTION_IMPORT)) {
						mRunning = true;
						new ImportTask().executeOnExecutor(mExecutor, new File(mFileName));
					}

					// Export
					else if (action.equals(ACTION_EXPORT)) {
						mRunning = true;
						new ExportTask().executeOnExecutor(mExecutor, new File(mFileName));
					}

					// Fetch
					else if (action.equals(ACTION_FETCH)) {
						if (uids.length > 0) {
							mRunning = true;
							new FetchTask().executeOnExecutor(mExecutor);
						}
					}

					// Submit
					else if (action.equals(ACTION_SUBMIT)) {
						if (uids.length > 0) {
							if (uids.length <= cSubmitLimit) {
								mRunning = true;
								new SubmitTask().executeOnExecutor(mExecutor);
							} else {
								String message = getString(R.string.msg_limit, ActivityShare.cSubmitLimit + 1);
								Toast.makeText(ActivityShare.this, message, Toast.LENGTH_SHORT).show();
								btnOk.setEnabled(false);
							}
						}
					}
				}
			});
		} else {
			// Hide ok button and separator
			btnOk.setVisibility(View.GONE);
			final View vButtonSeparator = findViewById(R.id.vButtonSeparator);
			vButtonSeparator.setVisibility(View.GONE);
		}

		// Listen for cancel
		btnCancel.setOnClickListener(new Button.OnClickListener() {
			@Override
			public void onClick(View v) {
				if (mRunning) {
					mAbort = true;
					Toast.makeText(ActivityShare.this, getString(R.string.msg_abort), Toast.LENGTH_SHORT).show();
				} else
					finish();
			}
		});
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent dataIntent) {
		super.onActivityResult(requestCode, resultCode, dataIntent);

		if (requestCode == ACTIVITY_IMPORT_SELECT) {
			// Import select
			if (resultCode == RESULT_CANCELED || dataIntent == null)
				finish();
			else {
				String fileName = dataIntent.getData().getPath();
				mFileName = fileName.replace("/document/primary:", Environment.getExternalStorageDirectory()
						.getAbsolutePath() + File.separatorChar);
				showFileName();
			}
		}
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
		super.onCreateContextMenu(menu, v, menuInfo);
		if (v.getId() == R.id.lvShare)
			menu.addSubMenu(Menu.NONE, R.string.menu_exclude, Menu.NONE, R.string.menu_exclude);
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		if (!mRunning && item.getItemId() == R.string.menu_exclude) {
			// remove app from list
			AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();
			mAppAdapter.remove(mAppAdapter.getItem(info.position));

			// Check submit limit
			if (mActionId == R.string.menu_submit && mAppAdapter.getCount() <= cSubmitLimit) {
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
		public List<AppHolder> mAppsWaiting = new ArrayList<AppHolder>();
		private List<AppHolder> mAppsDone = new ArrayList<AppHolder>();
		private ChangeNotifier changeNotifier = new ChangeNotifier();

		private class ChangeNotifier implements Runnable {
			AppHolder mScrollTo;

			@Override
			public void run() {
				if (mScrollTo != null) {
					int position = mAppAdapter.getPosition(mScrollTo);
					// Make sure apps done or in progress are listed first
					// All operations except importing
					// treat the apps in the listed order
					if (mActionId == R.string.menu_import && mAppsWaiting.contains(mScrollTo)) {
						mAppsWaiting.remove(mScrollTo);
						mAppsDone.add(mScrollTo);
						mAppAdapter.setNotifyOnChange(false);
						mAppAdapter.clear();
						mAppAdapter.addAll(mAppsDone);
						mAppAdapter.addAll(mAppsWaiting);
						// If I separate out the app currently in progress, I
						// could sort the done ones in the same way as the
						// waiting ones were.
						// We'd then have in order: a sorted list of the done
						// apps, mAppCurrent, then all the waiting apps in order
					}
					notifyDataSetChanged();
					if (position >= 0) {
						ListView lvShare = (ListView) findViewById(R.id.lvShare);
						lvShare.smoothScrollToPosition(position);
					}
				}
			}

			public void setScrollTo(AppHolder app) {
				mScrollTo = app;
			}
		};

		public AppListAdapter(Context context, int resource, List<AppHolder> objects) {
			super(context, resource, objects);
			mAppsWaiting.addAll(objects);
		}

		public List<Integer> getListUid() {
			List<Integer> uids = new ArrayList<Integer>();
			for (int i = 0; i < this.getCount(); i++)
				uids.add(this.getItem(i).appInfo.getUid());
			return uids;
		}

		public List<ApplicationInfoEx> getListAppInfo() {
			List<ApplicationInfoEx> apps = new ArrayList<ApplicationInfoEx>();
			for (int i = 0; i < this.getCount(); i++)
				apps.add(this.getItem(i).appInfo);
			return apps;
		}

		public void setState(int uid, int state, String message) {
			AppHolder app = mAppsByUid.get(uid);
			app.message = message;
			app.state = state;
			changeNotifier.setScrollTo(app);
			runOnUiThread(changeNotifier);
		}

		public void setState(int uid, int state) {
			AppHolder app = mAppsByUid.get(uid);
			app.state = state;
		}

		public void setMessage(int uid, String message) {
			AppHolder app = mAppsByUid.get(uid);
			app.message = message;
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
				holder.row.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
			else
				holder.row.setBackgroundColor(Color.TRANSPARENT);

			// Display icon
			holder.imgIcon.setImageDrawable(xApp.appInfo.getIcon(ActivityShare.this));
			holder.imgIcon.setVisibility(View.VISIBLE);

			// Set app name
			holder.tvName.setText(xApp.appInfo.toString());

			// Show app share state
			holder.tvMessage.setText(xApp.message == null ? "" : xApp.message);
			switch (xApp.state) {
			case STATE_WAITING:
				holder.imgResult.setVisibility(View.INVISIBLE);
				holder.pbRunning.setVisibility(View.INVISIBLE);
				break;
			case STATE_RUNNING:
				holder.imgResult.setVisibility(View.INVISIBLE);
				holder.pbRunning.setVisibility(View.VISIBLE);
				break;
			case STATE_SUCCESS:
				holder.imgResult.setBackgroundResource(R.drawable.btn_check_buttonless_on);
				holder.imgResult.setVisibility(View.VISIBLE);
				holder.pbRunning.setVisibility(View.INVISIBLE);
				break;
			case STATE_FAILURE:
				holder.imgResult.setBackgroundResource(R.drawable.indicator_input_error);
				holder.imgResult.setVisibility(View.VISIBLE);
				holder.pbRunning.setVisibility(View.INVISIBLE);
				break;
			}

			return convertView;
		}
	}

	// Tasks

	private class AppListTask extends AsyncTask<Object, Integer, List<AppHolder>> {
		private ProgressDialog mProgressDialog;

		@Override
		protected List<AppHolder> doInBackground(Object... params) {
			int[] uids = (int[]) params[0];
			String restrictionName = (String) params[1];
			List<AppHolder> apps = new ArrayList<AppHolder>();
			mAppsByUid = new SparseArray<AppHolder>();

			if (!mInteractive && mActionId == R.string.menu_export) {
				// Build list of distinct uids for export
				List<Integer> listUid = new ArrayList<Integer>();
				for (PackageInfo pInfo : getPackageManager().getInstalledPackages(0))
					if (!listUid.contains(pInfo.applicationInfo.uid))
						listUid.add(pInfo.applicationInfo.uid);
				// Convert to primitive array
				uids = new int[listUid.size()];
				for (int i = 0; i < listUid.size(); i++)
					uids[i] = listUid.get(i);
			}

			boolean some = false;
			mProgressDialog.setMax(uids.length);
			for (int i = 0; i < uids.length; i++) {
				mProgressDialog.setProgress(i);
				try {
					AppHolder app = new AppHolder(uids[i]);
					apps.add(app);
					mAppsByUid.put(uids[i], app);

					// If toggling, check if some restricted
					if (mActionId == R.string.menu_restriction_all)
						for (PRestriction restriction : PrivacyManager.getRestrictionList(uids[i], restrictionName))
							if (restriction.restricted) {
								some = true;
								break;
							}

				} catch (NameNotFoundException ex) {
					Util.bug(null, ex);
				}
			}

			if (mActionId == R.string.menu_restriction_all) {
				mSomeRestricted = some;
				mActionId = (some ? R.string.menu_clear_all : R.string.menu_restrict_all);
				Util.log(null, Log.WARN, "Toggle means clear=" + some);
			}

			Collections.sort(apps);
			// TODO: sort according to preferences
			// TODO: add sort options to actionbar just like in ActivityMain
			return apps;
		}

		@Override
		protected void onPreExecute() {
			super.onPreExecute();

			// Show progress dialog
			mProgressDialog = new ProgressDialog(ActivityShare.this);
			mProgressDialog.setMessage(getString(R.string.msg_loading));
			mProgressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			mProgressDialog.setProgressNumberFormat(null);
			mProgressDialog.setCancelable(false);
			mProgressDialog.setCanceledOnTouchOutside(false);
			mProgressDialog.show();
		}

		@Override
		protected void onPostExecute(List<AppHolder> listApp) {
			if (!ActivityShare.this.isFinishing()) {
				// Display app list
				mAppAdapter = new AppListAdapter(ActivityShare.this, R.layout.shareentry, listApp);
				ListView lvShare = (ListView) findViewById(R.id.lvShare);
				lvShare.setAdapter(mAppAdapter);

				// Dismiss progress dialog
				mProgressDialog.dismiss();

				// If toggling, set title
				if (mActionId == R.string.menu_clear_all || mActionId == R.string.menu_restrict_all)
					ActivityShare.this.setTitle(mActionId);

				// Launch non-interactive export
				if (!mInteractive && mActionId == R.string.menu_export) {
					mRunning = true;
					new ExportTask().executeOnExecutor(mExecutor, new File(mFileName));
				}
			}

			super.onPostExecute(listApp);
		}
	}

	private class ToggleTask extends AsyncTask<String, Integer, Throwable> {
		@Override
		protected Throwable doInBackground(String... params) {
			// Get data
			mProgressCurrent = 0;
			List<Integer> lstUid = mAppAdapter.getListUid();
			final String restriction = params[0];

			for (Integer uid : lstUid)
				try {
					if (mAbort)
						throw new AbortException(ActivityShare.this);

					// Update progess
					publishProgress(++mProgressCurrent, lstUid.size() + 1);
					mAppAdapter.setState(uid, STATE_RUNNING, null);

					List<Boolean> oldState = PrivacyManager.getRestartStates(uid, null);
					if (restriction == null && mSomeRestricted)
						PrivacyManager.deleteRestrictions(uid, null);
					else if (restriction == null)
						for (String restrictionName : PrivacyManager.getRestrictions()) {
							String templateName = PrivacyManager.cSettingTemplate + "." + restrictionName;
							if (PrivacyManager.getSettingBool(0, templateName, true, false))
								PrivacyManager.setRestriction(uid, restrictionName, null, !mSomeRestricted, false);
						}
					else
						PrivacyManager.setRestriction(uid, restriction, null, !mSomeRestricted, false);
					List<Boolean> newState = PrivacyManager.getRestartStates(uid, null);

					mAppAdapter.setState(uid, STATE_SUCCESS,
							!newState.equals(oldState) ? getString(R.string.msg_restart) : null);
				} catch (Throwable ex) {
					mAppAdapter.setState(uid, STATE_FAILURE, ex.getLocalizedMessage());
					return ex;
				}

			return null;
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(Throwable result) {
			if (!ActivityShare.this.isFinishing())
				done(result);
			super.onPostExecute(result);
		}
	}

	private class ExportTask extends AsyncTask<File, Integer, Throwable> {
		private File mFile;

		@Override
		protected Throwable doInBackground(File... params) {
			mProgressCurrent = 0;
			try {
				mFile = params[0];

				List<Integer> listUid = mAppAdapter.getListUid();

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
					List<PSetting> listGlobalSetting = PrivacyManager.getSettingList(0);
					for (PSetting setting : listGlobalSetting) {
						// Serialize setting
						serializer.startTag(null, "Setting");
						serializer.attribute(null, "Id", "");
						serializer.attribute(null, "Name", setting.name);
						if (setting.value != null)
							serializer.attribute(null, "Value", setting.value);
						serializer.endTag(null, "Setting");
					}

					// Process application settings and restrictions
					for (int uid : listUid)
						try {
							if (mAbort)
								throw new AbortException(ActivityShare.this);

							publishProgress(++mProgressCurrent, listUid.size() + 1);
							mAppAdapter.setState(uid, STATE_RUNNING, null);

							// Process application settings
							List<PSetting> listAppSetting = PrivacyManager.getSettingList(uid);
							for (PSetting setting : listAppSetting) {
								// Bind accounts/contacts to same device
								if (setting.name.startsWith(PrivacyManager.cSettingAccount)
										|| setting.name.startsWith(PrivacyManager.cSettingContact)
										|| setting.name.startsWith(PrivacyManager.cSettingRawContact))
									setting.name += "." + android_id;

								// Serialize setting
								serializer.startTag(null, "Setting");
								serializer.attribute(null, "Id", Integer.toString(uid));
								serializer.attribute(null, "Name", setting.name);
								if (setting.value != null)
									serializer.attribute(null, "Value", setting.value);
								serializer.endTag(null, "Setting");
							}

							// Process restrictions
							for (String restrictionName : PrivacyManager.getRestrictions()) {
								// Category
								// TODO: use getRestrictionList
								PRestriction crestricted = PrivacyManager.getRestrictionEx(uid, restrictionName, null);
								if (crestricted.restricted || crestricted.asked) {
									serializer.startTag(null, "Restriction");
									serializer.attribute(null, "Id", Integer.toString(uid));
									serializer.attribute(null, "Name", restrictionName);
									serializer.attribute(null, "Restricted", Boolean.toString(crestricted.restricted));
									serializer.attribute(null, "Asked", Boolean.toString(crestricted.asked));
									serializer.endTag(null, "Restriction");

									// Methods
									for (Hook md : PrivacyManager.getHooks(restrictionName)) {
										PRestriction mrestricted = PrivacyManager.getRestrictionEx(uid,
												restrictionName, md.getName());
										if (!mrestricted.restricted || mrestricted.asked || md.isDangerous()) {
											serializer.startTag(null, "Restriction");
											serializer.attribute(null, "Id", Integer.toString(uid));
											serializer.attribute(null, "Name", restrictionName);
											serializer.attribute(null, "Method", md.getName());
											serializer.attribute(null, "Restricted",
													Boolean.toString(mrestricted.restricted));
											serializer.attribute(null, "Asked", Boolean.toString(mrestricted.asked));
											serializer.endTag(null, "Restriction");
										}
									}
								}
							}

							mAppAdapter.setState(uid, STATE_SUCCESS, null);
						} catch (Throwable ex) {
							mAppAdapter.setState(uid, STATE_FAILURE, ex.getLocalizedMessage());
							throw ex;
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
				if (mFile.exists())
					mFile.delete();
				return ex;
			}
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(Throwable result) {
			if (!ActivityShare.this.isFinishing())
				if (mInteractive) {
					done(result);

					// Share
					if (result == null) {
						Intent intent = new Intent(android.content.Intent.ACTION_SEND);
						intent.setType("text/xml");
						intent.putExtra(Intent.EXTRA_STREAM, Uri.parse("file://" + mFileName));
						startActivity(Intent.createChooser(intent,
								String.format(getString(R.string.msg_saved_to), mFileName)));
					}
				} else {
					done(result);
					finish();
				}

			super.onPostExecute(result);
		}
	}

	private class ImportTask extends AsyncTask<File, Integer, Throwable> {
		private File mFile;

		@Override
		protected Throwable doInBackground(File... params) {
			try {
				mFile = (File) params[0];

				List<Integer> listUidSelected = mAppAdapter.getListUid();

				// Progress
				mProgressCurrent = 0;
				final int max = listUidSelected.size() + 1;
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
					fis = new FileInputStream(mFile);
					XMLReader xmlReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
					ImportHandler importHandler = new ImportHandler(listUidSelected, progress);
					xmlReader.setContentHandler(importHandler);
					xmlReader.parse(new InputSource(fis));
					mapPackage = importHandler.getPackageMap();

					if (mAbort)
						throw new AbortException(ActivityShare.this);
				} finally {
					if (fis != null)
						fis.close();
				}

				// Progress
				int progressMax = mapPackage.size();

				// Process result (legacy)
				for (String packageName : mapPackage.keySet()) {
					if (mAbort)
						throw new AbortException(ActivityShare.this);

					int uid = 0;
					try {
						publishProgress(++mProgressCurrent, progressMax + 1);

						// Get uid
						uid = getPackageManager().getPackageInfo(packageName, 0).applicationInfo.uid;

						if (listUidSelected.size() == 0 || listUidSelected.contains(uid)) {
							Util.log(null, Log.INFO, "Importing " + packageName);
							mAppAdapter.setState(uid, STATE_RUNNING, null);

							// Reset existing restrictions
							List<Boolean> oldState = PrivacyManager.getRestartStates(uid, null);
							PrivacyManager.deleteRestrictions(uid, null);

							// Set imported restrictions
							for (String restrictionName : mapPackage.get(packageName).keySet()) {
								PrivacyManager.setRestriction(uid, restrictionName, null, true, false);
								for (ImportHandler.MethodDescription md : mapPackage.get(packageName).get(
										restrictionName))
									PrivacyManager.setRestriction(uid, restrictionName, md.getMethodName(),
											md.isRestricted(), false);
							}
							List<Boolean> newState = PrivacyManager.getRestartStates(uid, null);

							mAppAdapter.setState(uid, STATE_SUCCESS,
									!newState.equals(oldState) ? getString(R.string.msg_restart) : null);
						}
					} catch (Throwable ex) {
						if (uid > 0)
							mAppAdapter.setState(uid, STATE_FAILURE, ex.getLocalizedMessage());
						Util.log(null, Log.WARN, "Not found package=" + packageName);
					}
				}

				// Display message
				Util.log(null, Log.INFO, "Importing finished");
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex;
			}
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(Throwable result) {
			// Mark as failed the apps that weren't found
			if (!ActivityShare.this.isFinishing()) {
				if (result == null) {
					List<AppHolder> listWaiting = new ArrayList<AppHolder>();
					listWaiting.addAll(mAppAdapter.mAppsWaiting);
					for (AppHolder app : listWaiting) {
						mAppAdapter.setState(app.appInfo.getUid(), STATE_FAILURE);
						mAppAdapter.setMessage(app.appInfo.getUid(), getString(R.string.msg_no_restrictions));
					}
					mAppAdapter.notifyDataSetChanged();
				}

				done(result);
			}
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
		private SparseArray<Map<String, String>> mSettings = new SparseArray<Map<String, String>>();
		private List<Integer> mListRestrictionUid = new ArrayList<Integer>();
		private List<Integer> mListAbortedUid = new ArrayList<Integer>();
		private SparseArray<List<Boolean>> mListRestartStates = new SparseArray<List<Boolean>>();

		public ImportHandler(List<Integer> listUidSelected, Runnable progress) {
			mListUidSelected = listUidSelected;
			mProgress = progress;
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {
			try {
				if (qName.equals("XPrivacy")) {
					// Root
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
						PrivacyManager.setSetting(0, name, value);
					} else if ("".equals(id))
						// Global setting
						// TODO: clear global settings
						PrivacyManager.setSetting(0, name, value);
					else {
						// Application setting
						int iid = Integer.parseInt(id);
						int uid = getUid(iid);
						if (uid >= 0 && mListUidSelected.size() == 0 || mListUidSelected.contains(uid)) {
							if (!mListRestrictionUid.contains(uid)) {
								// Cache settings
								if (mSettings.indexOfKey(uid) < 0)
									mSettings.put(uid, new HashMap<String, String>());
								mSettings.get(uid).put(name, value);
							} else {
								// This apps cached settings
								// have already been applied,
								// so add this one directly
								PrivacyManager.setSetting(uid, name, value);
							}
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
					boolean asked = Boolean.parseBoolean(attributes.getValue("Asked"));

					// Get uid
					int uid = getUid(id);

					if (uid >= 0 && mListUidSelected.size() == 0 || mListUidSelected.contains(uid)) {

						// Check whether we should abort
						if (mAbort && !mListRestrictionUid.contains(uid)) {
							// This app hasn't begun to be imported, so skip it
							if (!mListAbortedUid.contains(uid))
								mListAbortedUid.add(uid);
							Util.log(null, Log.INFO, "Skipping restriction for uid=" + uid);
							return;
						}

						// Progress report and pre-import cleanup
						if (!mListRestrictionUid.contains(uid)) {
							finishLastImport();

							// Mark the next one as in progress
							mListRestrictionUid.add(uid);
							mAppAdapter.setState(uid, STATE_RUNNING, null);
							runOnUiThread(mProgress);

							// Delete restrictions
							mListRestartStates.put(uid, PrivacyManager.getRestartStates(uid, null));
							PrivacyManager.deleteRestrictions(uid, null);
						}

						// Set restriction
						// TODO: use setRestrictionList
						PrivacyManager.setRestriction(uid, restrictionName, methodName, restricted, asked);
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
				finishLastImport();

				// Abort notification
				if (mListAbortedUid.size() > 0) {
					int uid = mListAbortedUid.get(0);
					mAppAdapter.setState(uid, STATE_FAILURE);
					mAppAdapter.setMessage(uid, getString(R.string.msg_aborted));
				}

				runOnUiThread(new Runnable() {
					@Override
					public void run() {
						mAppAdapter.notifyDataSetChanged();
					}
				});
			}
		}

		private void finishLastImport() {
			// Finish import for the last app imported
			if (mListRestrictionUid.size() > 0) {
				int lastUid = mListRestrictionUid.get(mListRestrictionUid.size() - 1);

				// Apply settings
				PrivacyManager.deleteSettings(lastUid);
				if (mSettings.indexOfKey(lastUid) >= 0)
					for (Entry<String, String> entry : mSettings.get(lastUid).entrySet())
						PrivacyManager.setSetting(lastUid, entry.getKey(), entry.getValue());

				// Restart notification
				List<Boolean> oldState = mListRestartStates.get(lastUid);
				List<Boolean> newState = PrivacyManager.getRestartStates(lastUid, null);

				// Mark as success
				mAppAdapter.setState(lastUid, STATE_SUCCESS,
						!newState.equals(oldState) ? getString(R.string.msg_restart) : null);
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

	private class FetchTask extends AsyncTask<int[], Integer, Throwable> {
		@Override
		@SuppressLint("DefaultLocale")
		protected Throwable doInBackground(int[]... params) {
			try {
				// Get data
				List<ApplicationInfoEx> lstApp = mAppAdapter.getListAppInfo();

				String[] license = Util.getProLicenseUnchecked();
				String android_id = Secure.getString(ActivityShare.this.getContentResolver(), Secure.ANDROID_ID);
				PackageInfo xInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
				String confidence = PrivacyManager.getSetting(0, PrivacyManager.cSettingConfidence, "", false);
				boolean dangerous = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingDangerous, false, false);

				// Initialize progress
				mProgressCurrent = 0;

				// Process applications
				for (ApplicationInfoEx appInfo : lstApp)
					try {
						publishProgress(++mProgressCurrent, lstApp.size() + 1);
						if (!appInfo.isSystem() || lstApp.size() == 1) {
							if (mAbort)
								throw new AbortException(ActivityShare.this);

							mAppAdapter.setState(appInfo.getUid(), STATE_RUNNING,
									ActivityShare.this.getString(R.string.menu_fetch));

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
							jRoot.put("xprivacy_version", xInfo.versionCode);
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

							if (mAbort)
								throw new AbortException(ActivityShare.this);

							mAppAdapter.setState(appInfo.getUid(), STATE_RUNNING,
									ActivityShare.this.getString(R.string.msg_applying));

							if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
								// Succeeded
								ByteArrayOutputStream out = new ByteArrayOutputStream();
								response.getEntity().writeTo(out);
								out.close();

								// Deserialize
								JSONObject status = new JSONObject(out.toString("UTF-8"));
								if (status.getBoolean("ok")) {
									JSONArray settings = status.getJSONArray("settings");
									// Delete existing restrictions
									List<Boolean> oldState = PrivacyManager.getRestartStates(appInfo.getUid(), null);
									PrivacyManager.deleteRestrictions(appInfo.getUid(), null);

									// Set fetched restrictions
									List<PRestriction> listRestriction = new ArrayList<PRestriction>();
									for (int i = 0; i < settings.length(); i++) {
										JSONObject entry = settings.getJSONObject(i);
										String restrictionName = entry.getString("restriction");
										String methodName = entry.has("method") ? entry.getString("method") : null;
										Hook hook = (methodName == null ? null : PrivacyManager.getHook(
												restrictionName, methodName));
										if (dangerous || hook == null || !hook.isDangerous()) {
											int voted_restricted = entry.getInt("restricted");
											int voted_not_restricted = entry.getInt("not_restricted");
											boolean restricted = (voted_restricted > voted_not_restricted);
											listRestriction.add(new PRestriction(appInfo.getUid(), restrictionName,
													methodName, restricted));
										}
									}
									PrivacyManager.setRestrictionList(listRestriction);
									List<Boolean> newState = PrivacyManager.getRestartStates(appInfo.getUid(), null);

									// Mark as new/changed
									PrivacyManager.setSetting(appInfo.getUid(), PrivacyManager.cSettingState,
											Integer.toString(ActivityMain.STATE_ATTENTION));

									// Change app modification time
									PrivacyManager.setSetting(appInfo.getUid(), PrivacyManager.cSettingModifyTime,
											Long.toString(System.currentTimeMillis()));

									mAppAdapter.setState(appInfo.getUid(), STATE_SUCCESS,
											!newState.equals(oldState) ? getString(R.string.msg_restart) : null);
								} else {
									int errno = status.getInt("errno");
									String message = status.getString("error");
									ServerException ex = new ServerException(ActivityShare.this, errno, message);
									mAppAdapter.setState(appInfo.getUid(), STATE_FAILURE, ex.getLocalizedMessage());
								}
							} else {
								// Failed
								response.getEntity().getContent().close();
								throw new IOException(statusLine.getReasonPhrase());
							}
						}
					} catch (Throwable ex) {
						mAppAdapter.setState(appInfo.getUid(), STATE_FAILURE, ex.getLocalizedMessage());
						throw ex;
					}
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex;
			}
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(Throwable result) {
			if (!ActivityShare.this.isFinishing())
				done(result);
			super.onPostExecute(result);
		}
	}

	@SuppressLint("DefaultLocale")
	private class SubmitTask extends AsyncTask<int[], Integer, Throwable> {
		@Override
		protected Throwable doInBackground(int[]... params) {
			try {
				// Get data
				List<ApplicationInfoEx> lstApp = mAppAdapter.getListAppInfo();

				// Initialize progress
				mProgressCurrent = 0;

				for (ApplicationInfoEx appInfo : lstApp)
					try {
						if (mAbort)
							throw new AbortException(ActivityShare.this);

						// Update progess
						publishProgress(++mProgressCurrent, lstApp.size() + 1);
						mAppAdapter.setState(appInfo.getUid(), STATE_RUNNING,
								ActivityShare.this.getString(R.string.msg_loading));

						// Check if any account allowed
						boolean allowedAccounts = false;
						AccountManager accountManager = AccountManager.get(ActivityShare.this);
						for (Account account : accountManager.getAccounts()) {
							String sha1 = Util.sha1(account.name + account.type);
							boolean allowed = PrivacyManager.getSettingBool(appInfo.getUid(),
									PrivacyManager.cSettingAccount + sha1, false, false);
							if (allowed) {
								allowedAccounts = true;
								break;
							}
						}

						// Check if any application allowed
						boolean allowedApplications = false;
						for (ApplicationInfoEx aAppInfo : ApplicationInfoEx.getXApplicationList(ActivityShare.this,
								null))
							for (String packageName : aAppInfo.getPackageName()) {
								boolean allowed = PrivacyManager.getSettingBool(aAppInfo.getUid(),
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
									boolean allowed = PrivacyManager.getSettingBool(appInfo.getUid(),
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
							// TODO: use getRestrictionList
							boolean restricted = PrivacyManager.getRestrictionEx(appInfo.getUid(), restrictionName,
									null).restricted;
							// Category
							long used = PrivacyManager.getUsage(appInfo.getUid(), restrictionName, null);
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
							for (Hook md : PrivacyManager.getHooks(restrictionName)) {
								boolean mRestricted = restricted
										&& PrivacyManager.getRestrictionEx(appInfo.getUid(), restrictionName,
												md.getName()).restricted;
								long mUsed = PrivacyManager.getUsage(appInfo.getUid(), restrictionName, md.getName());
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
						String android_id = Secure
								.getString(ActivityShare.this.getContentResolver(), Secure.ANDROID_ID);

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

						if (mAbort)
							throw new AbortException(ActivityShare.this);

						mAppAdapter.setState(appInfo.getUid(), STATE_RUNNING,
								ActivityShare.this.getString(R.string.menu_submit));

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
								PrivacyManager.setSetting(appInfo.getUid(), PrivacyManager.cSettingState,
										Integer.toString(ActivityMain.STATE_SHARED));
								mAppAdapter.setState(appInfo.getUid(), STATE_SUCCESS, null);
							} else {
								int errno = status.getInt("errno");
								String message = status.getString("error");
								ServerException ex = new ServerException(ActivityShare.this, errno, message);

								// Mark as unregistered
								if (errno == ServerException.cErrorNotActivated) {
									PrivacyManager.setSetting(0, PrivacyManager.cSettingRegistered,
											Boolean.toString(false));
									throw ex;
								} else
									mAppAdapter.setState(appInfo.getUid(), STATE_FAILURE, ex.getLocalizedMessage());
							}
						} else {
							// Failed
							response.getEntity().getContent().close();
							throw new IOException(statusLine.getReasonPhrase());
						}
					} catch (Throwable ex) {
						mAppAdapter.setState(appInfo.getUid(), STATE_FAILURE, ex.getLocalizedMessage());
						throw ex;
					}
				return null;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex;
			}
		}

		@Override
		protected void onProgressUpdate(Integer... values) {
			blueStreakOfProgress(values[0], values[1]);
			super.onProgressUpdate(values);
		}

		@Override
		protected void onPostExecute(Throwable result) {
			if (!ActivityShare.this.isFinishing())
				done(result);
			super.onPostExecute(result);
		}
	}

	public static boolean registerDevice(final ActivityBase context) {
		if (Util.hasProLicense(context) == null
				&& !PrivacyManager.getSettingBool(0, PrivacyManager.cSettingRegistered, false, false)) {
			// Get accounts
			String email = null;
			for (Account account : AccountManager.get(context).getAccounts())
				if ("com.google".equals(account.type)) {
					email = account.name;
					break;
				}

			LayoutInflater LayoutInflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View view = LayoutInflater.inflate(R.layout.register, null);
			final EditText input = (EditText) view.findViewById(R.id.etEmail);
			if (email != null)
				input.setText(email);

			// Build dialog
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);
			alertDialogBuilder.setTitle(R.string.msg_register);
			alertDialogBuilder.setIcon(context.getThemed(R.attr.icon_launcher));
			alertDialogBuilder.setView(view);
			alertDialogBuilder.setPositiveButton(context.getString(android.R.string.ok),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							String email = input.getText().toString();
							if (!"".equals(email))
								new RegisterTask(context).executeOnExecutor(mExecutor, email);
						}
					});
			alertDialogBuilder.setNegativeButton(context.getString(android.R.string.cancel),
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
	private static class RegisterTask extends AsyncTask<String, String, Throwable> {
		private ActivityBase mContext;

		public RegisterTask(ActivityBase context) {
			mContext = context;
		}

		protected Throwable doInBackground(String... params) {
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
						PrivacyManager.setSetting(0, PrivacyManager.cSettingRegistered, Boolean.toString(true));
						return null;
					} else {
						int errno = status.getInt("errno");
						String message = status.getString("error");
						throw new ServerException(errno, message);
					}
				} else {
					// Failed
					response.getEntity().getContent().close();
					throw new IOException(statusLine.getReasonPhrase());
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex;
			}
		}

		@Override
		protected void onPostExecute(Throwable result) {
			if (mContext != null) {
				String message;
				if (result == null)
					message = mContext.getString(R.string.msg_registered);
				else
					message = result.getLocalizedMessage();

				// Build dialog
				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(mContext);
				alertDialogBuilder.setTitle(R.string.app_name);
				alertDialogBuilder.setMessage(message);
				alertDialogBuilder.setIcon(mContext.getThemed(R.attr.icon_launcher));
				alertDialogBuilder.setPositiveButton(mContext.getString(android.R.string.ok),
						new DialogInterface.OnClickListener() {
							@Override
							public void onClick(DialogInterface dialog, int which) {
							}
						});

				// Show dialog
				AlertDialog alertDialog = alertDialogBuilder.create();
				alertDialog.show();
			}
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

		View vShareProgressFull = (View) findViewById(R.id.vShareProgressFull);
		vShareProgressFull.getLayoutParams().width = width;
		vShareProgressFull.invalidate();
	}

	private void done(Throwable ex) {
		String result = null;
		if (ex != null && !(ex instanceof AbortException))
			result = ex.getLocalizedMessage();

		// Check result string and display toast with error
		// TODO: it might be better to put this in a dialog box
		// asking whether to send debugging info
		if (result != null)
			Toast.makeText(this, result, Toast.LENGTH_LONG).show();

		// Reset progress bar
		blueStreakOfProgress(0, 1);
		mRunning = false;

		// Change ok button to "Close"
		final Button btnOk = (Button) findViewById(R.id.btnOk);
		btnOk.setText(getString(R.string.menu_close));
		btnOk.setEnabled(true);
		btnOk.setOnClickListener(new Button.OnClickListener() {
			@Override
			public void onClick(View v) {
				finish();
			}
		});

		// Remove cancel button and separator
		final Button btnCancel = (Button) findViewById(R.id.btnCancel);
		final View vButtonSeparator = findViewById(R.id.vButtonSeparator);
		btnCancel.setVisibility(View.GONE);
		vButtonSeparator.setVisibility(View.GONE);
		// TODO: a nice touch would be to make the cancel button open the main
		// list with only the failed apps in view.
		// I'm not sure what text to put on it though; "Examine failed" might
		// do, if it isn't too long.
	}

	public void fileChooser() {
		Intent chooseFile = new Intent(Intent.ACTION_GET_CONTENT);
		Uri uri = Uri.parse(Environment.getExternalStorageDirectory().getPath() + "/.xprivacy/");
		chooseFile.setDataAndType(uri, "text/xml");
		Intent intent = Intent.createChooser(chooseFile, getString(R.string.app_name));
		startActivityForResult(intent, ACTIVITY_IMPORT_SELECT);
	}

	private void showFileName() {
		TextView tvDescription = (TextView) findViewById(R.id.tvDescription);
		tvDescription.setText(".../" + new File(mFileName).getName());
		Button btnOk = (Button) findViewById(R.id.btnOk);
		btnOk.setEnabled(true);
	}

	public static String getBaseURL(Context context) {
		if (PrivacyManager.getSettingBool(0, PrivacyManager.cSettingHttps, true, true))
			return HTTPS_BASE_URL;
		else
			return HTTP_BASE_URL;
	}

	public static String getFileName(Context context, boolean multiple) {
		File folder = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator
				+ ".xprivacy");
		folder.mkdir();
		String fileName;
		if (multiple) {
			SimpleDateFormat format = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.ROOT);
			fileName = String.format("XPrivacy_%s_%s_%s.xml", Util.getSelfVersionName(context),
					format.format(new Date()), Build.DEVICE);
		} else
			fileName = "XPrivacy.xml";
		return new File(folder + File.separator + fileName).getAbsolutePath();
	}

	// Helper classes

	public static class AbortException extends Exception {
		private static final long serialVersionUID = 1L;

		public AbortException(Context context) {
			super(context.getString(R.string.msg_aborted));
		}
	}

	private static class ServerException extends Exception {
		private int mErrorNo;
		private Context mContext = null;
		private static final long serialVersionUID = 1L;

		public final static int cErrorNotActivated = 206;
		public final static int cErrorNoRestrictions = 305;

		public ServerException(int errorno, String message) {
			super(message);
			mErrorNo = errorno;
		}

		public ServerException(Context context, int errorno, String message) {
			super(message);
			mErrorNo = errorno;
			mContext = context;
		}

		@Override
		@SuppressLint("DefaultLocale")
		public String getLocalizedMessage() {
			if (mErrorNo == cErrorNoRestrictions && mContext != null)
				return mContext.getString(R.string.msg_no_restrictions);
			return String.format("Error %d: %s", mErrorNo, super.getLocalizedMessage());
			// general:
			// 'errno' => 101, 'error' => 'Empty request'
			// 'errno' => 102, 'error' => 'Please upgrade to at least ...'
			// 'errno' => 103, 'error' => 'Error connecting to database'
			// 'errno' => 104, 'error' => 'Unknown action: ...'

			// submit:
			// 'errno' => 201, 'error' => 'Not authorized'
			// 'errno' => 202, 'error' => 'Android ID missing'
			// 'errno' => 203, 'error' => 'Package name missing'
			// 'errno' => 204, 'error' => 'Too many packages for application'
			// 'errno' => 205, 'error' => 'Error storing restrictions'
			// 'errno' => 206, 'error' => 'Device not activated'

			// fetch:
			// 'errno' => 301, 'error' => 'Not authorized'
			// 'errno' => 303, 'error' => 'Package name missing'
			// 'errno' => 304, 'error' => 'Too many packages for application'
			// 'errno' => 305, 'error' => 'No restrictions available'
			// 'errno' => 306, 'error' => 'Error retrieving restrictions'
		}
	}
}

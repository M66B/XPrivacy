package biz.bokhorst.xprivacy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import javax.net.ssl.SSLException;
import javax.xml.parsers.SAXParserFactory;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ConnectTimeoutException;
import org.apache.http.conn.HttpHostConnectException;
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
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.ProgressDialog;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.TypedArray;
import android.database.Cursor;
import android.graphics.Color;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.PowerManager;
import android.os.Process;
import android.provider.ContactsContract;
import android.provider.Settings.Secure;
import android.support.v4.app.NotificationCompat;
import android.support.v7.widget.Toolbar;
import android.text.TextUtils;
import android.util.Log;
import android.util.Patterns;
import android.util.SparseArray;
import android.util.Xml;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.ScrollView;
import android.widget.Spinner;
import android.widget.RadioGroup.OnCheckedChangeListener;
import android.widget.TextView;
import android.widget.EditText;
import android.widget.Toast;

@SuppressLint("Wakelock")
public class ActivityShare extends ActivityBase {
	private int mActionId;
	private AppListAdapter mAppAdapter;
	private SparseArray<AppState> mAppsByUid;
	private boolean mRunning = false;
	private boolean mAbort = false;
	private int mProgressCurrent;
	private int mProgressWidth = 0;
	private String mFileName = null;
	private boolean mInteractive = false;

	private static final int STATE_WAITING = 0;
	private static final int STATE_RUNNING = 1;
	private static final int STATE_SUCCESS = 2;
	private static final int STATE_FAILURE = 3;

	private static final int ACTIVITY_IMPORT_SELECT = 0;

	public static final String cUidList = "UidList";
	public static final String cRestriction = "Restriction";
	public static final String cInteractive = "Interactive";
	public static final String cChoice = "Choice";
	public static final String cFileName = "FileName";
	public static final String HTTP_BASE_URL = "http://crowd.xprivacy.eu/";
	public static final String HTTPS_BASE_URL = "https://crowd.xprivacy.eu/";

	public static final int cSubmitLimit = 10;
	public static final int cProtocolVersion = 4;

	public static final String ACTION_EXPORT = "biz.bokhorst.xprivacy.action.EXPORT";
	public static final String ACTION_IMPORT = "biz.bokhorst.xprivacy.action.IMPORT";
	public static final String ACTION_FETCH = "biz.bokhorst.xprivacy.action.FETCH";
	public static final String ACTION_SUBMIT = "biz.bokhorst.xprivacy.action.SUBMIT";
	public static final String ACTION_TOGGLE = "biz.bokhorst.xprivacy.action.TOGGLE";

	public static final int CHOICE_CLEAR = 1;
	public static final int CHOICE_TEMPLATE = 2;

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

		// Check privacy service client
		if (!PrivacyService.checkClient())
			return;

		// Get data
		int userId = Util.getUserId(Process.myUid());
		final Bundle extras = getIntent().getExtras();
		final String action = getIntent().getAction();
		final int[] uids = (extras != null && extras.containsKey(cUidList) ? extras.getIntArray(cUidList) : new int[0]);
		final String restrictionName = (extras != null ? extras.getString(cRestriction) : null);
		int choice = (extras != null && extras.containsKey(cChoice) ? extras.getInt(cChoice) : -1);
		if (action.equals(ACTION_EXPORT))
			mFileName = (extras != null && extras.containsKey(cFileName) ? extras.getString(cFileName) : null);

		// License check
		if (action.equals(ACTION_IMPORT) || action.equals(ACTION_EXPORT)) {
			if (!Util.isProEnabled() && Util.hasProLicense(this) == null) {
				Util.viewUri(this, ActivityMain.cProUri);
				finish();
				return;
			}
		} else if (action.equals(ACTION_FETCH) || (action.equals(ACTION_TOGGLE) && uids.length > 1)) {
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

		// Check whether we need a user interface
		if (extras != null && extras.containsKey(cInteractive) && extras.getBoolean(cInteractive, false))
			mInteractive = true;

		// Set layout
		setContentView(R.layout.sharelist);
		setSupportActionBar((Toolbar) findViewById(R.id.widgetToolbar));

		// Reference controls
		final TextView tvDescription = (TextView) findViewById(R.id.tvDescription);
		final ScrollView svToggle = (ScrollView) findViewById(R.id.svToggle);
		final RadioGroup rgToggle = (RadioGroup) findViewById(R.id.rgToggle);
		final Spinner spRestriction = (Spinner) findViewById(R.id.spRestriction);
		RadioButton rbClear = (RadioButton) findViewById(R.id.rbClear);
		RadioButton rbTemplateFull = (RadioButton) findViewById(R.id.rbTemplateFull);
		RadioButton rbODEnable = (RadioButton) findViewById(R.id.rbEnableOndemand);
		RadioButton rbODDisable = (RadioButton) findViewById(R.id.rbDisableOndemand);
		final Spinner spTemplate = (Spinner) findViewById(R.id.spTemplate);
		final CheckBox cbClear = (CheckBox) findViewById(R.id.cbClear);
		final Button btnOk = (Button) findViewById(R.id.btnOk);
		final Button btnCancel = (Button) findViewById(R.id.btnCancel);

		// Set title
		if (action.equals(ACTION_TOGGLE)) {
			mActionId = R.string.menu_toggle;
			getSupportActionBar().setSubtitle(R.string.menu_toggle);
		} else if (action.equals(ACTION_IMPORT)) {
			mActionId = R.string.menu_import;
			getSupportActionBar().setSubtitle(R.string.menu_import);
		} else if (action.equals(ACTION_EXPORT)) {
			mActionId = R.string.menu_export;
			getSupportActionBar().setSubtitle(R.string.menu_export);
		} else if (action.equals(ACTION_FETCH)) {
			mActionId = R.string.menu_fetch;
			getSupportActionBar().setSubtitle(R.string.menu_fetch);
		} else if (action.equals(ACTION_SUBMIT)) {
			mActionId = R.string.menu_submit;
			getSupportActionBar().setSubtitle(R.string.menu_submit);
		} else {
			finish();
			return;
		}

		// Get localized restriction name
		List<String> listRestrictionName = new ArrayList<String>(PrivacyManager.getRestrictions(this).navigableKeySet());
		listRestrictionName.add(0, getString(R.string.menu_all));

		// Build restriction adapter
		SpinnerAdapter saRestriction = new SpinnerAdapter(this, android.R.layout.simple_spinner_item);
		saRestriction.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		saRestriction.addAll(listRestrictionName);

		// Setup restriction spinner
		int pos = 0;
		if (restrictionName != null)
			for (String restriction : PrivacyManager.getRestrictions(this).values()) {
				pos++;
				if (restrictionName.equals(restriction))
					break;
			}

		spRestriction.setAdapter(saRestriction);
		spRestriction.setSelection(pos);

		// Build template adapter
		SpinnerAdapter spAdapter = new SpinnerAdapter(this, android.R.layout.simple_spinner_item);
		spAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		String defaultName = PrivacyManager.getSetting(userId, Meta.cTypeTemplateName, "0",
				getString(R.string.title_default));
		spAdapter.add(defaultName);
		for (int i = 1; i <= 4; i++) {
			String alternateName = PrivacyManager.getSetting(userId, Meta.cTypeTemplateName, Integer.toString(i),
					getString(R.string.title_alternate) + " " + i);
			spAdapter.add(alternateName);
		}
		spTemplate.setAdapter(spAdapter);

		// Build application list
		AppListTask appListTask = new AppListTask();
		appListTask.executeOnExecutor(mExecutor, uids);

		// Import/export filename
		if (action.equals(ACTION_EXPORT) || action.equals(ACTION_IMPORT)) {
			// Check for availability of sharing intent
			Intent file = new Intent(Intent.ACTION_GET_CONTENT);
			file.setType("file/*");
			boolean hasIntent = Util.isIntentAvailable(ActivityShare.this, file);

			// Get file name
			if (mFileName == null)
				if (action.equals(ACTION_EXPORT)) {
					String packageName = null;
					if (uids.length == 1)
						try {
							ApplicationInfoEx appInfo = new ApplicationInfoEx(this, uids[0]);
							packageName = appInfo.getPackageName().get(0);
						} catch (Throwable ex) {
							Util.bug(null, ex);
						}
					mFileName = getFileName(this, hasIntent, packageName);
				} else
					mFileName = (hasIntent ? null : getFileName(this, false, null));

			if (mFileName == null)
				fileChooser();
			else
				showFileName();

			if (action.equals(ACTION_IMPORT))
				cbClear.setVisibility(View.VISIBLE);

		} else if (action.equals(ACTION_FETCH)) {
			tvDescription.setText(getBaseURL());
			cbClear.setVisibility(View.VISIBLE);

		} else if (action.equals(ACTION_TOGGLE)) {
			tvDescription.setVisibility(View.GONE);
			spRestriction.setVisibility(View.VISIBLE);
			svToggle.setVisibility(View.VISIBLE);

			// Listen for radio button
			rgToggle.setOnCheckedChangeListener(new OnCheckedChangeListener() {
				@Override
				public void onCheckedChanged(RadioGroup group, int checkedId) {
					btnOk.setEnabled(checkedId >= 0);
					spRestriction.setVisibility(checkedId == R.id.rbEnableOndemand
							|| checkedId == R.id.rbDisableOndemand ? View.GONE : View.VISIBLE);

					spTemplate
							.setVisibility(checkedId == R.id.rbTemplateCategory || checkedId == R.id.rbTemplateFull
									|| checkedId == R.id.rbTemplateMergeSet || checkedId == R.id.rbTemplateMergeReset ? View.VISIBLE
									: View.GONE);
				}
			});

			boolean ondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);
			rbODEnable.setVisibility(ondemand ? View.VISIBLE : View.GONE);
			rbODDisable.setVisibility(ondemand ? View.VISIBLE : View.GONE);

			if (choice == CHOICE_CLEAR)
				rbClear.setChecked(true);
			else if (choice == CHOICE_TEMPLATE)
				rbTemplateFull.setChecked(true);

		} else
			tvDescription.setText(getBaseURL());

		if (mInteractive) {
			// Enable ok
			// (showFileName does this for export/import)
			if (action.equals(ACTION_SUBMIT) || action.equals(ACTION_FETCH))
				btnOk.setEnabled(true);

			// Listen for ok
			btnOk.setOnClickListener(new Button.OnClickListener() {
				@Override
				public void onClick(View v) {
					btnOk.setEnabled(false);

					// Toggle
					if (action.equals(ACTION_TOGGLE)) {
						mRunning = true;
						for (int i = 0; i < rgToggle.getChildCount(); i++)
							((RadioButton) rgToggle.getChildAt(i)).setEnabled(false);
						int pos = spRestriction.getSelectedItemPosition();
						String restrictionName = (pos == 0 ? null : (String) PrivacyManager
								.getRestrictions(ActivityShare.this).values().toArray()[pos - 1]);
						new ToggleTask().executeOnExecutor(mExecutor, restrictionName);

						// Import
					} else if (action.equals(ACTION_IMPORT)) {
						mRunning = true;
						cbClear.setEnabled(false);
						new ImportTask().executeOnExecutor(mExecutor, new File(mFileName), cbClear.isChecked());
					}

					// Export
					else if (action.equals(ACTION_EXPORT)) {
						mRunning = true;
						new ExportTask().executeOnExecutor(mExecutor, new File(mFileName));

						// Fetch
					} else if (action.equals(ACTION_FETCH)) {
						if (uids.length > 0) {
							mRunning = true;
							cbClear.setEnabled(false);
							new FetchTask().executeOnExecutor(mExecutor, cbClear.isChecked());
						}
					}

					// Submit
					else if (action.equals(ACTION_SUBMIT)) {
						if (uids.length > 0) {
							if (uids.length <= cSubmitLimit) {
								mRunning = true;
								new SubmitTask().executeOnExecutor(mExecutor);
							} else {
								String message = getString(R.string.msg_limit, cSubmitLimit + 1);
								Toast.makeText(ActivityShare.this, message, Toast.LENGTH_LONG).show();
								btnOk.setEnabled(false);
							}
						}
					}
				}
			});

		} else
			btnOk.setEnabled(false);

		// Listen for cancel
		btnCancel.setOnClickListener(new Button.OnClickListener() {
			@Override
			public void onClick(View v) {
				if (mRunning) {
					mAbort = true;
					Toast.makeText(ActivityShare.this, getString(R.string.msg_abort), Toast.LENGTH_LONG).show();
				} else
					finish();
			}
		});
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent dataIntent) {
		super.onActivityResult(requestCode, resultCode, dataIntent);

		// Import select
		if (requestCode == ACTIVITY_IMPORT_SELECT)
			if (resultCode == RESULT_CANCELED || dataIntent == null)
				finish();
			else {
				String fileName = dataIntent.getData().getPath();
				mFileName = fileName.replace("/document/primary:", Environment.getExternalStorageDirectory()
						.getAbsolutePath() + File.separatorChar);
				showFileName();
			}
	}

	// State management

	public void setState(int uid, int state, String message) {
		final AppState app = mAppsByUid.get(uid);
		app.message = message;
		app.state = state;
		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				if (mAppAdapter != null) {
					mAppAdapter.notifyDataSetChanged();

					int position = mAppAdapter.getPosition(app);
					if (position >= 0) {
						ListView lvShare = (ListView) findViewById(R.id.lvShare);
						lvShare.smoothScrollToPosition(position);
					}
				}
			}
		});
	}

	public void setState(int uid, int state) {
		AppState app = mAppsByUid.get(uid);
		app.state = state;
	}

	public void setMessage(int uid, String message) {
		AppState app = mAppsByUid.get(uid);
		app.message = message;
	}

	// App info and share state

	private class AppState implements Comparable<AppState> {
		public int state = STATE_WAITING;
		public String message = null;
		public ApplicationInfoEx appInfo;

		public AppState(int uid) {
			appInfo = new ApplicationInfoEx(ActivityShare.this, uid);
		}

		@Override
		public int compareTo(AppState other) {
			return this.appInfo.compareTo(other.appInfo);
		}
	}

	// Adapters

	private class SpinnerAdapter extends ArrayAdapter<String> {
		public SpinnerAdapter(Context context, int textViewResourceId) {
			super(context, textViewResourceId);
		}
	}

	private class AppListAdapter extends ArrayAdapter<AppState> {
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

		public AppListAdapter(Context context, int resource, List<AppState> objects) {
			super(context, resource, objects);
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

		private class ViewHolder {
			private View row;
			private int position;
			public ImageView imgIcon;
			public ImageView imgInfo;
			public TextView tvName;
			public ImageView imgResult;
			public ProgressBar pbRunning;
			public TextView tvMessage;

			public ViewHolder(View theRow, int thePosition) {
				row = theRow;
				position = thePosition;
				imgIcon = (ImageView) row.findViewById(R.id.imgIcon);
				imgInfo = (ImageView) row.findViewById(R.id.imgInfo);
				tvName = (TextView) row.findViewById(R.id.tvApp);
				imgResult = (ImageView) row.findViewById(R.id.imgResult);
				pbRunning = (ProgressBar) row.findViewById(R.id.pbRunning);
				tvMessage = (TextView) row.findViewById(R.id.tvMessage);
			}
		}

		@Override
		@SuppressLint("InflateParams")
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
			final AppState xApp = getItem(holder.position);

			// Set background color
			if (xApp.appInfo.isSystem())
				holder.row.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
			else
				holder.row.setBackgroundColor(Color.TRANSPARENT);

			// Display icon
			holder.imgIcon.setImageDrawable(xApp.appInfo.getIcon(ActivityShare.this));
			holder.imgIcon.setVisibility(View.VISIBLE);

			holder.imgInfo.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					// Packages can be selected on the web site
					Util.viewUri(ActivityShare.this, Uri.parse(String.format(getBaseURL() + "?package_name=%s",
							xApp.appInfo.getPackageName().get(0))));
				}
			});

			// Set app name
			holder.tvName.setText(xApp.appInfo.toString());

			// Show app share state
			if (TextUtils.isEmpty(xApp.message))
				holder.tvMessage.setVisibility(View.GONE);
			else {
				holder.tvMessage.setVisibility(View.VISIBLE);
				holder.tvMessage.setText(xApp.message);
			}
			switch (xApp.state) {
			case STATE_WAITING:
				holder.imgResult.setVisibility(View.GONE);
				holder.pbRunning.setVisibility(View.GONE);
				break;
			case STATE_RUNNING:
				holder.imgResult.setVisibility(View.GONE);
				holder.pbRunning.setVisibility(View.VISIBLE);
				break;
			case STATE_SUCCESS:
				holder.imgResult.setBackgroundResource(R.drawable.btn_check_buttonless_on);
				holder.imgResult.setVisibility(View.VISIBLE);
				holder.pbRunning.setVisibility(View.GONE);
				break;
			case STATE_FAILURE:
				holder.imgResult.setBackgroundResource(R.drawable.indicator_input_error);
				holder.imgResult.setVisibility(View.VISIBLE);
				holder.pbRunning.setVisibility(View.GONE);
				break;
			default:
				Util.log(null, Log.ERROR, "Unknown state=" + xApp.state);
				break;
			}

			return convertView;
		}
	}

	// Tasks

	private class AppListTask extends AsyncTask<int[], Object, List<AppState>> {
		private ProgressDialog mProgressDialog;

		@Override
		protected List<AppState> doInBackground(int[]... params) {
			int[] uids = params[0];
			List<AppState> apps = new ArrayList<AppState>();
			mAppsByUid = new SparseArray<AppState>();

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

			mProgressDialog.setMax(uids.length);
			for (int i = 0; i < uids.length; i++) {
				mProgressDialog.setProgress(i);
				AppState app = new AppState(uids[i]);
				apps.add(app);
				mAppsByUid.put(uids[i], app);
			}

			Collections.sort(apps);
			return apps;
		}

		@SuppressWarnings("deprecation")
		@Override
		protected void onPreExecute() {
			super.onPreExecute();

			TypedArray ta = getTheme().obtainStyledAttributes(new int[] { R.attr.progress_horizontal });
			int progress_horizontal = ta.getResourceId(0, 0);
			ta.recycle();

			// Show progress dialog
			mProgressDialog = new ProgressDialog(ActivityShare.this);
			mProgressDialog.setMessage(getString(R.string.msg_loading));
			mProgressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			mProgressDialog.setProgressDrawable(getResources().getDrawable(progress_horizontal));
			mProgressDialog.setProgressNumberFormat(null);
			mProgressDialog.setCancelable(false);
			mProgressDialog.setCanceledOnTouchOutside(false);
			mProgressDialog.show();
		}

		@Override
		protected void onPostExecute(List<AppState> listApp) {
			if (!ActivityShare.this.isFinishing()) {
				// Display app list
				mAppAdapter = new AppListAdapter(ActivityShare.this, R.layout.shareentry, listApp);
				ListView lvShare = (ListView) findViewById(R.id.lvShare);
				lvShare.setAdapter(mAppAdapter);

				// Dismiss progress dialog
				if (mProgressDialog.isShowing())
					try {
						mProgressDialog.dismiss();
					} catch (IllegalArgumentException ignored) {
					}

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
			// Get wakelock
			PowerManager pm = (PowerManager) ActivityShare.this.getSystemService(Context.POWER_SERVICE);
			PowerManager.WakeLock wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "XPrivacy.Toggle");
			wl.acquire();
			try {
				// Get data
				mProgressCurrent = 0;
				List<Integer> lstUid = mAppAdapter.getListUid();
				final String restrictionName = params[0];
				int actionId = ((RadioGroup) ActivityShare.this.findViewById(R.id.rgToggle)).getCheckedRadioButtonId();
				Spinner spTemplate = ((Spinner) ActivityShare.this.findViewById(R.id.spTemplate));
				String templateName = Meta.cTypeTemplate;
				if (spTemplate.getSelectedItemPosition() > 0)
					templateName = Meta.cTypeTemplate + spTemplate.getSelectedItemPosition();

				for (Integer uid : lstUid)
					try {
						if (mAbort)
							throw new AbortException(ActivityShare.this);

						// Update progess
						publishProgress(++mProgressCurrent, lstUid.size() + 1);
						setState(uid, STATE_RUNNING, null);

						List<Boolean> oldState = PrivacyManager.getRestartStates(uid, restrictionName);

						if (actionId == R.id.rbClear) {
							PrivacyManager.deleteRestrictions(uid, restrictionName, (restrictionName == null));
							if (restrictionName == null) {
								PrivacyManager.deleteUsage(uid);
								PrivacyManager.deleteSettings(uid);
							}
						}

						else if (actionId == R.id.rbRestrict) {
							PrivacyManager.setRestriction(uid, restrictionName, null, true, false);
							PrivacyManager.updateState(uid);
						}

						else if (actionId == R.id.rbTemplateCategory)
							PrivacyManager.applyTemplate(uid, templateName, restrictionName, false, true, false);

						else if (actionId == R.id.rbTemplateFull)
							PrivacyManager.applyTemplate(uid, templateName, restrictionName, true, true, false);

						else if (actionId == R.id.rbTemplateMergeSet)
							PrivacyManager.applyTemplate(uid, templateName, restrictionName, true, false, false);

						else if (actionId == R.id.rbTemplateMergeReset)
							PrivacyManager.applyTemplate(uid, templateName, restrictionName, true, false, true);

						else if (actionId == R.id.rbEnableOndemand) {
							PrivacyManager.setSetting(uid, PrivacyManager.cSettingOnDemand, Boolean.toString(true));
							PrivacyManager.setSetting(uid, PrivacyManager.cSettingNotify, Boolean.toString(false));

						} else if (actionId == R.id.rbDisableOndemand) {
							PrivacyManager.setSetting(uid, PrivacyManager.cSettingOnDemand, Boolean.toString(false));
							PrivacyManager.setSetting(uid, PrivacyManager.cSettingNotify, Boolean.toString(true));

						} else
							Util.log(null, Log.ERROR, "Unknown action=" + actionId);

						List<Boolean> newState = PrivacyManager.getRestartStates(uid, restrictionName);

						setState(uid, STATE_SUCCESS, !newState.equals(oldState) ? getString(R.string.msg_restart)
								: null);
					} catch (Throwable ex) {
						setState(uid, STATE_FAILURE, ex.getMessage());
						return ex;
					}
			} finally {
				wl.release();
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
			// Get wakelock
			PowerManager pm = (PowerManager) ActivityShare.this.getSystemService(Context.POWER_SERVICE);
			PowerManager.WakeLock wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "XPrivacy.Export");
			wl.acquire();

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
					for (PackageInfo pInfo : getPackageManager().getInstalledPackages(0))
						if (listUid.size() == 1 ? pInfo.applicationInfo.uid == listUid.get(0) : true) {
							serializer.startTag(null, "PackageInfo");
							serializer.attribute(null, "Id", Integer.toString(pInfo.applicationInfo.uid));
							serializer.attribute(null, "Name", pInfo.packageName);
							serializer.endTag(null, "PackageInfo");
						}

					// Process global settings
					if (listUid.size() > 1) {
						List<PSetting> listGlobalSetting = PrivacyManager.getSettingList(0, null);
						for (PSetting setting : listGlobalSetting) {
							// Serialize setting
							serializer.startTag(null, "Setting");
							serializer.attribute(null, "Id", "");
							serializer.attribute(null, "Type", setting.type);
							serializer.attribute(null, "Name", setting.name);
							if (setting.value != null)
								serializer.attribute(null, "Value", setting.value);
							serializer.endTag(null, "Setting");
						}
					}

					// Process application settings and restrictions
					for (int uid : listUid)
						try {
							if (mAbort)
								throw new AbortException(ActivityShare.this);

							publishProgress(++mProgressCurrent, listUid.size() + 1);
							setState(uid, STATE_RUNNING, null);

							// Process application settings
							List<PSetting> listAppSetting = PrivacyManager.getSettingList(uid, null);
							for (PSetting setting : listAppSetting) {
								// Bind accounts/contacts to same device
								if (Meta.cTypeAccount.equals(setting.type) || Meta.cTypeContact.equals(setting.type))
									setting.name += "." + android_id;

								// Serialize setting
								serializer.startTag(null, "Setting");
								serializer.attribute(null, "Id", Integer.toString(uid));
								serializer.attribute(null, "Type", setting.type);
								serializer.attribute(null, "Name", setting.name);
								serializer.attribute(null, "Value", setting.value);
								serializer.endTag(null, "Setting");
							}

							// Process restrictions
							for (String restrictionName : PrivacyManager.getRestrictions()) {
								// Category
								PRestriction crestricted = PrivacyManager.getRestrictionEx(uid, restrictionName, null);
								serializer.startTag(null, "Restriction");
								serializer.attribute(null, "Id", Integer.toString(uid));
								serializer.attribute(null, "Name", restrictionName);
								serializer.attribute(null, "Restricted", Boolean.toString(crestricted.restricted));
								serializer.attribute(null, "Asked", Boolean.toString(crestricted.asked));
								serializer.endTag(null, "Restriction");

								// Methods
								for (Hook md : PrivacyManager.getHooks(restrictionName, null)) {
									PRestriction mrestricted = PrivacyManager.getRestrictionEx(uid, restrictionName,
											md.getName());
									if ((crestricted.restricted && !mrestricted.restricted)
											|| (!crestricted.asked && mrestricted.asked) || md.isDangerous()) {
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

							setState(uid, STATE_SUCCESS, null);
						} catch (Throwable ex) {
							setState(uid, STATE_FAILURE, ex.getMessage());
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
			} finally {
				wl.release();
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

	private class ImportTask extends AsyncTask<Object, Integer, Throwable> {
		@Override
		protected Throwable doInBackground(Object... params) {
			// Get wakelock
			PowerManager pm = (PowerManager) ActivityShare.this.getSystemService(Context.POWER_SERVICE);
			PowerManager.WakeLock wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "XPrivacy.Import");
			wl.acquire();
			try {
				// Parameters
				File file = (File) params[0];
				boolean clear = (Boolean) params[1];
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
				Util.log(null, Log.INFO, "Importing " + file);
				FileInputStream fis = null;
				Map<String, Map<String, List<ImportHandler.MethodDescription>>> mapPackage;
				try {
					fis = new FileInputStream(file);
					XMLReader xmlReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
					ImportHandler importHandler = new ImportHandler(clear, listUidSelected, progress);
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

						if (listUidSelected.contains(uid)) {
							Util.log(null, Log.INFO, "Importing " + packageName);
							setState(uid, STATE_RUNNING, null);

							// Reset existing restrictions
							List<Boolean> oldState = PrivacyManager.getRestartStates(uid, null);
							PrivacyManager.deleteRestrictions(uid, null, true);

							// Set imported restrictions
							for (String restrictionName : mapPackage.get(packageName).keySet()) {
								PrivacyManager.setRestriction(uid, restrictionName, null, true, false);
								for (ImportHandler.MethodDescription md : mapPackage.get(packageName).get(
										restrictionName))
									PrivacyManager.setRestriction(uid, restrictionName, md.getMethodName(),
											md.isRestricted(), false);
							}
							PrivacyManager.updateState(uid);
							List<Boolean> newState = PrivacyManager.getRestartStates(uid, null);

							setState(uid, STATE_SUCCESS, !newState.equals(oldState) ? getString(R.string.msg_restart)
									: null);
						}
					} catch (NameNotFoundException ignored) {
					} catch (Throwable ex) {
						if (listUidSelected.contains(uid))
							setState(uid, STATE_FAILURE, ex.getMessage());
						Util.bug(null, ex);
					}
				}

				// Display message
				Util.log(null, Log.INFO, "Importing finished");
				return null;
			} catch (Throwable ex) {
				return ex;
			} finally {
				wl.release();
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

	private class ImportHandler extends DefaultHandler {
		private boolean mClear;
		private List<Integer> mListUidSelected;
		private List<Integer> mListUidSettings = new ArrayList<Integer>();
		private List<Integer> mListUidRestrictions = new ArrayList<Integer>();

		private int lastUid = -1;
		private List<Boolean> mOldState = null;

		private SparseArray<String> mMapId = new SparseArray<String>();
		private Map<String, Integer> mMapUid = new HashMap<String, Integer>();
		private Map<String, Map<String, List<MethodDescription>>> mMapPackage = new HashMap<String, Map<String, List<MethodDescription>>>();

		private Runnable mProgress;
		private String android_id = Secure.getString(getContentResolver(), Secure.ANDROID_ID);

		public ImportHandler(boolean clear, List<Integer> listUidSelected, Runnable progress) {
			mClear = clear;
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
					String type = attributes.getValue("Type");
					String name = attributes.getValue("Name");
					String value = attributes.getValue("Value");

					// Failsafe
					if (name == null)
						return;

					// Do not import version number
					if (name.equals(PrivacyManager.cSettingVersion))
						return;

					// Decode legacy type
					if (name.startsWith("Account.") || name.startsWith("Application.") || name.startsWith("Contact.")
							|| name.startsWith("Template.") || name.startsWith("Whitelist.")) {
						name = name.replace("Whitelist.", "");
						int dot = name.indexOf('.');
						type = name.substring(0, dot);
						name = name.substring(dot + 1);
					} else if (type == null)
						type = "";

					// Import accounts/contacts only for same device
					if (Meta.cTypeAccount.equals(type) || Meta.cTypeContact.equals(type))
						if (name.endsWith("." + android_id))
							name = name.replace("." + android_id, "");
						else
							return;

					if (id == null) {
						// Legacy
						Util.log(null, Log.WARN, "Legacy " + name + "=" + value);
						int userId = Util.getUserId(Process.myUid());
						PrivacyManager.setSetting(userId, name, value);

					} else if ("".equals(id)) {
						// Global setting
						if (mListUidSelected.size() > 1) {
							int userId = Util.getUserId(Process.myUid());
							PrivacyManager.setSetting(userId, type, name, value);
						}

					} else {
						// Application setting
						int iid = Integer.parseInt(id);
						int uid = getUid(iid);
						if (mListUidSelected.contains(uid)) {
							// Check for abort
							if (mAbort && !mListUidSettings.contains(uid)) {
								setState(uid, STATE_FAILURE);
								setMessage(uid, getString(R.string.msg_aborted));
								return;
							}

							// Check for new uid
							if (!mListUidSettings.contains(uid)) {
								// Mark previous as success
								if (lastUid > 0) {
									boolean restart = !PrivacyManager.getRestartStates(lastUid, null).equals(mOldState);
									setState(lastUid, STATE_SUCCESS, restart ? getString(R.string.msg_restart) : null);
								}

								// Update state
								lastUid = uid;
								mListUidSettings.add(uid);

								// Update visible state
								setState(uid, STATE_RUNNING, null);

								// Clear settings
								if (mClear)
									PrivacyManager.deleteSettings(uid);
							}

							PrivacyManager.setSetting(uid, type, name, value);
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
					if (mListUidSelected.contains(uid)) {
						// Check for abort
						if (mAbort && !mListUidRestrictions.contains(uid)) {
							setState(uid, STATE_FAILURE);
							setMessage(uid, getString(R.string.msg_aborted));
							return;
						}

						// Check for new uid
						if (!mListUidRestrictions.contains(uid)) {
							// Mark previous as success
							if (lastUid > 0) {
								PrivacyManager.updateState(lastUid);
								boolean restart = !PrivacyManager.getRestartStates(lastUid, null).equals(mOldState);
								setState(lastUid, STATE_SUCCESS, restart ? getString(R.string.msg_restart) : null);
							}

							// Update state
							lastUid = uid;
							mListUidRestrictions.add(uid);
							mOldState = PrivacyManager.getRestartStates(uid, null);

							// Update visible state
							setState(uid, STATE_RUNNING, null);
							runOnUiThread(mProgress);

							// Clear restrictions
							if (mClear)
								PrivacyManager.deleteRestrictions(uid, null, false);
						}

						// Set restriction
						PrivacyManager.setRestriction(uid, restrictionName, methodName, restricted, asked);
					}
				} else
					Util.log(null, Log.WARN, "Unknown element name=" + qName);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) {
			if (qName.equals("XPrivacy")) {
				if (lastUid > 0) {
					PrivacyManager.updateState(lastUid);
					boolean restart = !PrivacyManager.getRestartStates(lastUid, null).equals(mOldState);
					setState(lastUid, STATE_SUCCESS, restart ? getString(R.string.msg_restart) : null);
				}

				// Cleanup salt
				int userId = Util.getUserId(Process.myUid());
				PrivacyManager.removeLegacySalt(userId);
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

	private class FetchTask extends AsyncTask<Boolean, Integer, Throwable> {
		@Override
		@SuppressLint("DefaultLocale")
		protected Throwable doInBackground(Boolean... params) {
			// Get wakelock
			PowerManager pm = (PowerManager) ActivityShare.this.getSystemService(Context.POWER_SERVICE);
			PowerManager.WakeLock wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "XPrivacy.Fetch");
			wl.acquire();
			try {
				// Get data
				boolean clear = params[0];
				List<ApplicationInfoEx> lstApp = mAppAdapter.getListAppInfo();

				int userId = Util.getUserId(Process.myUid());
				String[] license = Util.getProLicenseUnchecked();
				String android_id = Secure.getString(ActivityShare.this.getContentResolver(), Secure.ANDROID_ID);
				PackageInfo xInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
				String confidence = PrivacyManager.getSetting(userId, PrivacyManager.cSettingConfidence, "");

				// Initialize progress
				mProgressCurrent = 0;

				// Process applications
				for (ApplicationInfoEx appInfo : lstApp)
					try {
						publishProgress(++mProgressCurrent, lstApp.size() + 1);

						if (mAbort)
							throw new AbortException(ActivityShare.this);

						setState(appInfo.getUid(), STATE_RUNNING, ActivityShare.this.getString(R.string.menu_fetch));

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

						HttpPost httpost = new HttpPost(getBaseURL() + "?format=json&action=fetch");
						httpost.setEntity(new ByteArrayEntity(jRoot.toString().getBytes("UTF-8")));
						httpost.setHeader("Accept", "application/json");
						httpost.setHeader("Content-type", "application/json");
						HttpResponse response = httpclient.execute(httpost);
						StatusLine statusLine = response.getStatusLine();

						if (mAbort)
							throw new AbortException(ActivityShare.this);

						setState(appInfo.getUid(), STATE_RUNNING, ActivityShare.this.getString(R.string.msg_applying));

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

								// Clear existing restriction
								if (clear)
									PrivacyManager.deleteRestrictions(appInfo.getUid(), null, true);

								// Set fetched restrictions
								List<PRestriction> listRestriction = new ArrayList<PRestriction>();
								for (int i = 0; i < settings.length(); i++) {
									JSONObject entry = settings.getJSONObject(i);
									String restrictionName = entry.getString("restriction");
									String methodName = entry.has("method") ? entry.getString("method") : null;
									int voted_restricted = entry.getInt("restricted");
									int voted_not_restricted = entry.getInt("not_restricted");
									boolean restricted = (voted_restricted > voted_not_restricted);
									if (clear || restricted)
										listRestriction.add(new PRestriction(appInfo.getUid(), restrictionName,
												methodName, restricted));
								}
								PrivacyManager.setRestrictionList(listRestriction);
								List<Boolean> newState = PrivacyManager.getRestartStates(appInfo.getUid(), null);

								// Mark as new/changed
								PrivacyManager.setSetting(appInfo.getUid(), PrivacyManager.cSettingState,
										Integer.toString(ApplicationInfoEx.STATE_ATTENTION));

								// Change app modification time
								PrivacyManager.setSetting(appInfo.getUid(), PrivacyManager.cSettingModifyTime,
										Long.toString(System.currentTimeMillis()));

								setState(appInfo.getUid(), STATE_SUCCESS,
										!newState.equals(oldState) ? getString(R.string.msg_restart) : null);
							} else {
								int errno = status.getInt("errno");
								String message = status.getString("error");
								ServerException ex = new ServerException(ActivityShare.this, errno, message);
								setState(appInfo.getUid(), STATE_FAILURE, ex.getMessage());
							}
						} else {
							// Failed
							response.getEntity().getContent().close();
							throw new IOException(statusLine.getReasonPhrase());
						}
					} catch (Throwable ex) {
						setState(appInfo.getUid(), STATE_FAILURE, ex.getMessage());
						throw ex;
					}
				return null;
			} catch (ConnectTimeoutException ex) {
				return ex;
			} catch (HttpHostConnectException ex) {
				return ex;
			} catch (SocketTimeoutException ex) {
				return ex;
			} catch (SSLException ex) {
				return ex;
			} catch (UnknownHostException ex) {
				return ex;
			} catch (IOException ex) {
				return ex;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex;
			} finally {
				wl.release();
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
	private class SubmitTask extends AsyncTask<Object, Integer, Throwable> {
		@Override
		protected Throwable doInBackground(Object... params) {
			// Get wakelock
			PowerManager pm = (PowerManager) ActivityShare.this.getSystemService(Context.POWER_SERVICE);
			PowerManager.WakeLock wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "XPrivacy.Submit");
			wl.acquire();
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
						setState(appInfo.getUid(), STATE_RUNNING, ActivityShare.this.getString(R.string.msg_loading));

						// Check if any account allowed
						boolean allowedAccounts = false;
						AccountManager accountManager = AccountManager.get(ActivityShare.this);
						for (Account account : accountManager.getAccounts()) {
							String sha1 = Util.sha1(account.name + account.type);
							boolean allowed = PrivacyManager.getSettingBool(appInfo.getUid(), Meta.cTypeAccount, sha1,
									false);
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
								boolean allowed = PrivacyManager.getSettingBool(-appInfo.getUid(),
										Meta.cTypeApplication, packageName, false);
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
									boolean allowed = PrivacyManager.getSettingBool(-appInfo.getUid(),
											Meta.cTypeContact, Long.toString(id), false);
									if (allowed) {
										allowedContacts = true;
										break;
									}
								}
							} finally {
								cursor.close();
							}

						// Get white lists
						Map<String, TreeMap<String, Boolean>> mapWhitelist = PrivacyManager.listWhitelisted(
								appInfo.getUid(), null);

						// Encode restrictions
						JSONArray jSettings = new JSONArray();
						for (String restrictionName : PrivacyManager.getRestrictions()) {
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
							for (Hook md : PrivacyManager.getHooks(restrictionName, null)) {
								boolean mRestricted = restricted
										&& PrivacyManager.getRestrictionEx(appInfo.getUid(), restrictionName,
												md.getName()).restricted;
								long mUsed = PrivacyManager.getUsage(appInfo.getUid(), restrictionName, md.getName());

								boolean mWhitelisted = false;
								if (md.whitelist() != null && mapWhitelist.containsKey(md.whitelist()))
									for (Boolean allowed : mapWhitelist.get(md.whitelist()).values())
										if (mRestricted ? allowed : !allowed) {
											mWhitelisted = true;
											break;
										}

								JSONObject jMethod = new JSONObject();
								jMethod.put("restriction", restrictionName);
								jMethod.put("method", md.getName());
								jMethod.put("restricted", mRestricted);
								jMethod.put("used", mUsed);
								jMethod.put("allowed", mWhitelisted ? 1 : 0);
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

						setState(appInfo.getUid(), STATE_RUNNING, ActivityShare.this.getString(R.string.menu_submit));

						// Submit
						HttpParams httpParams = new BasicHttpParams();
						HttpConnectionParams.setConnectionTimeout(httpParams, TIMEOUT_MILLISEC);
						HttpConnectionParams.setSoTimeout(httpParams, TIMEOUT_MILLISEC);
						HttpClient httpclient = new DefaultHttpClient(httpParams);

						HttpPost httpost = new HttpPost(getBaseURL() + "?format=json&action=submit");
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
										Integer.toString(ApplicationInfoEx.STATE_SHARED));
								setState(appInfo.getUid(), STATE_SUCCESS, null);
							} else {
								int errno = status.getInt("errno");
								String message = status.getString("error");
								ServerException ex = new ServerException(ActivityShare.this, errno, message);

								// Mark as unregistered
								if (errno == ServerException.cErrorNotActivated) {
									int userId = Util.getUserId(Process.myUid());
									PrivacyManager.setSetting(userId, PrivacyManager.cSettingRegistered,
											Boolean.toString(false));
									throw ex;
								} else
									setState(appInfo.getUid(), STATE_FAILURE, ex.getMessage());
							}
						} else {
							// Failed
							response.getEntity().getContent().close();
							throw new IOException(statusLine.getReasonPhrase());
						}
					} catch (Throwable ex) {
						setState(appInfo.getUid(), STATE_FAILURE, ex.getMessage());
						throw ex;
					}
				return null;
			} catch (ConnectTimeoutException ex) {
				return ex;
			} catch (HttpHostConnectException ex) {
				return ex;
			} catch (SocketTimeoutException ex) {
				return ex;
			} catch (SSLException ex) {
				return ex;
			} catch (UnknownHostException ex) {
				return ex;
			} catch (IOException ex) {
				return ex;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex;
			} finally {
				wl.release();
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

	@SuppressLint("InflateParams")
	public static boolean registerDevice(final ActivityBase context) {
		int userId = Util.getUserId(Process.myUid());
		if (Util.hasProLicense(context) == null
				&& !PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingRegistered, false)) {
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
							if (Patterns.EMAIL_ADDRESS.matcher(email).matches())
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
			// Get wakelock
			PowerManager pm = (PowerManager) mContext.getSystemService(Context.POWER_SERVICE);
			PowerManager.WakeLock wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "XPrivacy.Register");
			wl.acquire();
			try {
				String android_id = Secure.getString(mContext.getContentResolver(), Secure.ANDROID_ID);

				// Encode message
				JSONObject jRoot = new JSONObject();
				jRoot.put("protocol_version", cProtocolVersion);
				jRoot.put("email", params[0]);
				jRoot.put("android_id", Util.md5(android_id).toLowerCase());

				// Submit
				HttpParams httpParams = new BasicHttpParams();
				HttpConnectionParams.setConnectionTimeout(httpParams, TIMEOUT_MILLISEC);
				HttpConnectionParams.setSoTimeout(httpParams, TIMEOUT_MILLISEC);
				HttpClient httpclient = new DefaultHttpClient(httpParams);

				HttpPost httpost = new HttpPost(getBaseURL() + "device?format=json&action=register");
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
						int userId = Util.getUserId(Process.myUid());
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingRegistered, Boolean.toString(true));
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
			} catch (ConnectTimeoutException ex) {
				return ex;
			} catch (HttpHostConnectException ex) {
				return ex;
			} catch (SocketTimeoutException ex) {
				return ex;
			} catch (SSLException ex) {
				return ex;
			} catch (UnknownHostException ex) {
				return ex;
			} catch (IOException ex) {
				return ex;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex;
			} finally {
				wl.release();
			}
		}

		@Override
		protected void onPostExecute(Throwable result) {
			if (!mContext.isFinishing()) {
				String message;
				if (result == null)
					message = mContext.getString(R.string.msg_registered);
				else
					message = result.getMessage();

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

	public static class UpdateTask extends AsyncTask<Object, Object, Object> {
		private Context mContext;
		private NotificationCompat.Builder builder;
		private Notification notification;
		private NotificationManager notificationManager;

		public UpdateTask(Context context) {
			mContext = context;
			builder = new NotificationCompat.Builder(context);
			notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);

		}

		@Override
		protected void onPreExecute() {
			// Build notification
			builder.setSmallIcon(R.drawable.ic_launcher);
			builder.setContentTitle(mContext.getString(R.string.app_name));
			builder.setAutoCancel(false);
			builder.setOngoing(true);
		}

		@Override
		@SuppressLint("DefaultLocale")
		protected Object doInBackground(Object... args) {
			try {
				// Notify
				builder.setContentText(mContext.getString(R.string.title_update_checking));
				builder.setWhen(System.currentTimeMillis());
				notification = builder.build();
				notificationManager.notify(Util.NOTIFY_UPDATE, notification);

				// Encode package
				String[] license = Util.getProLicenseUnchecked();
				int userId = Util.getUserId(Process.myUid());
				boolean test = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingTestVersions, false);
				String android_id = Secure.getString(mContext.getContentResolver(), Secure.ANDROID_ID);

				JSONObject jRoot = new JSONObject();
				jRoot.put("protocol_version", cProtocolVersion);
				jRoot.put("android_id", Util.md5(android_id).toLowerCase());
				jRoot.put("android_sdk", Build.VERSION.SDK_INT);
				jRoot.put("xprivacy_version", Util.getSelfVersionCode(mContext));
				jRoot.put("xprivacy_version_name", Util.getSelfVersionName(mContext));
				jRoot.put("test_versions", test);
				jRoot.put("email", license[1]);
				jRoot.put("signature", license[2]);

				// Update
				HttpParams httpParams = new BasicHttpParams();
				HttpConnectionParams.setConnectionTimeout(httpParams, TIMEOUT_MILLISEC);
				HttpConnectionParams.setSoTimeout(httpParams, TIMEOUT_MILLISEC);
				HttpClient httpclient = new DefaultHttpClient(httpParams);

				HttpPost httpost = new HttpPost(getBaseURL() + "?format=json&action=update");
				httpost.setEntity(new ByteArrayEntity(jRoot.toString().getBytes("UTF-8")));
				httpost.setHeader("Accept", "application/json");
				httpost.setHeader("Content-type", "application/json");
				HttpResponse response = httpclient.execute(httpost);
				StatusLine statusLine = response.getStatusLine();
				if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
					String contentType = response.getFirstHeader("Content-Type").getValue();
					if ("application/octet-stream".equals(contentType)) {
						// Update notification
						builder.setContentText(mContext.getString(R.string.title_update_downloading));
						builder.setWhen(System.currentTimeMillis());
						notification = builder.build();
						notificationManager.notify(Util.NOTIFY_UPDATE, notification);

						// Download APK
						File folder = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS);
						folder.mkdirs();
						String fileName = response.getFirstHeader("Content-Disposition").getElements()[0]
								.getParameterByName("filename").getValue();
						File download = new File(folder, fileName);
						FileOutputStream fos = null;
						try {
							fos = new FileOutputStream(download);
							response.getEntity().writeTo(fos);
						} finally {
							if (fos != null)
								fos.close();
						}

						return download;
					} else if ("application/json".equals(contentType)) {
						ByteArrayOutputStream out = new ByteArrayOutputStream();
						response.getEntity().writeTo(out);
						out.close();
						throw new IOException(out.toString("UTF-8"));
					} else
						throw new IOException(contentType);
				} else
					return statusLine;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex;
			}
		}

		@Override
		protected void onPostExecute(Object result) {
			if (result instanceof StatusLine) {
				notificationManager.cancel(Util.NOTIFY_UPDATE);
				StatusLine status = (StatusLine) result;
				if (status.getStatusCode() == 204) { // No Content
					String none = mContext.getString(R.string.title_update_none);
					Toast.makeText(mContext, none, Toast.LENGTH_LONG).show();
				} else
					Toast.makeText(mContext, status.getStatusCode() + " " + status.getReasonPhrase(), Toast.LENGTH_LONG)
							.show();

			} else if (result instanceof Throwable) {
				notificationManager.cancel(Util.NOTIFY_UPDATE);
				Throwable ex = (Throwable) result;
				Toast.makeText(mContext, ex.toString(), Toast.LENGTH_LONG).show();

			} else {
				File download = (File) result;
				Intent intent = new Intent(Intent.ACTION_VIEW);
				intent.setDataAndType(Uri.fromFile(download), "application/vnd.android.package-archive");

				PendingIntent pi = PendingIntent.getActivity(mContext, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT);

				// Update notification
				builder.setContentText(mContext.getString(R.string.title_update_install));
				builder.setWhen(System.currentTimeMillis());
				builder.setAutoCancel(true);
				builder.setOngoing(false);
				builder.setContentIntent(pi);
				notification = builder.build();
				notificationManager.notify(Util.NOTIFY_UPDATE, notification);
			}

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
		vShareProgressFull.requestLayout();
	}

	private void done(Throwable ex) {
		String result = null;
		if (ex != null && !(ex instanceof AbortException))
			result = ex.getMessage();

		// Check result string and display toast with error
		if (result != null)
			Toast.makeText(this, result, Toast.LENGTH_LONG).show();

		// Reset progress bar
		blueStreakOfProgress(0, 1);
		mRunning = false;

		// Update buttons
		final Button btnCancel = (Button) findViewById(R.id.btnCancel);
		final Button btnOk = (Button) findViewById(R.id.btnOk);
		btnCancel.setEnabled(false);
		btnOk.setEnabled(true);

		// Handle close
		btnOk.setOnClickListener(new Button.OnClickListener() {
			@Override
			public void onClick(View v) {
				finish();
			}
		});
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
		tvDescription.setText(mFileName);
		Button btnOk = (Button) findViewById(R.id.btnOk);
		btnOk.setEnabled(true);
	}

	public static String getBaseURL() {
		int userId = Util.getUserId(Process.myUid());
		if (PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingHttps, true))
			return HTTPS_BASE_URL;
		else
			return HTTP_BASE_URL;
	}

	public static String getFileName(Context context, boolean multiple, String packageName) {
		File folder = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator
				+ ".xprivacy");
		folder.mkdir();
		String fileName;
		if (multiple) {
			SimpleDateFormat format = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.ROOT);
			fileName = String.format("%s_XPrivacy_%s_%s%s.xml", format.format(new Date()),
					Util.getSelfVersionName(context), Build.DEVICE, (packageName == null ? "" : "_" + packageName));
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

	public static class ServerException extends Exception {
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
		public String getMessage() {
			if (mErrorNo == cErrorNoRestrictions && mContext != null)
				return mContext.getString(R.string.msg_no_restrictions);
			return String.format("Error %d: %s", mErrorNo, super.getMessage());
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
			// 'errno' => 207, 'error' => 'Unknown category'

			// fetch:
			// 'errno' => 301, 'error' => 'Not authorized'
			// 'errno' => 303, 'error' => 'Package name missing'
			// 'errno' => 304, 'error' => 'Too many packages for application'
			// 'errno' => 305, 'error' => 'No restrictions available'
			// 'errno' => 306, 'error' => 'Error retrieving restrictions'
			// 'errno' => 307, 'error' => 'There is a maximum of ...'
		}
	}
}

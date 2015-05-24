package biz.bokhorst.xprivacy;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Paint.Style;
import android.graphics.Typeface;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Process;
import android.support.v4.view.MenuItemCompat;
import android.support.v7.widget.Toolbar;
import android.text.TextUtils;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.view.Window;
import android.view.WindowManager;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.BaseExpandableListAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.EditText;
import android.widget.ExpandableListView;
import android.widget.Filter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RadioGroup;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.SearchView;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;

public class ActivityMain extends ActivityBase implements OnItemSelectedListener {
	private Spinner spRestriction = null;
	private AppListAdapter mAppAdapter = null;

	private String searchQuery = "";
	private int mSortMode;
	private boolean mSortInvert;
	private int mProgressWidth = 0;
	private int mProgress = 0;

	private Handler mProHandler = new Handler();

	private static final int SORT_BY_NAME = 0;
	private static final int SORT_BY_UID = 1;
	private static final int SORT_BY_INSTALL_TIME = 2;
	private static final int SORT_BY_UPDATE_TIME = 3;
	private static final int SORT_BY_MODIFY_TIME = 4;
	private static final int SORT_BY_STATE = 5;

	private static final int ACTIVITY_LICENSE = 0;
	private static final int LICENSED = 0x0100;
	private static final int NOT_LICENSED = 0x0231;
	private static final int RETRY = 0x0123;

	private static final int ERROR_CONTACTING_SERVER = 0x101;
	private static final int ERROR_INVALID_PACKAGE_NAME = 0x102;
	private static final int ERROR_NON_MATCHING_UID = 0x103;

	public static final String cAction = "Action";
	public static final int cActionRefresh = 1;

	public static final Uri cProUri = Uri.parse("http://www.xprivacy.eu/");

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

	private Comparator<ApplicationInfoEx> mSorter = new Comparator<ApplicationInfoEx>() {
		@Override
		public int compare(ApplicationInfoEx appInfo0, ApplicationInfoEx appInfo1) {
			int sortOrder = mSortInvert ? -1 : 1;
			switch (mSortMode) {
			case SORT_BY_NAME:
				return sortOrder * appInfo0.compareTo(appInfo1);
			case SORT_BY_UID:
				// Default lowest first
				return sortOrder * (appInfo0.getUid() - appInfo1.getUid());
			case SORT_BY_INSTALL_TIME:
				// Default newest first
				Long iTime0 = appInfo0.getInstallTime(ActivityMain.this);
				Long iTime1 = appInfo1.getInstallTime(ActivityMain.this);
				return sortOrder * iTime1.compareTo(iTime0);
			case SORT_BY_UPDATE_TIME:
				// Default newest first
				Long uTime0 = appInfo0.getUpdateTime(ActivityMain.this);
				Long uTime1 = appInfo1.getUpdateTime(ActivityMain.this);
				return sortOrder * uTime1.compareTo(uTime0);
			case SORT_BY_MODIFY_TIME:
				// Default newest first
				Long mTime0 = appInfo0.getModificationTime(ActivityMain.this);
				Long mTime1 = appInfo1.getModificationTime(ActivityMain.this);
				return sortOrder * mTime1.compareTo(mTime0);
			case SORT_BY_STATE:
				Integer state0 = appInfo0.getState(ActivityMain.this);
				Integer state1 = appInfo1.getState(ActivityMain.this);
				if (state0.compareTo(state1) == 0)
					return sortOrder * appInfo0.compareTo(appInfo1);
				else
					return sortOrder * state0.compareTo(state1);
			}
			return 0;
		}
	};

	private boolean mPackageChangeReceiverRegistered = false;

	private BroadcastReceiver mPackageChangeReceiver = new BroadcastReceiver() {
		@Override
		public void onReceive(Context context, Intent intent) {
			ActivityMain.this.recreate();
		}
	};

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		final int userId = Util.getUserId(Process.myUid());

		// Check privacy service client
		if (!PrivacyService.checkClient())
			return;

		// Import license file
		if (Intent.ACTION_VIEW.equals(getIntent().getAction()))
			if (Util.importProLicense(new File(getIntent().getData().getPath())) != null)
				Toast.makeText(this, getString(R.string.menu_pro), Toast.LENGTH_LONG).show();

		// Set layout
		setContentView(R.layout.mainlist);
		setSupportActionBar((Toolbar) findViewById(R.id.widgetToolbar));
		getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_HIDDEN);

		// Set sub title
		if (Util.hasProLicense(this) != null)
			getSupportActionBar().setSubtitle(R.string.menu_pro);

		// Annotate
		Meta.annotate(this.getResources());

		// Get localized restriction name
		List<String> listRestrictionName = new ArrayList<String>(PrivacyManager.getRestrictions(this).navigableKeySet());
		listRestrictionName.add(0, getString(R.string.menu_all));

		// Build spinner adapter
		SpinnerAdapter spAdapter = new SpinnerAdapter(this, android.R.layout.simple_spinner_item);
		spAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spAdapter.addAll(listRestrictionName);

		// Handle info
		ImageView imgInfo = (ImageView) findViewById(R.id.imgInfo);
		imgInfo.setOnClickListener(new View.OnClickListener() {
			@SuppressLint("SetJavaScriptEnabled")
			@Override
			public void onClick(View view) {
				int position = spRestriction.getSelectedItemPosition();
				if (position != AdapterView.INVALID_POSITION) {
					String query = (position == 0 ? "restrictions" : (String) PrivacyManager
							.getRestrictions(ActivityMain.this).values().toArray()[position - 1]);

					WebView webview = new WebView(ActivityMain.this);
					webview.getSettings().setUserAgentString("Mozilla/5.0");
					// needed for hashtag
					webview.getSettings().setJavaScriptEnabled(true);
					webview.loadUrl("https://github.com/M66B/XPrivacy#" + query);

					AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityMain.this);
					alertDialogBuilder.setTitle((String) spRestriction.getSelectedItem());
					alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
					alertDialogBuilder.setView(webview);
					alertDialogBuilder.setCancelable(true);
					AlertDialog alertDialog = alertDialogBuilder.create();
					alertDialog.show();
				}
			}
		});

		// Setup category spinner
		spRestriction = (Spinner) findViewById(R.id.spRestriction);
		spRestriction.setAdapter(spAdapter);
		spRestriction.setOnItemSelectedListener(this);
		int pos = getSelectedCategory(userId);
		spRestriction.setSelection(pos);

		// Setup sort
		mSortMode = Integer.parseInt(PrivacyManager.getSetting(userId, PrivacyManager.cSettingSortMode, "0"));
		mSortInvert = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingSortInverted, false);

		// Start task to get app list
		AppListTask appListTask = new AppListTask();
		appListTask.executeOnExecutor(mExecutor, (Object) null);

		// Check environment
		Requirements.check(this);

		// Licensing
		checkLicense();

		// Listen for package add/remove
		IntentFilter iff = new IntentFilter();
		iff.addAction(Intent.ACTION_PACKAGE_ADDED);
		iff.addAction(Intent.ACTION_PACKAGE_REMOVED);
		iff.addDataScheme("package");
		registerReceiver(mPackageChangeReceiver, iff);
		mPackageChangeReceiverRegistered = true;

		boolean showChangelog = true;

		// First run
		if (PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFirstRun, true)) {
			showChangelog = false;
			optionAbout();
		}

		// Tutorial
		if (!PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingTutorialMain, false)) {
			showChangelog = false;
			((ScrollView) findViewById(R.id.svTutorialHeader)).setVisibility(View.VISIBLE);
			((ScrollView) findViewById(R.id.svTutorialDetails)).setVisibility(View.VISIBLE);
		}
		View.OnClickListener listener = new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				ViewParent parent = view.getParent();
				while (!parent.getClass().equals(ScrollView.class))
					parent = parent.getParent();
				((View) parent).setVisibility(View.GONE);
				PrivacyManager.setSetting(userId, PrivacyManager.cSettingTutorialMain, Boolean.TRUE.toString());
			}
		};
		((Button) findViewById(R.id.btnTutorialHeader)).setOnClickListener(listener);
		((Button) findViewById(R.id.btnTutorialDetails)).setOnClickListener(listener);

		// Legacy
		if (!PrivacyManager.cVersion3) {
			long now = new Date().getTime();
			String legacy = PrivacyManager.getSetting(userId, PrivacyManager.cSettingLegacy, null);
			if (legacy == null || now > Long.parseLong(legacy) + 7 * 24 * 60 * 60 * 1000L) {
				showChangelog = false;
				PrivacyManager.setSetting(userId, PrivacyManager.cSettingLegacy, Long.toString(now));

				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
				alertDialogBuilder.setTitle(R.string.app_name);
				alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
				alertDialogBuilder.setMessage(R.string.title_update_legacy);
				alertDialogBuilder.setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						Util.viewUri(ActivityMain.this,
								Uri.parse("https://github.com/M66B/XPrivacy/blob/master/CHANGELOG.md#xprivacy3"));
					}
				});
				alertDialogBuilder.setNegativeButton(android.R.string.cancel, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						// Do nothing
					}
				});

				// Show dialog
				AlertDialog alertDialog = alertDialogBuilder.create();
				alertDialog.show();
			}
		}

		// Show changelog
		if (showChangelog) {
			String sVersion = PrivacyManager.getSetting(userId, PrivacyManager.cSettingChangelog, null);
			Version changelogVersion = new Version(sVersion == null ? "0.0" : sVersion);
			Version currentVersion = new Version(Util.getSelfVersionName(this));
			if (sVersion == null || changelogVersion.compareTo(currentVersion) < 0)
				optionChangelog();
		}
	}

	@Override
	protected void onResume() {
		super.onResume();

		// Update category selection
		if (spRestriction != null) {
			int userId = Util.getUserId(Process.myUid());
			int pos = getSelectedCategory(userId);
			spRestriction.setSelection(pos);
		}

		// Update list
		if (mAppAdapter != null)
			mAppAdapter.notifyDataSetChanged();
	}

	@Override
	protected void onNewIntent(Intent intent) {
		// Handle clear XPrivacy data (needs UI refresh)
		Bundle extras = intent.getExtras();
		if (extras != null && extras.containsKey(cAction) && extras.getInt(cAction) == cActionRefresh)
			recreate();
		else {
			// Refresh application list
			if (mAppAdapter != null)
				mAppAdapter.notifyDataSetChanged();

			// Import pro license
			if (Intent.ACTION_VIEW.equals(intent.getAction()))
				Util.importProLicense(new File(intent.getData().getPath()));
		}
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();

		if (mPackageChangeReceiverRegistered) {
			unregisterReceiver(mPackageChangeReceiver);
			mPackageChangeReceiverRegistered = false;
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent dataIntent) {
		super.onActivityResult(requestCode, resultCode, dataIntent);

		if (requestCode == ACTIVITY_LICENSE) {
			// License check
			if (dataIntent != null) {
				int code = dataIntent.getIntExtra("Code", -1);
				int reason = dataIntent.getIntExtra("Reason", -1);

				String sReason;
				if (reason == LICENSED)
					sReason = "LICENSED";
				else if (reason == NOT_LICENSED)
					sReason = "NOT_LICENSED";
				else if (reason == RETRY)
					sReason = "RETRY";
				else if (reason == ERROR_CONTACTING_SERVER)
					sReason = "ERROR_CONTACTING_SERVER";
				else if (reason == ERROR_INVALID_PACKAGE_NAME)
					sReason = "ERROR_INVALID_PACKAGE_NAME";
				else if (reason == ERROR_NON_MATCHING_UID)
					sReason = "ERROR_NON_MATCHING_UID";
				else
					sReason = Integer.toString(reason);

				Util.log(null, Log.WARN, "Licensing: code=" + code + " reason=" + sReason);

				if (code > 0) {
					Util.setPro(true);
					invalidateOptionsMenu();
					Toast.makeText(this, getString(R.string.menu_pro), Toast.LENGTH_LONG).show();
				} else if (reason == RETRY) {
					Util.setPro(false);
					mProHandler.postDelayed(new Runnable() {
						@Override
						public void run() {
							checkLicense();
						}
					}, 30 * 1000);
				}
			}
		}
	}

	// Filtering

	@Override
	public void onItemSelected(AdapterView<?> parent, View view, int pos, long id) {
		selectRestriction(pos);
	}

	@Override
	public void onNothingSelected(AdapterView<?> parent) {
		selectRestriction(0);
	}

	private void selectRestriction(int pos) {
		if (mAppAdapter != null) {
			int userId = Util.getUserId(Process.myUid());
			String restrictionName = (pos == 0 ? null : (String) PrivacyManager.getRestrictions(this).values()
					.toArray()[pos - 1]);
			mAppAdapter.setRestrictionName(restrictionName);
			PrivacyManager.setSetting(userId, PrivacyManager.cSettingSelectedCategory, restrictionName);
			applyFilter();
		}
	}

	private void applyFilter() {
		if (mAppAdapter != null) {
			ProgressBar pbFilter = (ProgressBar) findViewById(R.id.pbFilter);
			TextView tvStats = (TextView) findViewById(R.id.tvStats);
			TextView tvState = (TextView) findViewById(R.id.tvState);

			// Get settings
			int userId = Util.getUserId(Process.myUid());
			boolean fUsed = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFUsed, false);
			boolean fInternet = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFInternet, false);
			boolean fRestriction = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFRestriction, false);
			boolean fRestrictionNot = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFRestrictionNot,
					false);
			boolean fPermission = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFPermission, true);
			boolean fOnDemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFOnDemand, false);
			boolean fOnDemandNot = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFOnDemandNot, false);
			boolean fUser = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFUser, true);
			boolean fSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFSystem, false);

			String filter = String.format("%s\n%b\n%b\n%b\n%b\n%b\n%b\n%b\n%b\n%b", searchQuery, fUsed, fInternet,
					fRestriction, fRestrictionNot, fPermission, fOnDemand, fOnDemandNot, fUser, fSystem);
			pbFilter.setVisibility(ProgressBar.VISIBLE);
			tvStats.setVisibility(TextView.GONE);

			// Adjust progress state width
			RelativeLayout.LayoutParams tvStateLayout = (RelativeLayout.LayoutParams) tvState.getLayoutParams();
			tvStateLayout.addRule(RelativeLayout.LEFT_OF, R.id.pbFilter);

			mAppAdapter.getFilter().filter(filter);
		}
	}

	private void applySort() {
		if (mAppAdapter != null)
			mAppAdapter.sort();
	}

	// Options

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		if (inflater != null && PrivacyService.checkClient()) {
			// Inflate menu
			inflater.inflate(R.menu.main, menu);

			// Searchable
			SearchView searchView = (SearchView) MenuItemCompat.getActionView(menu.findItem(R.id.menu_search));
			if (searchView != null) {
				searchView.setIconifiedByDefault(false);

				searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
					@Override
					public boolean onQueryTextChange(String newText) {
						searchQuery = newText;
						applyFilter();
						return true;
					}

					@Override
					public boolean onQueryTextSubmit(String query) {
						searchQuery = query;
						applyFilter();
						return true;
					}
				});
				searchView.setOnCloseListener(new SearchView.OnCloseListener() {
					@Override
					public boolean onClose() {
						searchQuery = "";
						applyFilter();
						return true;
					}
				});
			}

			return true;
		} else
			return false;
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		int userId = Util.getUserId(Process.myUid());
		boolean mounted = Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState());
		boolean updates = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingUpdates, false);

		menu.findItem(R.id.menu_export).setEnabled(mounted);
		menu.findItem(R.id.menu_import).setEnabled(mounted);

		menu.findItem(R.id.menu_submit).setEnabled(Util.hasValidFingerPrint(this));

		menu.findItem(R.id.menu_pro).setVisible(!Util.isProEnabled() && Util.hasProLicense(this) == null);

		menu.findItem(R.id.menu_dump).setVisible(Util.isDebuggable(this));

		menu.findItem(R.id.menu_update).setVisible(updates);
		menu.findItem(R.id.menu_update).setEnabled(mounted);

		// Update filter count

		// Get settings
		boolean fUsed = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFUsed, false);
		boolean fInternet = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFInternet, false);
		boolean fRestriction = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFRestriction, false);
		boolean fPermission = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFPermission, true);
		boolean fOnDemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFOnDemand, false);
		boolean fUser = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFUser, true);
		boolean fSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFSystem, false);

		// Count number of active filters
		int numberOfFilters = 0;
		if (fUsed)
			numberOfFilters++;
		if (fInternet)
			numberOfFilters++;
		if (fRestriction)
			numberOfFilters++;
		if (fPermission)
			numberOfFilters++;
		if (fOnDemand)
			numberOfFilters++;
		if (fUser)
			numberOfFilters++;
		if (fSystem)
			numberOfFilters++;

		if (numberOfFilters > 0) {
			Bitmap bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.icon_filter).copy(
					Bitmap.Config.ARGB_8888, true);

			Paint paint = new Paint();
			paint.setStyle(Style.FILL);
			paint.setColor(Color.GRAY);
			paint.setTextSize(bitmap.getWidth() / 3);
			paint.setTypeface(Typeface.defaultFromStyle(Typeface.BOLD));

			String text = Integer.toString(numberOfFilters);

			Canvas canvas = new Canvas(bitmap);
			canvas.drawText(text, bitmap.getWidth() - paint.measureText(text), bitmap.getHeight(), paint);

			MenuItem fMenu = menu.findItem(R.id.menu_filter);
			fMenu.setIcon(new BitmapDrawable(getResources(), bitmap));
		}

		return super.onPrepareOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		try {
			switch (item.getItemId()) {
			case R.id.menu_sort:
				optionSort();
				return true;
			case R.id.menu_filter:
				optionFilter();
				return true;
			case R.id.menu_usage:
				optionUsage();
				return true;
			case R.id.menu_template:
				optionTemplate();
				return true;
			case R.id.menu_select_all:
				optionSelectAll();
				return true;
			case R.id.menu_toggle:
				optionToggle();
				return true;
			case R.id.menu_export:
				optionExport();
				return true;
			case R.id.menu_import:
				optionImport();
				return true;
			case R.id.menu_submit:
				optionSubmit();
				return true;
			case R.id.menu_fetch:
				optionFetch();
				return true;
			case R.id.menu_pro:
				optionPro();
				return true;
			case R.id.menu_theme:
				optionSwitchTheme();
				return true;
			case R.id.menu_settings:
				optionSettings();
				return true;
			case R.id.menu_dump:
				optionDump();
				return true;
			case R.id.menu_legend:
				optionLegend();
				return true;
			case R.id.menu_tutorial:
				optionTutorial();
				return true;
			case R.id.menu_changelog:
				optionChangelog();
				return true;
			case R.id.menu_update:
				optionUpdate();
				return true;
			case R.id.menu_report:
				optionReportIssue();
				return true;
			case R.id.menu_about:
				optionAbout();
				return true;
			default:
				return super.onOptionsItemSelected(item);
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return true;
		}
	}

	@SuppressLint("InflateParams")
	private void optionSort() {
		LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		View view = inflater.inflate(R.layout.sort, null);
		final RadioGroup rgSMode = (RadioGroup) view.findViewById(R.id.rgSMode);
		final CheckBox cbSInvert = (CheckBox) view.findViewById(R.id.cbSInvert);

		// Initialise controls
		switch (mSortMode) {
		case SORT_BY_NAME:
			rgSMode.check(R.id.rbSName);
			break;
		case SORT_BY_UID:
			rgSMode.check(R.id.rbSUid);
			break;
		case SORT_BY_INSTALL_TIME:
			rgSMode.check(R.id.rbSInstalled);
			break;
		case SORT_BY_UPDATE_TIME:
			rgSMode.check(R.id.rbSUpdated);
			break;
		case SORT_BY_MODIFY_TIME:
			rgSMode.check(R.id.rbSModified);
			break;
		case SORT_BY_STATE:
			rgSMode.check(R.id.rbSState);
			break;
		}
		cbSInvert.setChecked(mSortInvert);

		// Build dialog
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityMain.this);
		alertDialogBuilder.setTitle(R.string.menu_sort);
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
		alertDialogBuilder.setView(view);
		alertDialogBuilder.setPositiveButton(ActivityMain.this.getString(android.R.string.ok),
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						switch (rgSMode.getCheckedRadioButtonId()) {
						case R.id.rbSName:
							mSortMode = SORT_BY_NAME;
							break;
						case R.id.rbSUid:
							mSortMode = SORT_BY_UID;
							break;
						case R.id.rbSInstalled:
							mSortMode = SORT_BY_INSTALL_TIME;
							break;
						case R.id.rbSUpdated:
							mSortMode = SORT_BY_UPDATE_TIME;
							break;
						case R.id.rbSModified:
							mSortMode = SORT_BY_MODIFY_TIME;
							break;
						case R.id.rbSState:
							mSortMode = SORT_BY_STATE;
							break;
						}
						mSortInvert = cbSInvert.isChecked();

						int userId = Util.getUserId(Process.myUid());
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingSortMode, Integer.toString(mSortMode));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingSortInverted,
								Boolean.toString(mSortInvert));

						applySort();
					}
				});
		alertDialogBuilder.setNegativeButton(ActivityMain.this.getString(android.R.string.cancel),
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						// Do nothing
					}
				});

		// Show dialog
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	@SuppressLint("InflateParams")
	private void optionFilter() {
		LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		View view = inflater.inflate(R.layout.filters, null);
		final CheckBox cbFUsed = (CheckBox) view.findViewById(R.id.cbFUsed);
		final CheckBox cbFInternet = (CheckBox) view.findViewById(R.id.cbFInternet);
		final CheckBox cbFPermission = (CheckBox) view.findViewById(R.id.cbFPermission);
		final CheckBox cbFRestriction = (CheckBox) view.findViewById(R.id.cbFRestriction);
		final CheckBox cbFRestrictionNot = (CheckBox) view.findViewById(R.id.cbFRestrictionNot);
		final CheckBox cbFOnDemand = (CheckBox) view.findViewById(R.id.cbFOnDemand);
		final CheckBox cbFOnDemandNot = (CheckBox) view.findViewById(R.id.cbFOnDemandNot);
		final CheckBox cbFUser = (CheckBox) view.findViewById(R.id.cbFUser);
		final CheckBox cbFSystem = (CheckBox) view.findViewById(R.id.cbFSystem);
		final Button btnDefault = (Button) view.findViewById(R.id.btnDefault);

		// Get settings
		final int userId = Util.getUserId(Process.myUid());
		boolean fUsed = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFUsed, false);
		boolean fInternet = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFInternet, false);
		boolean fPermission = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFPermission, true);
		boolean fRestriction = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFRestriction, false);
		boolean fRestrictionNot = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFRestrictionNot, false);
		boolean fOnDemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFOnDemand, false);
		boolean fOnDemandNot = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFOnDemandNot, false);
		boolean fUser = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFUser, true);
		boolean fSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFSystem, false);

		boolean ondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);

		// Setup checkboxes
		cbFUsed.setChecked(fUsed);
		cbFInternet.setChecked(fInternet);
		cbFPermission.setChecked(fPermission);
		cbFRestriction.setChecked(fRestriction);
		cbFRestrictionNot.setChecked(fRestrictionNot);
		cbFOnDemand.setChecked(fOnDemand && ondemand);
		cbFOnDemandNot.setChecked(fOnDemandNot && ondemand);
		cbFUser.setChecked(fUser);
		cbFSystem.setChecked(fSystem);

		cbFRestrictionNot.setEnabled(fRestriction);

		cbFOnDemand.setEnabled(ondemand);
		cbFOnDemandNot.setEnabled(fOnDemand && ondemand);

		// Manage user/system filter exclusivity
		OnCheckedChangeListener checkListener = new OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				if (buttonView == cbFUser) {
					if (isChecked)
						cbFSystem.setChecked(false);
				} else if (buttonView == cbFSystem) {
					if (isChecked)
						cbFUser.setChecked(false);
				} else if (buttonView == cbFRestriction)
					cbFRestrictionNot.setEnabled(cbFRestriction.isChecked());
				else if (buttonView == cbFOnDemand)
					cbFOnDemandNot.setEnabled(cbFOnDemand.isChecked());
			}
		};
		cbFUser.setOnCheckedChangeListener(checkListener);
		cbFSystem.setOnCheckedChangeListener(checkListener);
		cbFRestriction.setOnCheckedChangeListener(checkListener);
		cbFOnDemand.setOnCheckedChangeListener(checkListener);

		// Clear button
		btnDefault.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View arg0) {
				cbFUsed.setChecked(false);
				cbFInternet.setChecked(false);
				cbFPermission.setChecked(true);
				cbFRestriction.setChecked(false);
				cbFRestrictionNot.setChecked(false);
				cbFOnDemand.setChecked(false);
				cbFOnDemandNot.setChecked(false);
				cbFUser.setChecked(true);
				cbFSystem.setChecked(false);
			}
		});

		// Build dialog
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityMain.this);
		alertDialogBuilder.setTitle(R.string.menu_filter);
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
		alertDialogBuilder.setView(view);
		alertDialogBuilder.setPositiveButton(ActivityMain.this.getString(android.R.string.ok),
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFUsed,
								Boolean.toString(cbFUsed.isChecked()));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFInternet,
								Boolean.toString(cbFInternet.isChecked()));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFRestriction,
								Boolean.toString(cbFRestriction.isChecked()));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFRestrictionNot,
								Boolean.toString(cbFRestrictionNot.isChecked()));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFPermission,
								Boolean.toString(cbFPermission.isChecked()));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFOnDemand,
								Boolean.toString(cbFOnDemand.isChecked()));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFOnDemandNot,
								Boolean.toString(cbFOnDemandNot.isChecked()));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFUser,
								Boolean.toString(cbFUser.isChecked()));
						PrivacyManager.setSetting(userId, PrivacyManager.cSettingFSystem,
								Boolean.toString(cbFSystem.isChecked()));

						invalidateOptionsMenu();
						applyFilter();
					}
				});
		alertDialogBuilder.setNegativeButton(ActivityMain.this.getString(android.R.string.cancel),
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
					}
				});

		// Show dialog
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private void optionUsage() {
		Intent intent = new Intent(this, ActivityUsage.class);
		if (mAppAdapter != null && mAppAdapter.getRestrictionName() != null)
			intent.putExtra(ActivityUsage.cRestriction, mAppAdapter.getRestrictionName());
		startActivity(intent);
	}

	@SuppressLint("InflateParams")
	private void optionTemplate() {
		final int userId = Util.getUserId(Process.myUid());

		// Build view
		LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		View view = inflater.inflate(R.layout.template, null);
		final Spinner spTemplate = (Spinner) view.findViewById(R.id.spTemplate);
		Button btnRename = (Button) view.findViewById(R.id.btnRename);
		ExpandableListView elvTemplate = (ExpandableListView) view.findViewById(R.id.elvTemplate);

		// Template selector
		final SpinnerAdapter spAdapter = new SpinnerAdapter(this, android.R.layout.simple_spinner_item);
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

		// Template definition
		final TemplateListAdapter templateAdapter = new TemplateListAdapter(this, view, R.layout.templateentry);
		elvTemplate.setAdapter(templateAdapter);
		elvTemplate.setGroupIndicator(null);

		spTemplate.setOnItemSelectedListener(new OnItemSelectedListener() {
			@Override
			public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2, long arg3) {
				templateAdapter.notifyDataSetChanged();
			}

			@Override
			public void onNothingSelected(AdapterView<?> arg0) {
				templateAdapter.notifyDataSetChanged();
			}
		});

		btnRename.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				if (Util.hasProLicense(ActivityMain.this) == null) {
					// Redirect to pro page
					Util.viewUri(ActivityMain.this, cProUri);
					return;
				}

				final int templateId = spTemplate.getSelectedItemPosition();
				if (templateId == AdapterView.INVALID_POSITION)
					return;

				AlertDialog.Builder dlgRename = new AlertDialog.Builder(spTemplate.getContext());
				dlgRename.setTitle(R.string.title_rename);

				final String original = (templateId == 0 ? getString(R.string.title_default)
						: getString(R.string.title_alternate) + " " + templateId);
				dlgRename.setMessage(original);

				final EditText input = new EditText(spTemplate.getContext());
				dlgRename.setView(input);

				dlgRename.setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener() {
					public void onClick(DialogInterface dialog, int whichButton) {
						String name = input.getText().toString();
						if (TextUtils.isEmpty(name)) {
							PrivacyManager.setSetting(userId, Meta.cTypeTemplateName, Integer.toString(templateId),
									null);
							name = original;
						} else {
							PrivacyManager.setSetting(userId, Meta.cTypeTemplateName, Integer.toString(templateId),
									name);
						}
						spAdapter.remove(spAdapter.getItem(templateId));
						spAdapter.insert(name, templateId);
						spAdapter.notifyDataSetChanged();
					}
				});

				dlgRename.setNegativeButton(android.R.string.cancel, new DialogInterface.OnClickListener() {
					public void onClick(DialogInterface dialog, int whichButton) {
						// Do nothing
					}
				});

				dlgRename.create().show();
			}
		});

		// Build dialog
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
		alertDialogBuilder.setTitle(R.string.menu_template);
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
		alertDialogBuilder.setView(view);
		alertDialogBuilder.setPositiveButton(getString(R.string.msg_done), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				// Do nothing
			}
		});

		// Show dialog
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private void optionSelectAll() {
		// Select all visible apps
		if (mAppAdapter != null)
			mAppAdapter.selectAllVisible();
	}

	private void optionToggle() {
		if (mAppAdapter != null) {
			Intent intent = new Intent(ActivityShare.ACTION_TOGGLE);
			intent.putExtra(ActivityShare.cInteractive, true);
			intent.putExtra(ActivityShare.cUidList,
					mAppAdapter == null ? new int[0] : mAppAdapter.getSelectedOrVisibleUid(0));
			intent.putExtra(ActivityShare.cRestriction, mAppAdapter.getRestrictionName());
			startActivity(intent);
		}
	}

	private void optionExport() {
		Intent intent = new Intent(ActivityShare.ACTION_EXPORT);
		intent.putExtra(ActivityShare.cInteractive, true);
		intent.putExtra(ActivityShare.cUidList,
				mAppAdapter == null ? new int[0] : mAppAdapter.getSelectedOrVisibleUid(AppListAdapter.cSelectAppAll));
		startActivity(intent);
	}

	private void optionImport() {
		Intent intent = new Intent(ActivityShare.ACTION_IMPORT);
		intent.putExtra(ActivityShare.cInteractive, true);
		intent.putExtra(ActivityShare.cUidList,
				mAppAdapter == null ? new int[0] : mAppAdapter.getSelectedOrVisibleUid(0));
		startActivity(intent);
	}

	private void optionSubmit() {
		if (ActivityShare.registerDevice(this)) {
			int[] uid = (mAppAdapter == null ? new int[0] : mAppAdapter
					.getSelectedOrVisibleUid(AppListAdapter.cSelectAppNone));
			if (uid.length == 0) {
				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
				alertDialogBuilder.setTitle(R.string.app_name);
				alertDialogBuilder.setMessage(R.string.msg_select);
				alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
				alertDialogBuilder.setPositiveButton(getString(android.R.string.ok),
						new DialogInterface.OnClickListener() {
							@Override
							public void onClick(DialogInterface dialog, int which) {
							}
						});
				AlertDialog alertDialog = alertDialogBuilder.create();
				alertDialog.show();
			} else if (uid.length <= ActivityShare.cSubmitLimit) {
				if (mAppAdapter != null) {
					Intent intent = new Intent(ActivityShare.ACTION_SUBMIT);
					intent.putExtra(ActivityShare.cInteractive, true);
					intent.putExtra(ActivityShare.cUidList, uid);
					startActivity(intent);
				}
			} else {
				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
				alertDialogBuilder.setTitle(R.string.app_name);
				alertDialogBuilder.setMessage(getString(R.string.msg_limit, ActivityShare.cSubmitLimit + 1));
				alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
				alertDialogBuilder.setPositiveButton(getString(android.R.string.ok),
						new DialogInterface.OnClickListener() {
							@Override
							public void onClick(DialogInterface dialog, int which) {
							}
						});
				AlertDialog alertDialog = alertDialogBuilder.create();
				alertDialog.show();
			}
		}
	}

	private void optionFetch() {
		if (Util.hasProLicense(this) == null) {
			// Redirect to pro page
			Util.viewUri(this, cProUri);
		} else {
			if (mAppAdapter != null) {

				Intent intent = new Intent(ActivityShare.ACTION_FETCH);
				intent.putExtra(ActivityShare.cInteractive, true);
				intent.putExtra(ActivityShare.cUidList,
						mAppAdapter == null ? new int[0] : mAppAdapter.getSelectedOrVisibleUid(0));
				startActivity(intent);
			}
		}
	}

	private void optionPro() {
		// Redirect to pro page
		Util.viewUri(this, cProUri);
	}

	private void optionSwitchTheme() {
		int userId = Util.getUserId(Process.myUid());
		String themeName = PrivacyManager.getSetting(userId, PrivacyManager.cSettingTheme, "");
		themeName = (themeName.equals("Dark") ? "Light" : "Dark");
		PrivacyManager.setSetting(userId, PrivacyManager.cSettingTheme, themeName);
		this.recreate();
	}

	private void optionSettings() {
		Intent intent = new Intent(this, ActivitySettings.class);
		startActivity(intent);
	}

	private void optionDump() {
		try {
			PrivacyService.getClient().dump(0);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private void optionLegend() {
		// Show help
		Dialog dialog = new Dialog(ActivityMain.this);
		dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dialog.setTitle(R.string.menu_legend);
		dialog.setContentView(R.layout.legend);
		dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, getThemed(R.attr.icon_launcher));

		((ImageView) dialog.findViewById(R.id.imgHelpHalf)).setImageBitmap(getHalfCheckBox());
		((ImageView) dialog.findViewById(R.id.imgHelpOnDemand)).setImageBitmap(getOnDemandCheckBox());

		for (View child : Util.getViewsByTag((ViewGroup) dialog.findViewById(android.R.id.content), "details"))
			child.setVisibility(View.GONE);

		((LinearLayout) dialog.findViewById(R.id.llUnsafe)).setVisibility(PrivacyManager.cVersion3 ? View.VISIBLE
				: View.GONE);

		dialog.setCancelable(true);
		dialog.show();
	}

	private void optionTutorial() {
		((ScrollView) findViewById(R.id.svTutorialHeader)).setVisibility(View.VISIBLE);
		((ScrollView) findViewById(R.id.svTutorialDetails)).setVisibility(View.VISIBLE);
		int userId = Util.getUserId(Process.myUid());
		PrivacyManager.setSetting(userId, PrivacyManager.cSettingTutorialMain, Boolean.FALSE.toString());

		Dialog dlgUsage = new Dialog(this);
		dlgUsage.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgUsage.setTitle(R.string.title_usage_header);
		dlgUsage.setContentView(R.layout.usage);
		dlgUsage.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, getThemed(R.attr.icon_launcher));
		dlgUsage.setCancelable(true);
		dlgUsage.show();
	}

	private void optionChangelog() {
		WebView webview = new WebView(this);
		webview.setWebViewClient(new WebViewClient() {
			public void onPageFinished(WebView view, String url) {
				int userId = Util.getUserId(Process.myUid());
				Version currentVersion = new Version(Util.getSelfVersionName(ActivityMain.this));
				PrivacyManager.setSetting(userId, PrivacyManager.cSettingChangelog, currentVersion.toString());
			}
		});
		webview.loadUrl("https://github.com/M66B/XPrivacy/blob/master/CHANGELOG.md");

		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
		alertDialogBuilder.setTitle(R.string.menu_changelog);
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
		alertDialogBuilder.setView(webview);
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private void optionUpdate() {
		if (Util.hasProLicense(this) == null)
			Util.viewUri(this, ActivityMain.cProUri);
		else
			new ActivityShare.UpdateTask(this).executeOnExecutor(mExecutor);
	}

	private void optionReportIssue() {
		// Report issue
		Util.viewUri(this, Uri.parse("https://github.com/M66B/XPrivacy#support"));
	}

	@SuppressLint("DefaultLocale")
	private void optionAbout() {
		// About
		Dialog dlgAbout = new Dialog(this);
		dlgAbout.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgAbout.setTitle(R.string.menu_about);
		dlgAbout.setContentView(R.layout.about);
		dlgAbout.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, getThemed(R.attr.icon_launcher));

		// Show version
		try {
			int userId = Util.getUserId(Process.myUid());
			Version currentVersion = new Version(Util.getSelfVersionName(this));
			Version storedVersion = new Version(
					PrivacyManager.getSetting(userId, PrivacyManager.cSettingVersion, "0.0"));
			boolean migrated = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingMigrated, false);
			String versionName = currentVersion.toString();
			if (currentVersion.compareTo(storedVersion) != 0)
				versionName += "/" + storedVersion.toString();
			if (!migrated)
				versionName += "!";
			int versionCode = Util.getSelfVersionCode(this);
			TextView tvVersion = (TextView) dlgAbout.findViewById(R.id.tvVersion);
			tvVersion.setText(String.format(getString(R.string.app_version), versionName, versionCode));
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		if (!PrivacyManager.cVersion3 || Hook.isAOSP(19))
			((TextView) dlgAbout.findViewById(R.id.tvCompatibility)).setVisibility(View.GONE);

		// Show license
		String licensed = Util.hasProLicense(this);
		TextView tvLicensed1 = (TextView) dlgAbout.findViewById(R.id.tvLicensed);
		TextView tvLicensed2 = (TextView) dlgAbout.findViewById(R.id.tvLicensedAlt);
		if (licensed == null) {
			tvLicensed1.setText(Environment.getExternalStorageDirectory().getAbsolutePath());
			tvLicensed2.setText(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS)
					.getAbsolutePath());
		} else {
			tvLicensed1.setText(String.format(getString(R.string.app_licensed), licensed));
			tvLicensed2.setVisibility(View.GONE);
		}

		// Show some build properties
		String android = String.format("%s (%d)", Build.VERSION.RELEASE, Build.VERSION.SDK_INT);
		((TextView) dlgAbout.findViewById(R.id.tvAndroid)).setText(android);
		((TextView) dlgAbout.findViewById(R.id.build_brand)).setText(Build.BRAND);
		((TextView) dlgAbout.findViewById(R.id.build_manufacturer)).setText(Build.MANUFACTURER);
		((TextView) dlgAbout.findViewById(R.id.build_model)).setText(Build.MODEL);
		((TextView) dlgAbout.findViewById(R.id.build_product)).setText(Build.PRODUCT);
		((TextView) dlgAbout.findViewById(R.id.build_device)).setText(Build.DEVICE);
		((TextView) dlgAbout.findViewById(R.id.build_host)).setText(Build.HOST);
		((TextView) dlgAbout.findViewById(R.id.build_display)).setText(Build.DISPLAY);
		((TextView) dlgAbout.findViewById(R.id.build_id)).setText(Build.ID);

		dlgAbout.setCancelable(true);

		final int userId = Util.getUserId(Process.myUid());
		if (PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFirstRun, true))
			dlgAbout.setOnDismissListener(new DialogInterface.OnDismissListener() {
				@Override
				public void onDismiss(DialogInterface dialog) {
					Dialog dlgUsage = new Dialog(ActivityMain.this);
					dlgUsage.requestWindowFeature(Window.FEATURE_LEFT_ICON);
					dlgUsage.setTitle(R.string.app_name);
					dlgUsage.setContentView(R.layout.usage);
					dlgUsage.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, getThemed(R.attr.icon_launcher));
					dlgUsage.setCancelable(true);
					dlgUsage.setOnDismissListener(new DialogInterface.OnDismissListener() {
						@Override
						public void onDismiss(DialogInterface dialog) {
							PrivacyManager.setSetting(userId, PrivacyManager.cSettingFirstRun, Boolean.FALSE.toString());
							optionLegend();
						}
					});
					dlgUsage.show();
				}
			});

		dlgAbout.show();
	}

	// Tasks

	private class AppListTask extends AsyncTask<Object, Integer, List<ApplicationInfoEx>> {
		private String mRestrictionName;
		private ProgressDialog mProgressDialog;

		@Override
		protected List<ApplicationInfoEx> doInBackground(Object... params) {
			mRestrictionName = null;

			// Delegate
			return ApplicationInfoEx.getXApplicationList(ActivityMain.this, mProgressDialog);
		}

		@SuppressWarnings("deprecation")
		@Override
		protected void onPreExecute() {
			super.onPreExecute();

			TypedArray ta = getTheme().obtainStyledAttributes(new int[] { R.attr.progress_horizontal });
			int progress_horizontal = ta.getResourceId(0, 0);
			ta.recycle();

			// Show progress dialog
			mProgressDialog = new ProgressDialog(ActivityMain.this);
			mProgressDialog.setMessage(getString(R.string.msg_loading));
			mProgressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			mProgressDialog.setProgressDrawable(getResources().getDrawable(progress_horizontal));
			mProgressDialog.setProgressNumberFormat(null);
			mProgressDialog.setCancelable(false);
			mProgressDialog.setCanceledOnTouchOutside(false);
			mProgressDialog.show();
		}

		@Override
		protected void onPostExecute(List<ApplicationInfoEx> listApp) {
			if (!ActivityMain.this.isFinishing()) {
				// Display app list
				mAppAdapter = new AppListAdapter(ActivityMain.this, R.layout.mainentry, listApp, mRestrictionName);
				ListView lvApp = (ListView) findViewById(R.id.lvApp);
				lvApp.setAdapter(mAppAdapter);

				// Dismiss progress dialog
				if (mProgressDialog.isShowing())
					try {
						mProgressDialog.dismiss();
					} catch (IllegalArgumentException ignored) {
					}

				// Restore state
				ActivityMain.this.selectRestriction(spRestriction.getSelectedItemPosition());
			}

			super.onPostExecute(listApp);
		}
	}

	// Adapters

	private class SpinnerAdapter extends ArrayAdapter<String> {
		public SpinnerAdapter(Context context, int textViewResourceId) {
			super(context, textViewResourceId);
		}
	}

	@SuppressLint("DefaultLocale")
	private class TemplateListAdapter extends BaseExpandableListAdapter {
		private View mView;
		private Spinner mSpinner;
		private List<String> listRestrictionName;
		private List<String> listLocalizedTitle;
		private boolean ondemand;
		private Version version;
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

		public TemplateListAdapter(Context context, View view, int resource) {
			mView = view;
			mSpinner = (Spinner) view.findViewById(R.id.spTemplate);

			// Get restriction categories
			TreeMap<String, String> tmRestriction = PrivacyManager.getRestrictions(context);
			listRestrictionName = new ArrayList<String>(tmRestriction.values());
			listLocalizedTitle = new ArrayList<String>(tmRestriction.navigableKeySet());

			int userId = Util.getUserId(Process.myUid());
			ondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);
			version = new Version(Util.getSelfVersionName(context));
		}

		private String getTemplate() {
			if (mSpinner.getSelectedItemPosition() == 0)
				return Meta.cTypeTemplate;
			else
				return Meta.cTypeTemplate + mSpinner.getSelectedItemPosition();
		}

		private class ViewHolder {
			private View row;
			public ImageView imgIndicator;
			public ImageView imgInfo;
			public TextView tvRestriction;
			public ImageView imgUnsafe;
			public ImageView imgCbRestrict;
			public ImageView imgCbAsk;
			public boolean restricted;
			public boolean asked;

			public ViewHolder(View theRow) {
				row = theRow;
				imgIndicator = (ImageView) row.findViewById(R.id.imgIndicator);
				imgInfo = (ImageView) row.findViewById(R.id.imgInfo);
				tvRestriction = (TextView) row.findViewById(R.id.tvRestriction);
				imgUnsafe = (ImageView) row.findViewById(R.id.imgUnsafe);
				imgCbRestrict = (ImageView) row.findViewById(R.id.imgCbRestrict);
				imgCbAsk = (ImageView) row.findViewById(R.id.imgCbAsk);
			}
		}

		@Override
		public Object getGroup(int groupPosition) {
			return listRestrictionName.get(groupPosition);
		}

		@Override
		public int getGroupCount() {
			return listRestrictionName.size();
		}

		@Override
		public long getGroupId(int groupPosition) {
			return groupPosition;
		}

		@Override
		@SuppressLint("InflateParams")
		public View getGroupView(int groupPosition, boolean isExpanded, View convertView, ViewGroup parent) {
			final ViewHolder holder;
			if (convertView == null) {
				convertView = mInflater.inflate(R.layout.templateentry, null);
				holder = new ViewHolder(convertView);
				convertView.setTag(holder);
			} else
				holder = (ViewHolder) convertView.getTag();

			// Get entry
			final String restrictionName = (String) getGroup(groupPosition);

			// Get info
			final int userId = Util.getUserId(Process.myUid());
			String value = PrivacyManager.getSetting(userId, getTemplate(), restrictionName,
					Boolean.toString(!ondemand) + "+ask");
			holder.restricted = value.contains("true");
			holder.asked = (!ondemand || value.contains("asked"));

			boolean partialRestricted = false;
			boolean partialAsked = false;
			if (holder.restricted || !holder.asked)
				for (Hook hook : PrivacyManager.getHooks(restrictionName, version)) {
					String settingName = restrictionName + "." + hook.getName();
					String childValue = PrivacyManager.getSetting(userId, getTemplate(), settingName, null);
					if (childValue == null)
						childValue = Boolean.toString(holder.restricted && !hook.isDangerous())
								+ (holder.asked || (hook.isDangerous() && hook.whitelist() == null) ? "+asked" : "+ask");
					if (!childValue.contains("true"))
						partialRestricted = true;
					if (childValue.contains("asked"))
						partialAsked = true;
				}

			Bitmap bmRestricted = (holder.restricted ? partialRestricted ? getHalfCheckBox() : getFullCheckBox()
					: getOffCheckBox());
			Bitmap bmAsked = (holder.asked ? getOffCheckBox() : partialAsked ? getHalfCheckBox()
					: getOnDemandCheckBox());

			// Indicator state
			holder.imgIndicator.setImageResource(getThemed(isExpanded ? R.attr.icon_expander_maximized
					: R.attr.icon_expander_minimized));
			holder.imgIndicator.setVisibility(View.VISIBLE);
			holder.imgInfo.setVisibility(View.GONE);
			holder.imgUnsafe.setVisibility(View.GONE);

			// Set data
			holder.tvRestriction.setTypeface(null, Typeface.BOLD);
			holder.tvRestriction.setText(listLocalizedTitle.get(groupPosition));
			holder.imgCbRestrict.setImageBitmap(bmRestricted);
			holder.imgCbAsk.setImageBitmap(bmAsked);
			holder.imgCbAsk.setVisibility(ondemand ? View.VISIBLE : View.GONE);

			holder.imgCbRestrict.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View arg0) {
					// Update setting
					holder.restricted = !holder.restricted;
					PrivacyManager.setSetting(userId, getTemplate(), restrictionName, (holder.restricted ? "true"
							: "false") + "+" + (holder.asked ? "asked" : "ask"));
					notifyDataSetChanged(); // update childs
				}
			});

			holder.imgCbAsk.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View arg0) {
					// Update setting
					holder.asked = (!ondemand || !holder.asked);
					PrivacyManager.setSetting(userId, getTemplate(), restrictionName, (holder.restricted ? "true"
							: "false") + "+" + (holder.asked ? "asked" : "ask"));
					notifyDataSetChanged(); // update childs
				}
			});

			return convertView;
		}

		@Override
		public Object getChild(int groupPosition, int childPosition) {
			return PrivacyManager.getHooks((String) getGroup(groupPosition), version).get(childPosition);
		}

		@Override
		public long getChildId(int groupPosition, int childPosition) {
			return childPosition;
		}

		@Override
		public int getChildrenCount(int groupPosition) {
			return PrivacyManager.getHooks((String) getGroup(groupPosition), version).size();
		}

		@Override
		public boolean isChildSelectable(int groupPosition, int childPosition) {
			return false;
		}

		@Override
		@SuppressLint("InflateParams")
		public View getChildView(int groupPosition, int childPosition, boolean isLastChild, View convertView,
				ViewGroup parent) {
			final ViewHolder holder;
			if (convertView == null) {
				convertView = mInflater.inflate(R.layout.templateentry, null);
				holder = new ViewHolder(convertView);
				convertView.setTag(holder);
			} else
				holder = (ViewHolder) convertView.getTag();

			// Get entry
			final int userId = Util.getUserId(Process.myUid());
			final String restrictionName = (String) getGroup(groupPosition);
			final Hook hook = (Hook) getChild(groupPosition, childPosition);
			final String settingName = restrictionName + "." + hook.getName();

			// Get parent info
			String parentValue = PrivacyManager.getSetting(userId, getTemplate(), restrictionName,
					Boolean.toString(!ondemand) + "+ask");
			boolean parentRestricted = parentValue.contains("true");
			boolean parentAsked = (!ondemand || parentValue.contains("asked"));

			// Get child info
			String value = PrivacyManager.getSetting(userId, getTemplate(), settingName, null);
			// This is to circumvent caching problems
			// The child value depends on the parent value
			if (value == null)
				value = Boolean.toString(parentRestricted && !hook.isDangerous())
						+ (parentAsked || (hook.isDangerous() && hook.whitelist() == null) ? "+asked" : "+ask");
			holder.restricted = value.contains("true");
			holder.asked = (!ondemand || value.contains("asked"));
			Bitmap bmRestricted = (parentRestricted && holder.restricted ? getFullCheckBox() : getOffCheckBox());
			Bitmap bmAsked = (parentAsked || holder.asked ? getOffCheckBox() : getOnDemandCheckBox());

			// Set indicator
			holder.imgIndicator.setVisibility(View.INVISIBLE);

			// Function help
			if (hook.getAnnotation() == null)
				holder.imgInfo.setVisibility(View.GONE);
			else {
				holder.imgInfo.setVisibility(View.VISIBLE);
				holder.imgInfo.setOnClickListener(new View.OnClickListener() {
					@Override
					public void onClick(View view) {
						ActivityApp.showHelp(ActivityMain.this, mView, hook);
					}
				});
			}
			holder.imgUnsafe.setVisibility(hook.isUnsafe() ? View.VISIBLE : View.GONE);

			// Set data
			if (hook.isDangerous())
				holder.row.setBackgroundColor(getResources().getColor(
						getThemed(hook.isDangerousDefined() ? R.attr.color_dangerous : R.attr.color_dangerous_user)));
			else
				holder.row.setBackgroundColor(hook.isDangerousDefined() ? getResources().getColor(
						getThemed(R.attr.color_dangerous_off)) : Color.TRANSPARENT);
			holder.tvRestriction.setText(hook.getName());
			holder.imgCbRestrict.setEnabled(parentRestricted);
			holder.imgCbRestrict.setImageBitmap(bmRestricted);
			holder.imgCbAsk.setEnabled(!parentAsked);
			holder.imgCbAsk.setImageBitmap(bmAsked);
			holder.imgCbAsk.setVisibility(ondemand ? View.VISIBLE : View.GONE);

			// Listen for long press
			if (Util.getUserId(Process.myUid()) == 0)
				holder.tvRestriction.setOnLongClickListener(new View.OnLongClickListener() {
					@Override
					public boolean onLongClick(View view) {
						hook.toggleDangerous();

						// Change background color
						if (hook.isDangerous())
							holder.row.setBackgroundColor(getResources().getColor(
									getThemed(hook.isDangerousDefined() ? R.attr.color_dangerous
											: R.attr.color_dangerous_user)));
						else
							holder.row.setBackgroundColor(hook.isDangerousDefined() ? getResources().getColor(
									getThemed(R.attr.color_dangerous_off)) : Color.TRANSPARENT);

						notifyDataSetChanged();

						return true;
					}
				});

			holder.imgCbRestrict.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View view) {
					// Update setting
					holder.restricted = !holder.restricted;
					PrivacyManager.setSetting(userId, getTemplate(), settingName,
							(holder.restricted ? "true" : "false") + "+" + (holder.asked ? "asked" : "ask"));
					notifyDataSetChanged(); // update parent
				}
			});

			holder.imgCbAsk.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View view) {
					// Update setting
					holder.asked = !holder.asked;
					PrivacyManager.setSetting(userId, getTemplate(), settingName,
							(holder.restricted ? "true" : "false") + "+" + (holder.asked ? "asked" : "ask"));
					notifyDataSetChanged(); // update parent
				}
			});

			return convertView;
		}

		@Override
		public boolean hasStableIds() {
			return true;
		}
	}

	@SuppressLint("DefaultLocale")
	private class AppListAdapter extends ArrayAdapter<ApplicationInfoEx> {
		private Context mContext;
		private boolean mSelecting = false;
		private List<ApplicationInfoEx> mListAppAll;
		private List<ApplicationInfoEx> mListAppSelected = new ArrayList<ApplicationInfoEx>();
		private String mRestrictionName;
		private Version mVersion;
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		private AtomicInteger mFiltersRunning = new AtomicInteger(0);
		private int mHighlightColor;

		public final static int cSelectAppAll = 1;
		public final static int cSelectAppNone = 2;

		public AppListAdapter(Context context, int resource, List<ApplicationInfoEx> objects,
				String initialRestrictionName) {
			super(context, resource, objects);
			mContext = context;
			mListAppAll = new ArrayList<ApplicationInfoEx>();
			mListAppAll.addAll(objects);
			mRestrictionName = initialRestrictionName;
			mVersion = new Version(Util.getSelfVersionName(context));

			TypedArray ta1 = context.getTheme().obtainStyledAttributes(
					new int[] { android.R.attr.colorPressedHighlight });
			mHighlightColor = ta1.getColor(0, 0xFF00FF);
			ta1.recycle();
		}

		public void setRestrictionName(String restrictionName) {
			mRestrictionName = restrictionName;
		}

		public String getRestrictionName() {
			return mRestrictionName;
		}

		public List<ApplicationInfoEx> getSelectedOrVisible(int flags) {
			if (mListAppSelected.size() > 0)
				return mListAppSelected;
			else {
				if (flags == cSelectAppAll)
					return mListAppAll;
				else {
					List<ApplicationInfoEx> listApp = new ArrayList<ApplicationInfoEx>();
					if (flags != cSelectAppNone)
						for (int i = 0; i < this.getCount(); i++)
							listApp.add(this.getItem(i));
					return listApp;
				}
			}
		}

		public int[] getSelectedOrVisibleUid(int flags) {
			List<ApplicationInfoEx> listAppInfo = getSelectedOrVisible(flags);
			int[] uid = new int[listAppInfo.size()];
			for (int pos = 0; pos < listAppInfo.size(); pos++)
				uid[pos] = listAppInfo.get(pos).getUid();
			return uid;
		}

		public void selectAllVisible() {
			// Look through the visible apps to figure out what to do
			mSelecting = false;
			for (int i = 0; i < this.getCount(); i++) {
				if (!mListAppSelected.contains(this.getItem(i))) {
					mSelecting = true;
					break;
				}
			}

			if (mSelecting) {
				// Add the visible apps not already selected
				for (int i = 0; i < this.getCount(); i++)
					if (!mListAppSelected.contains(this.getItem(i)))
						mListAppSelected.add(this.getItem(i));
			} else
				mListAppSelected.clear();

			this.showStats();
			this.notifyDataSetChanged();
		}

		public void showStats() {
			TextView tvStats = (TextView) findViewById(R.id.tvStats);
			String stats = String.format("%d/%d", this.getCount(), mListAppAll.size());
			if (mListAppSelected.size() > 0)
				stats += String.format(" (%d)", mListAppSelected.size());
			tvStats.setText(stats);
		}

		@Override
		public Filter getFilter() {
			return new AppFilter();
		}

		private class AppFilter extends Filter {
			public AppFilter() {
			}

			@Override
			protected FilterResults performFiltering(CharSequence constraint) {
				int userId = Util.getUserId(Process.myUid());

				int filtersRunning = mFiltersRunning.addAndGet(1);
				FilterResults results = new FilterResults();

				// Get arguments
				String[] components = ((String) constraint).split("\\n");
				String fName = components[0];
				boolean fUsed = Boolean.parseBoolean(components[1]);
				boolean fInternet = Boolean.parseBoolean(components[2]);
				boolean fRestricted = Boolean.parseBoolean(components[3]);
				boolean fRestrictedNot = Boolean.parseBoolean(components[4]);
				boolean fPermission = Boolean.parseBoolean(components[5]);
				boolean fOnDemand = Boolean.parseBoolean(components[6]);
				boolean fOnDemandNot = Boolean.parseBoolean(components[7]);
				boolean fUser = Boolean.parseBoolean(components[8]);
				boolean fSystem = Boolean.parseBoolean(components[9]);

				// Match applications
				int current = 0;
				int max = AppListAdapter.this.mListAppAll.size();
				List<ApplicationInfoEx> lstApp = new ArrayList<ApplicationInfoEx>();
				for (ApplicationInfoEx xAppInfo : AppListAdapter.this.mListAppAll) {
					// Check if another filter has been started
					if (filtersRunning != mFiltersRunning.get())
						return null;

					// Send progress info to main activity
					current++;
					if (current % 5 == 0) {
						final int position = current;
						final int maximum = max;
						runOnUiThread(new Runnable() {
							@Override
							public void run() {
								setProgress(getString(R.string.msg_applying), position, maximum);
							}
						});
					}

					// Get if name contains
					boolean contains = false;
					if (!fName.equals(""))
						contains = (xAppInfo.toString().toLowerCase().contains(((String) fName).toLowerCase()));

					// Get if used
					boolean used = false;
					if (fUsed)
						used = (PrivacyManager.getUsage(xAppInfo.getUid(), mRestrictionName, null) != 0);

					// Get if internet
					boolean internet = false;
					if (fInternet)
						internet = xAppInfo.hasInternet(mContext);

					// Get some restricted
					boolean someRestricted = false;
					if (fRestricted)
						for (PRestriction restriction : PrivacyManager.getRestrictionList(xAppInfo.getUid(),
								mRestrictionName))
							if (restriction.restricted) {
								someRestricted = true;
								break;
							}

					// Get Android permission
					boolean permission = false;
					if (fPermission)
						if (mRestrictionName == null)
							permission = true;
						else if (PrivacyManager.hasPermission(mContext, xAppInfo, mRestrictionName, mVersion)
								|| PrivacyManager.getUsage(xAppInfo.getUid(), mRestrictionName, null) > 0)
							permission = true;

					// Get if onDemand
					boolean onDemand = false;
					boolean isApp = PrivacyManager.isApplication(xAppInfo.getUid());
					boolean odSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemandSystem,
							false);
					boolean gondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);
					if (fOnDemand && (isApp || odSystem) && gondemand) {
						onDemand = PrivacyManager.getSettingBool(-xAppInfo.getUid(), PrivacyManager.cSettingOnDemand,
								false);
						if (onDemand && mRestrictionName != null)
							onDemand = !PrivacyManager.getRestrictionEx(xAppInfo.getUid(), mRestrictionName, null).asked;
					}

					// Get if user
					boolean user = false;
					if (fUser)
						user = !xAppInfo.isSystem();

					// Get if system
					boolean system = false;
					if (fSystem)
						system = xAppInfo.isSystem();

					// Apply filters
					if ((fName.equals("") ? true : contains) && (fUsed ? used : true) && (fInternet ? internet : true)
							&& (fRestricted ? (fRestrictedNot ? !someRestricted : someRestricted) : true)
							&& (fPermission ? permission : true)
							&& (fOnDemand ? (fOnDemandNot ? !onDemand : onDemand) : true) && (fUser ? user : true)
							&& (fSystem ? system : true))
						lstApp.add(xAppInfo);
				}

				// Check again whether another filter has been started
				if (filtersRunning != mFiltersRunning.get())
					return null;

				// Apply current sorting
				Collections.sort(lstApp, mSorter);

				// Last check whether another filter has been started
				if (filtersRunning != mFiltersRunning.get())
					return null;

				synchronized (this) {
					results.values = lstApp;
					results.count = lstApp.size();
				}

				return results;
			}

			@Override
			@SuppressWarnings("unchecked")
			protected void publishResults(CharSequence constraint, FilterResults results) {
				if (results != null) {
					clear();
					TextView tvStats = (TextView) findViewById(R.id.tvStats);
					TextView tvState = (TextView) findViewById(R.id.tvState);
					ProgressBar pbFilter = (ProgressBar) findViewById(R.id.pbFilter);
					pbFilter.setVisibility(ProgressBar.GONE);
					tvStats.setVisibility(TextView.VISIBLE);

					runOnUiThread(new Runnable() {
						@Override
						public void run() {
							setProgress(getString(R.string.title_restrict), 0, 1);
						}
					});

					// Adjust progress state width
					RelativeLayout.LayoutParams tvStateLayout = (RelativeLayout.LayoutParams) tvState.getLayoutParams();
					tvStateLayout.addRule(RelativeLayout.LEFT_OF, R.id.tvStats);

					if (results.values == null)
						notifyDataSetInvalidated();
					else {
						addAll((ArrayList<ApplicationInfoEx>) results.values);
						notifyDataSetChanged();
					}
					AppListAdapter.this.showStats();
				}
			}
		}

		public void sort() {
			sort(mSorter);
		}

		private class ViewHolder {
			private View row;
			private int position;
			public View vwState;
			public LinearLayout llAppType;
			public ImageView imgIcon;
			public ImageView imgUsed;
			public ImageView imgGranted;
			public ImageView imgInternet;
			public ImageView imgFrozen;
			public ImageView imgSettings;
			public LinearLayout llName;
			public TextView tvName;
			public ImageView imgCbRestricted;
			public ProgressBar pbRunning;
			public ImageView imgCbAsk;

			public ViewHolder(View theRow, int thePosition) {
				row = theRow;
				position = thePosition;
				vwState = (View) row.findViewById(R.id.vwState);
				llAppType = (LinearLayout) row.findViewById(R.id.llAppType);
				imgIcon = (ImageView) row.findViewById(R.id.imgIcon);
				imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
				imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
				imgInternet = (ImageView) row.findViewById(R.id.imgInternet);
				imgFrozen = (ImageView) row.findViewById(R.id.imgFrozen);
				imgSettings = (ImageView) row.findViewById(R.id.imgSettings);
				llName = (LinearLayout) row.findViewById(R.id.llName);
				tvName = (TextView) row.findViewById(R.id.tvName);
				imgCbRestricted = (ImageView) row.findViewById(R.id.imgCbRestricted);
				pbRunning = (ProgressBar) row.findViewById(R.id.pbRunning);
				imgCbAsk = (ImageView) row.findViewById(R.id.imgCbAsk);
			}
		}

		private class HolderTask extends AsyncTask<Object, Object, Object> {
			private int position;
			private ViewHolder holder;
			private ApplicationInfoEx xAppInfo = null;
			private int state;
			private Bitmap bicon;
			private Drawable dicon;
			private boolean used;
			private boolean enabled;
			private boolean granted;
			private boolean settings;
			private RState rstate;
			private boolean gondemand;
			private boolean ondemand;
			private boolean can;
			private boolean methodExpert;

			public HolderTask(int thePosition, ViewHolder theHolder, ApplicationInfoEx theAppInfo) {
				position = thePosition;
				holder = theHolder;
				xAppInfo = theAppInfo;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (xAppInfo != null) {
					int userId = Util.getUserId(Process.myUid());

					// Get state
					state = xAppInfo.getState(ActivityMain.this);

					// Get icon
					bicon = xAppInfo.getIconBitmap(ActivityMain.this);
					if (bicon == null)
						dicon = xAppInfo.getIcon(ActivityMain.this);

					// Get if used
					used = (PrivacyManager.getUsage(xAppInfo.getUid(), mRestrictionName, null) != 0);

					// Get if enabled
					enabled = PrivacyManager.getSettingBool(xAppInfo.getUid(), PrivacyManager.cSettingRestricted, true);

					// Get if on demand
					gondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);
					boolean isApp = PrivacyManager.isApplication(xAppInfo.getUid());
					boolean odSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemandSystem,
							false);
					ondemand = (isApp || odSystem);
					if (ondemand && mRestrictionName != null)
						ondemand = PrivacyManager.getSettingBool(-xAppInfo.getUid(), PrivacyManager.cSettingOnDemand,
								false);

					// Get if granted
					granted = true;
					if (mRestrictionName != null)
						if (!PrivacyManager.hasPermission(ActivityMain.this, xAppInfo, mRestrictionName, mVersion))
							granted = false;

					// Get if application settings
					settings = PrivacyManager.hasSpecificSettings(xAppInfo.getUid());

					// Get restriction/ask state
					rstate = new RState(xAppInfo.getUid(), mRestrictionName, null, mVersion);

					// Get can restrict
					can = PrivacyManager.canRestrict(rstate.mUid, Process.myUid(), rstate.mRestrictionName,
							rstate.mMethodName, true);
					methodExpert = (mRestrictionName == null || PrivacyManager.getSettingBool(userId,
							PrivacyManager.cSettingMethodExpert, false));

					return holder;
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.position == position && result != null) {
					// Set background color
					if (xAppInfo.isSystem())
						holder.llAppType.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
					else
						holder.llAppType.setBackgroundColor(Color.TRANSPARENT);

					// Display state
					if (state == ApplicationInfoEx.STATE_ATTENTION)
						holder.vwState.setBackgroundColor(getResources().getColor(
								getThemed(R.attr.color_state_attention)));
					else if (state == ApplicationInfoEx.STATE_SHARED)
						holder.vwState
								.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_state_shared)));
					else
						holder.vwState.setBackgroundColor(getResources().getColor(
								getThemed(R.attr.color_state_restricted)));

					// Display icon
					if (bicon == null)
						holder.imgIcon.setImageDrawable(dicon);
					else
						holder.imgIcon.setImageBitmap(bicon);
					holder.imgIcon.setVisibility(View.VISIBLE);

					// Display on demand
					if (gondemand) {
						if (ondemand) {
							holder.imgCbAsk.setImageBitmap(getAskBoxImage(rstate, methodExpert));
							holder.imgCbAsk.setVisibility(View.VISIBLE);
						} else
							holder.imgCbAsk.setVisibility(View.INVISIBLE);
					} else
						holder.imgCbAsk.setVisibility(View.GONE);

					// Display usage
					holder.tvName.setTypeface(null, used ? Typeface.BOLD_ITALIC : Typeface.NORMAL);
					holder.imgUsed.setVisibility(used ? View.VISIBLE : View.INVISIBLE);

					// Display if permissions
					holder.imgGranted.setVisibility(granted ? View.VISIBLE : View.INVISIBLE);

					// Display if internet access
					holder.imgInternet.setVisibility(xAppInfo.hasInternet(ActivityMain.this) ? View.VISIBLE
							: View.INVISIBLE);

					// Display if frozen
					holder.imgFrozen
							.setVisibility(xAppInfo.isFrozen(ActivityMain.this) ? View.VISIBLE : View.INVISIBLE);

					// Display if settings
					holder.imgSettings.setVisibility(settings ? View.VISIBLE : View.GONE);

					// Display restriction
					holder.imgCbRestricted.setImageBitmap(getCheckBoxImage(rstate, methodExpert));
					holder.imgCbRestricted.setVisibility(View.VISIBLE);

					// Display enabled state
					holder.tvName.setEnabled(enabled && can);
					holder.imgCbRestricted.setEnabled(enabled && can);
					holder.imgCbAsk.setEnabled(enabled && can);

					// Display selection
					if (mListAppSelected.contains(xAppInfo))
						holder.row.setBackgroundColor(mHighlightColor);
					else
						holder.row.setBackgroundColor(Color.TRANSPARENT);

					// Handle details click
					holder.imgIcon.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							Intent intentSettings = new Intent(ActivityMain.this, ActivityApp.class);
							intentSettings.putExtra(ActivityApp.cUid, xAppInfo.getUid());
							intentSettings.putExtra(ActivityApp.cRestrictionName, mRestrictionName);
							ActivityMain.this.startActivity(intentSettings);
						}
					});

					// Listen for restriction changes
					holder.imgCbRestricted.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							if (mRestrictionName == null && rstate.restricted != false) {
								AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityMain.this);
								alertDialogBuilder.setTitle(R.string.menu_clear_all);
								alertDialogBuilder.setMessage(R.string.msg_sure);
								alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
								alertDialogBuilder.setPositiveButton(getString(android.R.string.ok),
										new DialogInterface.OnClickListener() {
											@Override
											public void onClick(DialogInterface dialog, int which) {
												deleteRestrictions();
											}
										});
								alertDialogBuilder.setNegativeButton(getString(android.R.string.cancel),
										new DialogInterface.OnClickListener() {
											@Override
											public void onClick(DialogInterface dialog, int which) {
											}
										});
								AlertDialog alertDialog = alertDialogBuilder.create();
								alertDialog.show();
							} else
								toggleRestrictions();
						}
					});

					// Listen for ask changes
					if (gondemand && ondemand)
						holder.imgCbAsk.setOnClickListener(new View.OnClickListener() {
							@Override
							public void onClick(View view) {
								holder.imgCbAsk.setVisibility(View.GONE);
								holder.pbRunning.setVisibility(View.VISIBLE);

								new AsyncTask<Object, Object, Object>() {
									@Override
									protected Object doInBackground(Object... arg0) {
										rstate.toggleAsked();
										rstate = new RState(xAppInfo.getUid(), mRestrictionName, null, mVersion);
										return null;
									}

									@Override
									protected void onPostExecute(Object result) {
										holder.imgCbAsk.setImageBitmap(getAskBoxImage(rstate, methodExpert));
										holder.pbRunning.setVisibility(View.GONE);
										holder.imgCbAsk.setVisibility(View.VISIBLE);
									}
								}.executeOnExecutor(mExecutor);
							}
						});
					else
						holder.imgCbAsk.setClickable(false);
				}
			}

			private void deleteRestrictions() {
				holder.imgCbRestricted.setVisibility(View.GONE);
				holder.pbRunning.setVisibility(View.VISIBLE);

				new AsyncTask<Object, Object, Object>() {
					private List<Boolean> oldState;

					@Override
					protected Object doInBackground(Object... arg0) {
						// Update restriction
						oldState = PrivacyManager.getRestartStates(xAppInfo.getUid(), mRestrictionName);
						PrivacyManager.deleteRestrictions(xAppInfo.getUid(), null, true);
						PrivacyManager.setSetting(xAppInfo.getUid(), PrivacyManager.cSettingOnDemand,
								Boolean.toString(true));
						return null;
					}

					@Override
					protected void onPostExecute(Object result) {
						// Update visible state
						holder.vwState.setBackgroundColor(getResources().getColor(
								getThemed(R.attr.color_state_attention)));

						// Update stored state
						rstate = new RState(xAppInfo.getUid(), mRestrictionName, null, mVersion);
						holder.imgCbRestricted.setImageBitmap(getCheckBoxImage(rstate, methodExpert));
						holder.imgCbAsk.setImageBitmap(getAskBoxImage(rstate, methodExpert));

						// Notify restart
						if (oldState.contains(true))
							Toast.makeText(ActivityMain.this, getString(R.string.msg_restart), Toast.LENGTH_LONG)
									.show();

						// Display new state
						showState();

						holder.pbRunning.setVisibility(View.GONE);
						holder.imgCbRestricted.setVisibility(View.VISIBLE);
					}
				}.executeOnExecutor(mExecutor);
			}

			private void toggleRestrictions() {
				holder.imgCbRestricted.setVisibility(View.GONE);
				holder.pbRunning.setVisibility(View.VISIBLE);

				new AsyncTask<Object, Object, Object>() {
					private List<Boolean> oldState;
					private List<Boolean> newState;

					@Override
					protected Object doInBackground(Object... arg0) {
						// Change restriction
						oldState = PrivacyManager.getRestartStates(xAppInfo.getUid(), mRestrictionName);
						rstate.toggleRestriction();
						newState = PrivacyManager.getRestartStates(xAppInfo.getUid(), mRestrictionName);
						return null;
					}

					@Override
					protected void onPostExecute(Object result) {
						// Update restriction display
						rstate = new RState(xAppInfo.getUid(), mRestrictionName, null, mVersion);
						holder.imgCbRestricted.setImageBitmap(getCheckBoxImage(rstate, methodExpert));
						holder.imgCbAsk.setImageBitmap(getAskBoxImage(rstate, methodExpert));

						// Notify restart
						if (!newState.equals(oldState))
							Toast.makeText(ActivityMain.this, getString(R.string.msg_restart), Toast.LENGTH_LONG)
									.show();

						// Display new state
						showState();

						holder.pbRunning.setVisibility(View.GONE);
						holder.imgCbRestricted.setVisibility(View.VISIBLE);
					}
				}.executeOnExecutor(mExecutor);
			}

			private void showState() {
				state = xAppInfo.getState(ActivityMain.this);
				if (state == ApplicationInfoEx.STATE_ATTENTION)
					holder.vwState.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_state_attention)));
				else if (state == ApplicationInfoEx.STATE_SHARED)
					holder.vwState.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_state_shared)));
				else
					holder.vwState
							.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_state_restricted)));
			}
		}

		@Override
		@SuppressLint("InflateParams")
		public View getView(int position, View convertView, ViewGroup parent) {
			final ViewHolder holder;
			if (convertView == null) {
				convertView = mInflater.inflate(R.layout.mainentry, null);
				holder = new ViewHolder(convertView, position);
				convertView.setTag(holder);
			} else {
				holder = (ViewHolder) convertView.getTag();
				holder.position = position;
			}

			// Get info
			final ApplicationInfoEx xAppInfo = getItem(holder.position);

			// Set data
			holder.row.setBackgroundColor(Color.TRANSPARENT);
			holder.vwState.setBackgroundColor(Color.TRANSPARENT);
			holder.llAppType.setBackgroundColor(Color.TRANSPARENT);
			holder.imgIcon.setVisibility(View.INVISIBLE);
			holder.tvName.setText(xAppInfo.toString());
			holder.tvName.setTypeface(null, Typeface.NORMAL);
			holder.imgUsed.setVisibility(View.INVISIBLE);
			holder.imgGranted.setVisibility(View.INVISIBLE);
			holder.imgInternet.setVisibility(View.INVISIBLE);
			holder.imgFrozen.setVisibility(View.INVISIBLE);
			holder.imgSettings.setVisibility(View.GONE);
			holder.imgCbRestricted.setVisibility(View.INVISIBLE);
			holder.imgCbAsk.setVisibility(View.INVISIBLE);
			holder.tvName.setEnabled(false);
			holder.imgCbRestricted.setEnabled(false);

			holder.imgIcon.setClickable(false);
			holder.imgCbRestricted.setClickable(false);
			holder.imgCbAsk.setClickable(false);

			// Listen for multiple select
			holder.llName.setOnLongClickListener(new View.OnLongClickListener() {
				@Override
				public boolean onLongClick(View view) {
					if (mListAppSelected.contains(xAppInfo)) {
						mSelecting = false;
						mListAppSelected.clear();
						mAppAdapter.notifyDataSetChanged();
					} else {
						mSelecting = true;
						mListAppSelected.add(xAppInfo);
						holder.row.setBackgroundColor(mHighlightColor);
					}
					showStats();
					return true;
				}
			});

			// Listen for application selection
			holder.llName.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(final View view) {
					if (mSelecting) {
						if (mListAppSelected.contains(xAppInfo)) {
							mListAppSelected.remove(xAppInfo);
							holder.row.setBackgroundColor(Color.TRANSPARENT);
							if (mListAppSelected.size() == 0)
								mSelecting = false;
						} else {
							mListAppSelected.add(xAppInfo);
							holder.row.setBackgroundColor(mHighlightColor);
						}
						showStats();
					} else {
						Intent intentSettings = new Intent(ActivityMain.this, ActivityApp.class);
						intentSettings.putExtra(ActivityApp.cUid, xAppInfo.getUid());
						intentSettings.putExtra(ActivityApp.cRestrictionName, mRestrictionName);
						ActivityMain.this.startActivity(intentSettings);
					}
				}
			});

			// Async update
			new HolderTask(position, holder, xAppInfo).executeOnExecutor(mExecutor, (Object) null);

			return convertView;
		}
	}

	// Helper methods

	private void setProgress(String text, int progress, int max) {
		// Set up the progress bar
		if (mProgressWidth == 0) {
			final View vProgressEmpty = (View) findViewById(R.id.vProgressEmpty);
			mProgressWidth = vProgressEmpty.getMeasuredWidth();
		}
		// Display stuff
		TextView tvState = (TextView) findViewById(R.id.tvState);
		if (text != null)
			tvState.setText(text);
		if (max == 0)
			max = 1;
		mProgress = (int) ((float) mProgressWidth) * progress / max;

		View vProgressFull = (View) findViewById(R.id.vProgressFull);
		vProgressFull.getLayoutParams().width = mProgress;
	}

	private int getSelectedCategory(final int userId) {
		int pos = 0;
		String restrictionName = PrivacyManager.getSetting(userId, PrivacyManager.cSettingSelectedCategory, null);
		if (restrictionName != null)
			for (String restriction : PrivacyManager.getRestrictions(this).values()) {
				pos++;
				if (restrictionName.equals(restriction))
					break;
			}
		return pos;
	}

	private void checkLicense() {
		if (!Util.isProEnabled() && Util.hasProLicense(this) == null)
			if (Util.isProEnablerInstalled(this))
				try {
					int uid = getPackageManager().getPackageInfo("biz.bokhorst.xprivacy.pro", 0).applicationInfo.uid;
					PrivacyManager.deleteRestrictions(uid, null, true);
					Util.log(null, Log.INFO, "Licensing: check");
					startActivityForResult(new Intent("biz.bokhorst.xprivacy.pro.CHECK"), ACTIVITY_LICENSE);
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}
	}
}

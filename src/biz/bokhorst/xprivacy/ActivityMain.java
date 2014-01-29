package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageInfo;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.view.Window;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.Filter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RadioGroup;
import android.widget.RelativeLayout;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;

public class ActivityMain extends Activity implements OnItemSelectedListener, CompoundButton.OnCheckedChangeListener {
	private int mThemeId;
	private Spinner spRestriction = null;
	private AppListAdapter mAppAdapter = null;
	private Bitmap[] mCheck;
	private int mTab;
	private int mProgressWidth = 0;
	private int mProgress = 0;
	private String mSharingState = null;

	private Comparator<ApplicationInfoEx> mSorter = new Comparator<ApplicationInfoEx>() {
		@Override
		public int compare(ApplicationInfoEx appInfo0, ApplicationInfoEx appInfo1) {
			int sortMode = Integer.parseInt(PrivacyManager
					.getSetting(null, 0, PrivacyManager.cSettingSMode, "0", false));
			boolean sortInvert = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingSInvert, false, false);
			int sortOrder = sortInvert ? -1 : 1;
			switch (sortMode) {
			case SORT_BY_NAME:
				return sortOrder * appInfo0.compareTo(appInfo1);
			case SORT_BY_UID:
				// default lowest first
				return sortOrder * (appInfo0.getUid() - appInfo1.getUid());
			case SORT_BY_INSTALL_TIME:
				// default newest first
				Long iTime0 = appInfo0.getInstallTime(ActivityMain.this);
				Long iTime1 = appInfo1.getInstallTime(ActivityMain.this);
				return sortOrder * iTime1.compareTo(iTime0);
			case SORT_BY_UPDATE_TIME:
				// default newest first
				Long uTime0 = appInfo0.getUpdateTime(ActivityMain.this);
				Long uTime1 = appInfo1.getUpdateTime(ActivityMain.this);
				return sortOrder * uTime1.compareTo(uTime0);
			case SORT_BY_MODIF_TIME:
				// default newest first
				Long mTime0 = appInfo0.getModificationTime(ActivityMain.this);
				Long mTime1 = appInfo1.getModificationTime(ActivityMain.this);
				return sortOrder * mTime1.compareTo(mTime0);
			}
			return 0;
		}
	};

	private Handler mProHandler = new Handler();

	public static final int STATE_ATTENTION = 0;
	public static final int STATE_RESTRICTED = 1;
	public static final int STATE_SHARED = 2;

	private static final int SORT_BY_NAME = 0;
	private static final int SORT_BY_UID = 1;
	private static final int SORT_BY_INSTALL_TIME = 2;
	private static final int SORT_BY_UPDATE_TIME = 3;
	private static final int SORT_BY_MODIF_TIME = 4;

	private static final int TAB_NONE = 0;
	private static final int TAB_CATEGORY = 1;
	private static final int TAB_FILTERS = 2;

	private static final int ACTIVITY_LICENSE = 0;
	private static final int LICENSED = 0x0100;
	private static final int NOT_LICENSED = 0x0231;
	private static final int RETRY = 0x0123;

	private static final int ERROR_CONTACTING_SERVER = 0x101;
	private static final int ERROR_INVALID_PACKAGE_NAME = 0x102;
	private static final int ERROR_NON_MATCHING_UID = 0x103;

	public static final Uri cProUri = Uri.parse("http://www.xprivacy.eu/");
	public static final String cXUrl = "https://github.com/M66B/XPrivacy?mobile=0";

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

	private BroadcastReceiver mPackageChangeReceiver = new BroadcastReceiver() {
		@Override
		public void onReceive(Context context, Intent intent) {
			ActivityMain.this.recreate();
		}
	};

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Check privacy service client
		if (!Util.isXposedEnabled() || PrivacyService.getClient() == null) {
			setContentView(R.layout.reboot);
			Requirements.check(this);
			mPackageChangeReceiver = null;
		} else {
			// Salt should be the same when exporting/importing
			String salt = PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingSalt, null, false);
			if (salt == null) {
				salt = Build.SERIAL;
				if (salt == null)
					salt = "";
				PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingSalt, salt);
			}

			// Set theme
			String themeName = PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingTheme, "", false);
			mThemeId = (themeName.equals("Dark") ? R.style.CustomTheme : R.style.CustomTheme_Light);
			setTheme(mThemeId);

			// Set layout
			setContentView(R.layout.mainlist);
			getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_HIDDEN);

			if (Util.hasProLicense(this) != null)
				setTitle(String.format("%s - %s", getString(R.string.app_name), getString(R.string.menu_pro)));

			// Get localized restriction name
			List<String> listRestrictionName = new ArrayList<String>(PrivacyManager.getRestrictions(this)
					.navigableKeySet());
			listRestrictionName.add(0, getString(R.string.menu_all));

			// Build spinner adapter
			SpinnerAdapter spAdapter = new SpinnerAdapter(this, android.R.layout.simple_spinner_item);
			spAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
			spAdapter.addAll(listRestrictionName);

			// Handle info
			ImageView imgInfo = (ImageView) findViewById(R.id.imgInfo);
			imgInfo.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					int position = spRestriction.getSelectedItemPosition();
					if (position != AdapterView.INVALID_POSITION) {
						String query = (position == 0 ? "restrictions" : (String) PrivacyManager
								.getRestrictions(ActivityMain.this).values().toArray()[position - 1]);
						Util.viewUri(ActivityMain.this, Uri.parse(cXUrl + "#" + query));
					}
				}
			});

			// Setup spinner
			spRestriction = (Spinner) findViewById(R.id.spRestriction);
			spRestriction.setAdapter(spAdapter);
			spRestriction.setOnItemSelectedListener(this);

			// Setup name filter
			final EditText etFilter = (EditText) findViewById(R.id.etFilter);
			etFilter.addTextChangedListener(new TextWatcher() {
				@Override
				public void onTextChanged(CharSequence s, int start, int before, int count) {
					String text = etFilter.getText().toString();
					ImageView imgClear = (ImageView) findViewById(R.id.imgClear);
					imgClear.setImageDrawable(getResources().getDrawable(
							Util.getThemed(ActivityMain.this, text.equals("") ? R.attr.icon_clear_grayed
									: R.attr.icon_clear)));
					applyFilter();
				}

				@Override
				public void beforeTextChanged(CharSequence s, int start, int count, int after) {
				}

				@Override
				public void afterTextChanged(Editable s) {
				}
			});

			ImageView imgClear = (ImageView) findViewById(R.id.imgClear);
			imgClear.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					etFilter.setText("");
				}
			});

			// Setup used filter
			boolean fUsed = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingFUsed, false, false);
			CheckBox cFbUsed = (CheckBox) findViewById(R.id.cbFUsed);
			cFbUsed.setChecked(fUsed);
			cFbUsed.setOnCheckedChangeListener(this);

			// Setup internet filter
			boolean fInternet = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingFInternet, false, false);
			CheckBox cbFInternet = (CheckBox) findViewById(R.id.cbFInternet);
			cbFInternet.setChecked(fInternet);
			cbFInternet.setOnCheckedChangeListener(this);

			// Setup restriction filter
			boolean fRestriction = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingFRestriction, false,
					false);
			CheckBox cbFRestriction = (CheckBox) findViewById(R.id.cbFRestriction);
			cbFRestriction.setChecked(fRestriction);
			cbFRestriction.setOnCheckedChangeListener(this);

			boolean fRestrictionNot = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingFRestrictionNot,
					false, false);
			CheckBox cbFRestrictionNot = (CheckBox) findViewById(R.id.cbFRestrictionNot);
			cbFRestrictionNot.setChecked(fRestrictionNot);
			cbFRestrictionNot.setOnCheckedChangeListener(this);

			// Setup permission filter
			boolean fPermission = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingFPermission, true,
					false);
			CheckBox cbFPermission = (CheckBox) findViewById(R.id.cbFPermission);
			cbFPermission.setChecked(fPermission);
			cbFPermission.setOnCheckedChangeListener(this);

			// Setup user filter
			boolean fUser = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingFUser, true, false);
			final CheckBox cbFUser = (CheckBox) findViewById(R.id.cbFUser);
			cbFUser.setChecked(fUser);
			cbFUser.setOnCheckedChangeListener(this);

			// Setup system filter
			boolean fSystem = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingFSystem, false, false);
			final CheckBox cbFSystem = (CheckBox) findViewById(R.id.cbFSystem);
			cbFSystem.setChecked(fSystem);
			cbFSystem.setOnCheckedChangeListener(this);

			// Tab states
			mTab = Integer.parseInt(PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingOpenTab,
					Integer.toString(TAB_NONE), false));
			openTab(mTab);

			// Handle toggle filters visibility
			TextView tvFilterDetail = (TextView) findViewById(R.id.tvFilterDetail);
			tvFilterDetail.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					int currentTab = Integer.parseInt(PrivacyManager.getSetting(null, 0,
							PrivacyManager.cSettingOpenTab, Integer.toString(TAB_NONE), false));
					if (currentTab == TAB_FILTERS)
						openTab(TAB_NONE);
					else
						openTab(TAB_FILTERS);
				}
			});

			// Handle toggle categories visibility
			TextView tvCategoryDetail = (TextView) findViewById(R.id.tvCategoryDetail);
			tvCategoryDetail.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					int currentTab = Integer.parseInt(PrivacyManager.getSetting(null, 0,
							PrivacyManager.cSettingOpenTab, Integer.toString(TAB_NONE), false));
					if (currentTab == TAB_CATEGORY)
						openTab(TAB_NONE);
					else
						openTab(TAB_CATEGORY);
				}
			});

			// Handle post share operation done message
			if (mSharingState != null) {
				TextView tvState = (TextView) findViewById(R.id.tvState);
				tvState.setText(mSharingState);
				mSharingState = null;
			}

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

			// First run
			if (PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingFirstRun, true, false)) {
				optionAbout();
				PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingFirstRun, Boolean.FALSE.toString());
			}

			// Build tri-state check box images
			mCheck = Util.getTriStateCheckBox(this);

			// Tutorial
			if (!PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingTutorialMain, false, false)) {
				((RelativeLayout) findViewById(R.id.rlTutorialHeader)).setVisibility(View.VISIBLE);
				((RelativeLayout) findViewById(R.id.rlTutorialDetails)).setVisibility(View.VISIBLE);
			}
			View.OnClickListener listener = new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					ViewParent parent = view.getParent();
					while (!parent.getClass().equals(RelativeLayout.class))
						parent = parent.getParent();
					((View) parent).setVisibility(View.GONE);
					PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingTutorialMain, Boolean.TRUE.toString());
				}
			};
			((Button) findViewById(R.id.btnTutorialHeader)).setOnClickListener(listener);
			((Button) findViewById(R.id.btnTutorialDetails)).setOnClickListener(listener);
		}
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mAppAdapter != null)
			mAppAdapter.notifyDataSetChanged();
	}

	@Override
	protected void onNewIntent(Intent intent) {
		// Do nothing
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();
		if (mPackageChangeReceiver != null)
			unregisterReceiver(mPackageChangeReceiver);
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
					Toast toast = Toast.makeText(this, getString(R.string.menu_pro), Toast.LENGTH_LONG);
					toast.show();
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

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		if (PrivacyService.getClient() == null)
			return false;
		else {
			MenuInflater inflater = getMenuInflater();
			inflater.inflate(R.menu.main, menu);
			return true;
		}
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		boolean mounted = Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState());

		menu.findItem(R.id.menu_export).setEnabled(mounted);
		menu.findItem(R.id.menu_import).setEnabled(mounted);
		menu.findItem(R.id.menu_pro).setVisible(!Util.isProEnabled() && Util.hasProLicense(this) == null);

		return super.onPrepareOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		try {
			switch (item.getItemId()) {
			case R.id.menu_help:
				optionHelp();
				return true;
			case R.id.menu_select_all:
				optionSelectAll();
				return true;
			case R.id.menu_sort:
				optionSort();
				return true;
			case R.id.menu_tutorial:
				optionTutorial();
				return true;
			case R.id.menu_all:
				optionAll();
				return true;
			case R.id.menu_usage:
				optionUsage();
				return true;
			case R.id.menu_settings:
				SettingsDialog.edit(ActivityMain.this, null);
				return true;
			case R.id.menu_template:
				optionTemplate();
				return true;
			case R.id.menu_report:
				optionReportIssue();
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
			case R.id.menu_refresh:
				optionRefresh();
				return true;
			case R.id.menu_theme:
				optionSwitchTheme();
				return true;
			case R.id.menu_pro:
				optionPro();
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
			String restrictionName = (pos == 0 ? null : (String) PrivacyManager.getRestrictions(this).values()
					.toArray()[pos - 1]);
			mAppAdapter.setRestrictionName(restrictionName);
			applyFilter();
		}
	}

	@Override
	public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
		CheckBox cbFUsed = (CheckBox) findViewById(R.id.cbFUsed);
		CheckBox cbFInternet = (CheckBox) findViewById(R.id.cbFInternet);
		CheckBox cbFRestriction = (CheckBox) findViewById(R.id.cbFRestriction);
		CheckBox cbFRestrictionNot = (CheckBox) findViewById(R.id.cbFRestrictionNot);
		CheckBox cbFPermission = (CheckBox) findViewById(R.id.cbFPermission);
		CheckBox cbFUser = (CheckBox) findViewById(R.id.cbFUser);
		CheckBox cbFSystem = (CheckBox) findViewById(R.id.cbFSystem);
		if (buttonView == cbFUsed)
			PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingFUsed, Boolean.toString(isChecked));
		else if (buttonView == cbFInternet)
			PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingFInternet, Boolean.toString(isChecked));
		else if (buttonView == cbFRestriction)
			PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingFRestriction, Boolean.toString(isChecked));
		else if (buttonView == cbFRestrictionNot)
			PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingFRestrictionNot, Boolean.toString(isChecked));
		else if (buttonView == cbFPermission)
			PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingFPermission, Boolean.toString(isChecked));
		else if (buttonView == cbFUser) {
			if (isChecked)
				cbFSystem.setChecked(false);
			PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingFUser, Boolean.toString(isChecked));
		} else if (buttonView == cbFSystem) {
			if (isChecked)
				cbFUser.setChecked(false);
			PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingFSystem, Boolean.toString(isChecked));
		}
		applyFilter();
	}

	private void applyFilter() {
		if (mAppAdapter != null) {
			EditText etFilter = (EditText) findViewById(R.id.etFilter);
			CheckBox cFbUsed = (CheckBox) findViewById(R.id.cbFUsed);
			CheckBox cbFInternet = (CheckBox) findViewById(R.id.cbFInternet);
			CheckBox cbFRestriction = (CheckBox) findViewById(R.id.cbFRestriction);
			CheckBox cbFRestrictionNot = (CheckBox) findViewById(R.id.cbFRestrictionNot);
			CheckBox cbFPermission = (CheckBox) findViewById(R.id.cbFPermission);
			CheckBox cbFUser = (CheckBox) findViewById(R.id.cbFUser);
			CheckBox cbFSystem = (CheckBox) findViewById(R.id.cbFSystem);
			ProgressBar pbFilter = (ProgressBar) findViewById(R.id.pbFilter);
			TextView tvStats = (TextView) findViewById(R.id.tvStats);
			TextView tvState = (TextView) findViewById(R.id.tvState);
			String filter = String.format("%s\n%b\n%b\n%b\n%b\n%b\n%b\n%b", etFilter.getText().toString(),
					cFbUsed.isChecked(), cbFInternet.isChecked(), cbFRestriction.isChecked(),
					cbFRestrictionNot.isChecked(), cbFPermission.isChecked(), cbFUser.isChecked(),
					cbFSystem.isChecked());
			pbFilter.setVisibility(ProgressBar.VISIBLE);
			tvStats.setVisibility(TextView.GONE);

			// Adjust progress state width
			RelativeLayout.LayoutParams tvStateLayout = (RelativeLayout.LayoutParams) tvState.getLayoutParams();
			tvStateLayout.addRule(RelativeLayout.LEFT_OF, R.id.pbFilter);

			mAppAdapter.getFilter().filter(filter);
		}
	}

	private void applySort() {
		if (mAppAdapter != null) {
			mAppAdapter.sort();
		}
	}

	// Options

	private void optionAll() {
		Intent intent = new Intent(ActivityShare.ACTION_TOGGLE);
		intent.putExtra(ActivityShare.cInteractive, true);
		intent.putExtra(ActivityShare.cUidList,
				mAppAdapter == null ? new int[0] : mAppAdapter.getSelectedOrVisibleUid(0));
		intent.putExtra(ActivityShare.cRestriction, mAppAdapter.getRestrictionName());
		startActivity(intent);
	}

	private void optionUsage() {
		Intent intent = new Intent(this, ActivityUsage.class);
		intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
		startActivity(intent);
	}

	private void optionTemplate() {
		// Get restriction categories
		TreeMap<String, String> tmRestriction = PrivacyManager.getRestrictions(this);
		List<String> listRestrictionName = new ArrayList<String>(tmRestriction.navigableKeySet());
		final List<String> listLocalizedTitle = new ArrayList<String>(tmRestriction.values());

		CharSequence[] options = new CharSequence[listLocalizedTitle.size()];
		listRestrictionName.toArray(options);
		boolean[] selection = new boolean[listLocalizedTitle.size()];
		for (int i = 0; i < listLocalizedTitle.size(); i++) {
			String templateName = PrivacyManager.cSettingTemplate + "." + listLocalizedTitle.get(i);
			selection[i] = PrivacyManager.getSettingBool(null, 0, templateName, true, false);
			PrivacyManager.setSetting(null, 0, templateName, Boolean.toString(selection[i]));
		}

		// Build dialog
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
		alertDialogBuilder.setTitle(R.string.menu_template);
		alertDialogBuilder.setIcon(Util.getThemed(this, R.attr.icon_launcher));
		alertDialogBuilder.setMultiChoiceItems(options, selection, new DialogInterface.OnMultiChoiceClickListener() {
			public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
				String templateName = PrivacyManager.cSettingTemplate + "." + listLocalizedTitle.get(whichButton);
				PrivacyManager.setSetting(null, 0, templateName, Boolean.toString(isChecked));
			}
		});
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

	private void optionReportIssue() {
		// Report issue
		Util.viewUri(this, Uri.parse("https://github.com/M66B/XPrivacy/issues"));
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
				alertDialogBuilder.setIcon(Util.getThemed(this, R.attr.icon_launcher));
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
				alertDialogBuilder.setIcon(Util.getThemed(this, R.attr.icon_launcher));
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
				intent.putExtra(
						ActivityShare.cUidList,
						mAppAdapter == null ? new int[0] : mAppAdapter
								.getSelectedOrVisibleUid(AppListAdapter.cSelectAppUser));
				startActivity(intent);
			}
		}
	}

	private void optionRefresh() {
		this.recreate();
	}

	private void optionSwitchTheme() {
		String themeName = PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingTheme, "", false);
		themeName = (themeName.equals("Dark") ? "Light" : "Dark");
		PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingTheme, themeName);
		this.recreate();
	}

	private void optionPro() {
		// Redirect to pro page
		Util.viewUri(this, cProUri);
	}

	private void optionAbout() {
		// About
		Dialog dlgAbout = new Dialog(this);
		dlgAbout.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgAbout.setTitle(R.string.menu_about);
		dlgAbout.setContentView(R.layout.about);
		dlgAbout.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, Util.getThemed(this, R.attr.icon_launcher));

		// Show version
		try {
			PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
			TextView tvVersion = (TextView) dlgAbout.findViewById(R.id.tvVersion);
			tvVersion.setText(String.format(getString(R.string.app_version), pInfo.versionName, pInfo.versionCode));
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		// Show Xposed version
		int xVersion = Util.getXposedAppProcessVersion();
		TextView tvXVersion = (TextView) dlgAbout.findViewById(R.id.tvXVersion);
		tvXVersion.setText(String.format(getString(R.string.app_xversion), xVersion));

		// Show license
		String licensed = Util.hasProLicense(this);
		TextView tvLicensed = (TextView) dlgAbout.findViewById(R.id.tvLicensed);
		if (licensed == null)
			tvLicensed.setText(String.format(getString(R.string.msg_licensed), Environment
					.getExternalStorageDirectory().getAbsolutePath()));
		else
			tvLicensed.setText(String.format(getString(R.string.msg_licensed), licensed));

		dlgAbout.setCancelable(true);
		dlgAbout.show();
	}

	private void optionHelp() {
		// Show help
		Dialog dialog = new Dialog(ActivityMain.this);
		dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dialog.setTitle(R.string.menu_help);
		dialog.setContentView(R.layout.help);
		dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, Util.getThemed(this, R.attr.icon_launcher));
		ImageView imgHelpHalf = (ImageView) dialog.findViewById(R.id.imgHelpHalf);
		imgHelpHalf.setImageBitmap(mCheck[1]);
		dialog.setCancelable(true);
		dialog.show();
	}

	private void optionSelectAll() {
		// Select all visible apps
		if (mAppAdapter != null)
			mAppAdapter.selectAllVisible();
	}

	private void optionSort() {
		LayoutInflater LayoutInflater = (LayoutInflater) ActivityMain.this
				.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		View view = LayoutInflater.inflate(R.layout.sort, null);
		final RadioGroup rgSMode = (RadioGroup) view.findViewById(R.id.rgSMode);
		final CheckBox cbSInvert = (CheckBox) view.findViewById(R.id.cbSInvert);

		// Initialise controls
		int sortMode = Integer.parseInt(PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingSMode, "0", false));
		boolean sortInvert = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingSInvert, false, false);
		switch (sortMode) {
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
		case SORT_BY_MODIF_TIME:
			rgSMode.check(R.id.rbSModified);
			break;
		}
		cbSInvert.setChecked(sortInvert);

		// Build dialog
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityMain.this);
		alertDialogBuilder.setTitle(R.string.menu_sort);
		alertDialogBuilder.setIcon(Util.getThemed(ActivityMain.this, R.attr.icon_launcher));
		alertDialogBuilder.setView(view);
		alertDialogBuilder.setPositiveButton(ActivityMain.this.getString(android.R.string.ok),
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						switch (rgSMode.getCheckedRadioButtonId()) {
						case R.id.rbSName:
							PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingSMode,
									Integer.toString(SORT_BY_NAME));
							break;
						case R.id.rbSUid:
							PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingSMode,
									Integer.toString(SORT_BY_UID));
							break;
						case R.id.rbSInstalled:
							PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingSMode,
									Integer.toString(SORT_BY_INSTALL_TIME));
							break;
						case R.id.rbSUpdated:
							PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingSMode,
									Integer.toString(SORT_BY_UPDATE_TIME));
							break;
						case R.id.rbSModified:
							PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingSMode,
									Integer.toString(SORT_BY_MODIF_TIME));
							break;
						}
						PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingSInvert,
								Boolean.toString(cbSInvert.isChecked()));
						applySort();
					}
				});

		// Show dialog
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private void optionTutorial() {
		openTab(TAB_CATEGORY);
		((RelativeLayout) findViewById(R.id.rlTutorialHeader)).setVisibility(View.VISIBLE);
		((RelativeLayout) findViewById(R.id.rlTutorialDetails)).setVisibility(View.VISIBLE);
		PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingTutorialMain, Boolean.FALSE.toString());
	}

	private void openTab(int which) {
		TextView tvFilterDetail = (TextView) findViewById(R.id.tvFilterDetail);
		View vFilterHighlight = findViewById(R.id.vFilterHighlight);
		EditText etFilter = (EditText) findViewById(R.id.etFilter);
		LinearLayout llFilters = (LinearLayout) findViewById(R.id.llFilters);
		LinearLayout llCategories = (LinearLayout) findViewById(R.id.llCategories);
		CheckBox cbFUsed = (CheckBox) findViewById(R.id.cbFUsed);
		CheckBox cbFInternet = (CheckBox) findViewById(R.id.cbFInternet);
		CheckBox cbFRestriction = (CheckBox) findViewById(R.id.cbFRestriction);
		CheckBox cbFPermission = (CheckBox) findViewById(R.id.cbFPermission);
		CheckBox cbFUser = (CheckBox) findViewById(R.id.cbFUser);
		CheckBox cbFSystem = (CheckBox) findViewById(R.id.cbFSystem);
		TextView tvCategories = (TextView) findViewById(R.id.tvCategoryDetail);
		View vCategoryHighlight = findViewById(R.id.vCategoryHighlight);

		if (which == TAB_FILTERS) {
			llFilters.setVisibility(View.VISIBLE);
			llCategories.setVisibility(View.GONE);

			tvFilterDetail.setText(R.string.title_filters);
			tvCategories.setText((String) spRestriction.getSelectedItem());

		} else if (which == TAB_CATEGORY) {
			llFilters.setVisibility(View.GONE);
			llCategories.setVisibility(View.VISIBLE);

			tvCategories.setText(R.string.title_categories);

		} else {
			llFilters.setVisibility(View.GONE);
			llCategories.setVisibility(View.GONE);

			tvCategories.setText((String) spRestriction.getSelectedItem());
		}

		// Filters title
		if (which != TAB_FILTERS) {
			// Count number of active filters
			int numberOfFilters = 0;
			if (etFilter.getText().length() > 0)
				numberOfFilters++;
			if (cbFUsed.isChecked())
				numberOfFilters++;
			if (cbFInternet.isChecked())
				numberOfFilters++;
			if (cbFRestriction.isChecked())
				numberOfFilters++;
			if (cbFPermission.isChecked())
				numberOfFilters++;
			if (cbFUser.isChecked())
				numberOfFilters++;
			if (cbFSystem.isChecked())
				numberOfFilters++;

			tvFilterDetail.setText(getResources().getQuantityString(R.plurals.title_active_filters, numberOfFilters,
					numberOfFilters));
		}

		// Set highlights
		vFilterHighlight.setBackgroundResource(which != TAB_FILTERS ? android.R.color.transparent : Util.getThemed(
				this, android.R.attr.colorActivatedHighlight));
		vCategoryHighlight.setBackgroundResource(which != TAB_CATEGORY ? android.R.color.transparent : Util.getThemed(
				this, android.R.attr.colorActivatedHighlight));

		// Save setting
		PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingOpenTab, Integer.toString(which));
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

		@Override
		protected void onPreExecute() {
			super.onPreExecute();

			// Show progress dialog
			ListView lvApp = (ListView) findViewById(R.id.lvApp);
			mProgressDialog = new ProgressDialog(lvApp.getContext());
			mProgressDialog.setMessage(getString(R.string.msg_loading));
			mProgressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			mProgressDialog.setProgressNumberFormat(null);
			mProgressDialog.setCancelable(false);
			mProgressDialog.show();
		}

		@Override
		protected void onPostExecute(List<ApplicationInfoEx> listApp) {
			super.onPostExecute(listApp);

			// Display app list
			mAppAdapter = new AppListAdapter(ActivityMain.this, R.layout.mainentry, listApp, mRestrictionName);
			ListView lvApp = (ListView) findViewById(R.id.lvApp);
			lvApp.setAdapter(mAppAdapter);

			// Dismiss progress dialog
			try {
				mProgressDialog.dismiss();
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

			// Restore state
			ActivityMain.this.selectRestriction(spRestriction.getSelectedItemPosition());
		}
	}

	// Adapters

	private class SpinnerAdapter extends ArrayAdapter<String> {
		public SpinnerAdapter(Context context, int textViewResourceId) {
			super(context, textViewResourceId);
		}
	}

	@SuppressLint("DefaultLocale")
	private class AppListAdapter extends ArrayAdapter<ApplicationInfoEx> {
		private Context mContext;
		private List<ApplicationInfoEx> mListAppAll;
		private List<ApplicationInfoEx> mListAppSelected = new ArrayList<ApplicationInfoEx>();
		private String mRestrictionName;
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		private AtomicInteger mFiltersRunning = new AtomicInteger(0);
		private int mHighlightColor;

		public final static int cSelectAppAll = 1;
		public final static int cSelectAppNone = 2;
		public final static int cSelectAppUser = 3;

		public AppListAdapter(Context context, int resource, List<ApplicationInfoEx> objects,
				String initialRestrictionName) {
			super(context, resource, objects);
			mContext = context;
			mListAppAll = new ArrayList<ApplicationInfoEx>();
			mListAppAll.addAll(objects);
			mRestrictionName = initialRestrictionName;

			TypedArray ta1 = context.getTheme().obtainStyledAttributes(
					new int[] { android.R.attr.colorLongPressedHighlight });
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
					if (flags == cSelectAppUser) {
						for (ApplicationInfoEx appInfo : mListAppAll)
							if (!appInfo.isSystem())
								listApp.add(appInfo);
					} else if (flags != cSelectAppNone) {
						for (int i = 0; i < this.getCount(); i++)
							listApp.add(this.getItem(i));
					}
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
			boolean addThem = false;
			for (int i = 0; i < this.getCount(); i++) {
				if (!mListAppSelected.contains(this.getItem(i))) {
					addThem = true;
					break;
				}
			}

			if (addThem) {
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
				boolean fUser = Boolean.parseBoolean(components[6]);
				boolean fSystem = Boolean.parseBoolean(components[7]);

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
						used = (PrivacyManager.getUsed(xAppInfo.getUid(), mRestrictionName, null) != 0);

					// Get if internet
					boolean internet = false;
					if (fInternet)
						internet = xAppInfo.hasInternet(mContext);

					// Get some restricted
					boolean someRestricted = false;
					if (fRestricted)
						for (ParcelableRestriction restriction : PrivacyManager.getRestrictionList(xAppInfo.getUid(),
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
						else if (PrivacyManager.hasPermission(mContext, xAppInfo, mRestrictionName)
								|| PrivacyManager.getUsed(xAppInfo.getUid(), mRestrictionName, null) > 0)
							permission = true;

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
							&& (fPermission ? permission : true) && (fUser ? user : true) && (fSystem ? system : true))
						lstApp.add(xAppInfo);
				}

				// Apply current sorting
				Collections.sort(lstApp, mSorter);

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
			public TextView tvName;
			public ImageView imgCBName;
			public RelativeLayout rlName;

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
				tvName = (TextView) row.findViewById(R.id.tvName);
				imgCBName = (ImageView) row.findViewById(R.id.imgCBName);
				rlName = (RelativeLayout) row.findViewById(R.id.rlName);
			}
		}

		private class HolderTask extends AsyncTask<Object, Object, Object> {
			private int position;
			private ViewHolder holder;
			private ApplicationInfoEx xAppInfo = null;
			private int state;
			private boolean used;
			private boolean enabled;
			private boolean granted;
			private List<String> listRestriction;
			private boolean crestricted;
			private boolean allRestricted;
			private boolean someRestricted;

			public HolderTask(int thePosition, ViewHolder theHolder, ApplicationInfoEx theAppInfo) {
				position = thePosition;
				holder = theHolder;
				xAppInfo = theAppInfo;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (xAppInfo != null) {
					// Get state
					state = xAppInfo.getState(ActivityMain.this);

					// Get if used
					used = (PrivacyManager.getUsed(xAppInfo.getUid(), mRestrictionName, null) != 0);

					// Get if enabled
					enabled = PrivacyManager.getSettingBool(null, xAppInfo.getUid(), PrivacyManager.cSettingRestricted,
							true, false);

					// Get if granted
					granted = true;
					if (mRestrictionName != null)
						if (!PrivacyManager.hasPermission(ActivityMain.this, xAppInfo, mRestrictionName))
							granted = false;

					// Get restrictions
					if (mRestrictionName == null)
						listRestriction = PrivacyManager.getRestrictions();
					else {
						listRestriction = new ArrayList<String>();
						listRestriction.add(mRestrictionName);
					}

					crestricted = false;
					for (String restrictionName : listRestriction)
						crestricted = crestricted
								|| PrivacyManager.getRestriction(null, xAppInfo.getUid(), restrictionName, null, false,
										false, null);

					// Get all/some restricted
					allRestricted = true;
					someRestricted = false;
					for (ParcelableRestriction restriction : PrivacyManager.getRestrictionList(xAppInfo.getUid(),
							mRestrictionName)) {
						allRestricted = (allRestricted && restriction.restricted);
						someRestricted = (someRestricted || restriction.restricted);
					}

					return holder;
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.position == position && result != null) {
					// Set background color
					if (xAppInfo.isSystem())
						holder.llAppType.setBackgroundColor(getResources().getColor(
								Util.getThemed(ActivityMain.this, R.attr.color_dangerous)));
					else
						holder.llAppType.setBackgroundColor(Color.TRANSPARENT);

					// Display state
					if (state == STATE_ATTENTION)
						holder.vwState.setBackgroundColor(getResources().getColor(
								Util.getThemed(ActivityMain.this, R.attr.color_state_attention)));
					else if (state == STATE_SHARED)
						holder.vwState.setBackgroundColor(getResources().getColor(
								Util.getThemed(ActivityMain.this, R.attr.color_state_shared)));
					else
						holder.vwState.setBackgroundColor(getResources().getColor(
								Util.getThemed(ActivityMain.this, R.attr.color_state_restricted)));

					// Display icon
					holder.imgIcon.setImageDrawable(xAppInfo.getIcon(ActivityMain.this));
					holder.imgIcon.setVisibility(View.VISIBLE);

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

					// Display restriction
					if (allRestricted)
						holder.imgCBName.setImageBitmap(mCheck[2]); // Full
					else if (someRestricted || crestricted)
						holder.imgCBName.setImageBitmap(mCheck[1]); // Half
					else
						holder.imgCBName.setImageBitmap(mCheck[0]); // Off
					holder.imgCBName.setVisibility(View.VISIBLE);

					// Display enabled state
					holder.tvName.setEnabled(enabled);
					holder.imgCBName.setEnabled(enabled);
					holder.rlName.setEnabled(enabled);

					// Display selection
					if (mListAppSelected.contains(xAppInfo))
						holder.row.setBackgroundColor(mHighlightColor);
					else
						holder.row.setBackgroundColor(Color.TRANSPARENT);

					// Listen for multiple select
					holder.rlName.setOnLongClickListener(new View.OnLongClickListener() {
						@Override
						public boolean onLongClick(View view) {
							if (mListAppSelected.contains(xAppInfo))
								mListAppSelected.remove(xAppInfo);
							else
								mListAppSelected.add(xAppInfo);

							if (mListAppSelected.contains(xAppInfo))
								holder.row.setBackgroundColor(mHighlightColor);
							else
								holder.row.setBackgroundColor(Color.TRANSPARENT);

							AppListAdapter.this.showStats();
							return true;
						}
					});

					// Listen for restriction changes
					holder.rlName.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(final View view) {
							// Process click
							if (mRestrictionName == null && someRestricted) {
								AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityMain.this);
								alertDialogBuilder.setTitle(R.string.menu_clear_all);
								alertDialogBuilder.setMessage(R.string.msg_sure);
								alertDialogBuilder.setIcon(Util.getThemed(ActivityMain.this, R.attr.icon_launcher));
								alertDialogBuilder.setPositiveButton(getString(android.R.string.ok),
										new DialogInterface.OnClickListener() {
											@Override
											public void onClick(DialogInterface dialog, int which) {
												// Update restriction
												boolean restart = PrivacyManager.deleteRestrictions(xAppInfo.getUid());
												allRestricted = false;
												someRestricted = false;

												// Update visible state
												holder.imgCBName.setImageBitmap(mCheck[0]); // Off
												holder.vwState.setBackgroundColor(getResources()
														.getColor(
																Util.getThemed(ActivityMain.this,
																		R.attr.color_state_attention)));

												// Notify restart
												if (restart)
													Toast.makeText(ActivityMain.this, getString(R.string.msg_restart),
															Toast.LENGTH_SHORT).show();
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
							} else {
								// Update restriction
								crestricted = false;
								for (String restrictionName : listRestriction)
									crestricted = crestricted
											|| PrivacyManager.getRestriction(null, xAppInfo.getUid(), restrictionName,
													null, false, false, null);
								crestricted = !crestricted;

								boolean restart = false;
								for (String restrictionName : listRestriction)
									restart = PrivacyManager.setRestriction(null, xAppInfo.getUid(), restrictionName,
											null, crestricted) || restart;

								// Update all/some restricted
								allRestricted = true;
								someRestricted = false;
								for (ParcelableRestriction restriction : PrivacyManager.getRestrictionList(
										xAppInfo.getUid(), mRestrictionName)) {
									allRestricted = (allRestricted && restriction.restricted);
									someRestricted = (someRestricted || restriction.restricted);
								}

								// Update visible state
								if (allRestricted)
									holder.imgCBName.setImageBitmap(mCheck[2]); // Full
								else if (someRestricted || crestricted)
									holder.imgCBName.setImageBitmap(mCheck[1]); // Half
								else
									holder.imgCBName.setImageBitmap(mCheck[0]); // Off

								// Notify restart
								if (restart)
									Toast.makeText(ActivityMain.this, getString(R.string.msg_restart),
											Toast.LENGTH_SHORT).show();
							}

							// Display new state
							state = xAppInfo.getState(ActivityMain.this);
							if (state == STATE_ATTENTION)
								holder.vwState.setBackgroundColor(getResources().getColor(
										Util.getThemed(ActivityMain.this, R.attr.color_state_attention)));
							else if (state == STATE_SHARED)
								holder.vwState.setBackgroundColor(getResources().getColor(
										Util.getThemed(ActivityMain.this, R.attr.color_state_shared)));
							else
								holder.vwState.setBackgroundColor(getResources().getColor(
										Util.getThemed(ActivityMain.this, R.attr.color_state_restricted)));
						}
					});
				}
			}
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			ViewHolder holder;
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

			// Handle details click
			holder.imgIcon.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					Intent intentSettings = new Intent(ActivityMain.this, ActivityApp.class);
					intentSettings.putExtra(ActivityApp.cUid, xAppInfo.getUid());
					intentSettings.putExtra(ActivityApp.cRestrictionName, mRestrictionName);
					intentSettings.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					ActivityMain.this.startActivity(intentSettings);
				}
			});

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
			holder.imgCBName.setVisibility(View.INVISIBLE);
			holder.tvName.setEnabled(false);
			holder.imgCBName.setEnabled(false);
			holder.rlName.setEnabled(false);

			// Async update
			new HolderTask(position, holder, xAppInfo).executeOnExecutor(mExecutor, (Object) null);

			return convertView;
		}
	}

	// Share operations progress listener

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

	// Helper methods

	private void checkLicense() {
		if (!Util.isProEnabled() && Util.hasProLicense(this) == null)
			if (Util.isProEnablerInstalled(this))
				try {
					Util.log(null, Log.INFO, "Licensing: check");
					startActivityForResult(new Intent("biz.bokhorst.xprivacy.pro.CHECK"), ACTIVITY_LICENSE);
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}
	}
}

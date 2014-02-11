package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.NotificationManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.provider.ContactsContract;
import android.provider.ContactsContract.CommonDataKinds.Phone;
import android.support.v4.app.NavUtils;
import android.support.v4.app.TaskStackBuilder;
import android.text.Html;
import android.text.TextUtils;
import android.text.format.DateUtils;
import android.text.method.LinkMovementMethod;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.view.Window;
import android.view.WindowManager;
import android.widget.BaseExpandableListAdapter;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.ExpandableListView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.PopupWindow;
import android.widget.RelativeLayout;
import android.widget.Switch;
import android.widget.TextView;
import android.widget.Toast;

public class ActivityApp extends ActivityBase {
	private int mThemeId;
	private ApplicationInfoEx mAppInfo = null;
	private RestrictionAdapter mPrivacyListAdapter = null;
	private Bitmap[] mCheck;

	public static final String cUid = "Uid";
	public static final String cRestrictionName = "RestrictionName";
	public static final String cMethodName = "MethodName";
	public static final String cAction = "Action";
	public static final int cActionClear = 1;
	public static final int cActionSettings = 2;
	public static final int cActionRefresh = 3;

	private static final int MENU_LAUNCH = 1;
	private static final int MENU_SETTINGS = 2;
	private static final int MENU_KILL = 3;
	private static final int MENU_STORE = 4;

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

	private boolean mPackageChangeReceiverRegistered = false;

	private BroadcastReceiver mPackageChangeReceiver = new BroadcastReceiver() {
		@Override
		public void onReceive(Context context, Intent intent) {
			int uid = intent.getIntExtra(Intent.EXTRA_UID, 0);
			if (mAppInfo.getUid() == uid)
				finish();
		}
	};

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Check privacy service client
		if (!PrivacyService.checkClient()) {
			setContentView(R.layout.reboot);
			return;
		}

		// Set theme
		String themeName = PrivacyManager.getSetting(0, PrivacyManager.cSettingTheme, "", false);
		mThemeId = (themeName.equals("Dark") ? R.style.CustomTheme : R.style.CustomTheme_Light);
		setTheme(mThemeId);

		// Set layout
		setContentView(R.layout.restrictionlist);

		// Get arguments
		Bundle extras = getIntent().getExtras();
		if (extras == null) {
			finish();
			return;
		}

		int uid = extras.getInt(cUid);
		String restrictionName = (extras.containsKey(cRestrictionName) ? extras.getString(cRestrictionName) : null);
		String methodName = (extras.containsKey(cMethodName) ? extras.getString(cMethodName) : null);

		// Get app info
		mAppInfo = new ApplicationInfoEx(this, uid);
		if (mAppInfo.getPackageName().size() == 0) {
			finish();
			return;
		}

		// Set title
		setTitle(String.format("%s - %s", getString(R.string.app_name),
				TextUtils.join(", ", mAppInfo.getApplicationName())));

		// Handle info click
		ImageView imgInfo = (ImageView) findViewById(R.id.imgInfo);
		imgInfo.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				// Packages can be selected on the web site
				Util.viewUri(ActivityApp.this, Uri.parse(String.format(ActivityShare.getBaseURL(ActivityApp.this)
						+ "?package_name=%s", mAppInfo.getPackageName().get(0))));
			}
		});

		// Display app name
		TextView tvAppName = (TextView) findViewById(R.id.tvApp);
		tvAppName.setText(mAppInfo.toString());

		// Background color
		if (mAppInfo.isSystem()) {
			LinearLayout llInfo = (LinearLayout) findViewById(R.id.llInfo);
			llInfo.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
		}

		// Display app icon
		final ImageView imgIcon = (ImageView) findViewById(R.id.imgIcon);
		imgIcon.setImageDrawable(mAppInfo.getIcon(this));

		// Handle icon click
		imgIcon.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				openContextMenu(imgIcon);
			}
		});

		// Display restriction state
		Switch swEnabled = (Switch) findViewById(R.id.swEnable);
		swEnabled.setChecked(PrivacyManager.getSettingBool(mAppInfo.getUid(), PrivacyManager.cSettingRestricted, true,
				false));
		swEnabled.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				PrivacyManager.setSetting(mAppInfo.getUid(), PrivacyManager.cSettingRestricted,
						Boolean.toString(isChecked));
			}
		});

		// Add context menu to icon
		registerForContextMenu(imgIcon);

		// Check if internet access
		if (!mAppInfo.hasInternet(this)) {
			ImageView imgInternet = (ImageView) findViewById(R.id.imgInternet);
			imgInternet.setVisibility(View.INVISIBLE);
		}

		// Check if frozen
		if (!mAppInfo.isFrozen(this)) {
			ImageView imgFrozen = (ImageView) findViewById(R.id.imgFrozen);
			imgFrozen.setVisibility(View.INVISIBLE);
		}

		// Display version
		TextView tvVersion = (TextView) findViewById(R.id.tvVersion);
		tvVersion.setText(TextUtils.join(", ", mAppInfo.getPackageVersionName(this)));

		// Display package name
		TextView tvPackageName = (TextView) findViewById(R.id.tvPackageName);
		tvPackageName.setText(TextUtils.join(", ", mAppInfo.getPackageName()));

		// Fill privacy list view adapter
		final ExpandableListView lvRestriction = (ExpandableListView) findViewById(R.id.elvRestriction);
		lvRestriction.setGroupIndicator(null);
		mPrivacyListAdapter = new RestrictionAdapter(R.layout.restrictionentry, mAppInfo, restrictionName, methodName);
		lvRestriction.setAdapter(mPrivacyListAdapter);
		if (restrictionName != null) {
			int groupPosition = new ArrayList<String>(PrivacyManager.getRestrictions(this).values())
					.indexOf(restrictionName);
			lvRestriction.expandGroup(groupPosition);
			lvRestriction.setSelectedGroup(groupPosition);
			if (methodName != null) {
				int childPosition = PrivacyManager.getHooks(restrictionName).indexOf(
						new Hook(restrictionName, methodName));
				lvRestriction.setSelectedChild(groupPosition, childPosition, true);
			}
		}

		// Listen for package add/remove
		IntentFilter iff = new IntentFilter();
		iff.addAction(Intent.ACTION_PACKAGE_REMOVED);
		iff.addDataScheme("package");
		registerReceiver(mPackageChangeReceiver, iff);
		mPackageChangeReceiverRegistered = true;

		// Up navigation
		getActionBar().setDisplayHomeAsUpEnabled(true);

		mCheck = getTriStateCheckBox();

		// Tutorial
		if (!PrivacyManager.getSettingBool(0, PrivacyManager.cSettingTutorialDetails, false, false)) {
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
				PrivacyManager.setSetting(0, PrivacyManager.cSettingTutorialDetails, Boolean.TRUE.toString());
			}
		};
		((Button) findViewById(R.id.btnTutorialHeader)).setOnClickListener(listener);
		((Button) findViewById(R.id.btnTutorialDetails)).setOnClickListener(listener);

		// Process actions
		if (extras.containsKey(cAction)) {
			NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
			notificationManager.cancel(mAppInfo.getUid());
			if (extras.getInt(cAction) == cActionClear)
				optionClear();
			else if (extras.getInt(cAction) == cActionSettings)
				optionSettings();
		}

		// Annotate
		Meta.annotate(this);
	}

	@Override
	protected void onNewIntent(Intent intent) {
		Bundle extras = intent.getExtras();
		if (extras != null && extras.containsKey(cAction) && extras.getInt(cAction) == cActionRefresh) {
			if (mPrivacyListAdapter != null)
				mPrivacyListAdapter.notifyDataSetChanged();
		} else {
			setIntent(intent);
			recreate();
		}
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mPrivacyListAdapter != null)
			mPrivacyListAdapter.notifyDataSetChanged();
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
	public boolean onCreateOptionsMenu(Menu menu) {
		if (PrivacyService.checkClient()) {
			MenuInflater inflater = getMenuInflater();
			inflater.inflate(R.menu.app, menu);
			return true;
		} else
			return false;
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		// Accounts
		boolean accountsRestricted = PrivacyManager.getRestrictionEx(mAppInfo.getUid(), PrivacyManager.cAccounts, null).restricted;
		boolean appsRestricted = PrivacyManager.getRestrictionEx(mAppInfo.getUid(), PrivacyManager.cSystem, null).restricted;
		boolean contactsRestricted = PrivacyManager.getRestrictionEx(mAppInfo.getUid(), PrivacyManager.cContacts, null).restricted;

		menu.findItem(R.id.menu_accounts).setEnabled(accountsRestricted);
		menu.findItem(R.id.menu_applications).setEnabled(appsRestricted);
		menu.findItem(R.id.menu_contacts).setEnabled(contactsRestricted);

		return super.onPrepareOptionsMenu(menu);
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		super.onCreateContextMenu(menu, v, menuInfo);

		PackageManager pm = getPackageManager();
		List<String> listPackageNames = mAppInfo.getPackageName();
		List<String> listApplicationName = mAppInfo.getApplicationName();
		for (int i = 0; i < listPackageNames.size(); i++) {
			Menu appMenu = (listPackageNames.size() == 1) ? menu : menu.addSubMenu(i, Menu.NONE, Menu.NONE,
					listApplicationName.get(i));

			// Launch
			MenuItem launch = appMenu.add(i, MENU_LAUNCH, Menu.NONE, getString(R.string.menu_app_launch));
			if (pm.getLaunchIntentForPackage(listPackageNames.get(i)) == null)
				launch.setEnabled(false);

			// Settings
			appMenu.add(i, MENU_SETTINGS, Menu.NONE, getString(R.string.menu_app_settings));

			// Kill
			MenuItem kill = appMenu.add(i, MENU_KILL, Menu.NONE, getString(R.string.menu_app_kill));
			kill.setEnabled(PrivacyManager.isApplication(mAppInfo.getUid()));

			// Play store
			MenuItem store = appMenu.add(i, MENU_STORE, Menu.NONE, getString(R.string.menu_app_store));
			if (!Util.hasMarketLink(this, listPackageNames.get(i)))
				store.setEnabled(false);
		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case android.R.id.home:
			Intent upIntent = NavUtils.getParentActivityIntent(this);
			if (upIntent != null)
				if (NavUtils.shouldUpRecreateTask(this, upIntent))
					TaskStackBuilder.create(this).addNextIntentWithParentStack(upIntent).startActivities();
				else
					NavUtils.navigateUpTo(this, upIntent);
			return true;
		case R.id.menu_help:
			optionHelp();
			return true;
		case R.id.menu_tutorial:
			optionTutorial();
			return true;
		case R.id.menu_apply:
			optionApply();
			return true;
		case R.id.menu_clear:
			optionClear();
			return true;
		case R.id.menu_usage:
			optionUsage();
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
		case R.id.menu_accounts:
			optionAccounts();
			return true;
		case R.id.menu_applications:
			optionApplications();
			return true;
		case R.id.menu_contacts:
			optionContacts();
			return true;
		case R.id.menu_settings:
			optionSettings();
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MENU_LAUNCH:
			optionLaunch(item.getGroupId());
			return true;
		case MENU_SETTINGS:
			optionAppSettings(item.getGroupId());
			return true;
		case MENU_KILL:
			optionKill(item.getGroupId());
			return true;
		case MENU_STORE:
			optionStore(item.getGroupId());
			return true;
		default:
			return super.onContextItemSelected(item);
		}
	}

	// Options

	private void optionHelp() {
		// Show help
		Dialog dialog = new Dialog(ActivityApp.this);
		dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dialog.setTitle(R.string.menu_help);
		dialog.setContentView(R.layout.help);
		dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, getThemed(R.attr.icon_launcher));
		ImageView imgHelpHalf = (ImageView) dialog.findViewById(R.id.imgHelpHalf);
		imgHelpHalf.setImageBitmap(mCheck[1]);
		dialog.setCancelable(true);
		dialog.show();
	}

	private void optionTutorial() {
		((RelativeLayout) findViewById(R.id.rlTutorialHeader)).setVisibility(View.VISIBLE);
		((RelativeLayout) findViewById(R.id.rlTutorialDetails)).setVisibility(View.VISIBLE);
		PrivacyManager.setSetting(0, PrivacyManager.cSettingTutorialDetails, Boolean.FALSE.toString());
	}

	private void optionApply() {
		final boolean ondemand = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingOnDemand, true, false);

		// Get toggle
		// TOD: getRestrictionList
		boolean some = false;
		final List<String> listRestriction = PrivacyManager.getRestrictions();
		for (String restrictionName : listRestriction) {
			String templateName = PrivacyManager.cSettingTemplate + "." + restrictionName;
			if (PrivacyManager.getSettingBool(0, templateName, !ondemand, false))
				if (PrivacyManager.getRestrictionEx(mAppInfo.getUid(), restrictionName, null).restricted) {
					some = true;
					break;
				}
		}
		final boolean restrict = !some;

		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
		alertDialogBuilder.setTitle(getString(restrict ? R.string.menu_apply : R.string.menu_clear_all));
		alertDialogBuilder.setMessage(R.string.msg_sure);
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
		alertDialogBuilder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				// Do toggle
				List<Boolean> oldState = PrivacyManager.getRestartStates(mAppInfo.getUid(), null);
				for (String restrictionName : listRestriction) {
					String templateName = PrivacyManager.cSettingTemplate + "." + restrictionName;
					if (PrivacyManager.getSettingBool(0, templateName, !ondemand, false))
						PrivacyManager.setRestriction(mAppInfo.getUid(), restrictionName, null, restrict, false);
				}
				List<Boolean> newState = PrivacyManager.getRestartStates(mAppInfo.getUid(), null);

				// Refresh display
				if (mPrivacyListAdapter != null)
					mPrivacyListAdapter.notifyDataSetChanged();

				// Notify restart
				if (!newState.equals(oldState))
					Toast.makeText(ActivityApp.this, getString(R.string.msg_restart), Toast.LENGTH_SHORT).show();
			}
		});
		alertDialogBuilder.setNegativeButton(getString(android.R.string.cancel), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
			}
		});
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private void optionClear() {
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
		alertDialogBuilder.setTitle(R.string.menu_clear_all);
		alertDialogBuilder.setMessage(R.string.msg_sure);
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
		alertDialogBuilder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				List<Boolean> oldState = PrivacyManager.getRestartStates(mAppInfo.getUid(), null);
				PrivacyManager.deleteRestrictions(mAppInfo.getUid(), null);

				// Refresh display
				if (mPrivacyListAdapter != null)
					mPrivacyListAdapter.notifyDataSetChanged();

				// Notify restart
				if (oldState.contains(true))
					Toast.makeText(ActivityApp.this, getString(R.string.msg_restart), Toast.LENGTH_SHORT).show();
			}
		});
		alertDialogBuilder.setNegativeButton(getString(android.R.string.cancel), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
			}
		});
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private void optionUsage() {
		Intent intent = new Intent(this, ActivityUsage.class);
		intent.putExtra(ActivityUsage.cUid, mAppInfo.getUid());
		startActivity(intent);
	}

	private void optionImport() {
		Intent intent = new Intent(ActivityShare.ACTION_IMPORT);
		int[] uid = new int[] { mAppInfo.getUid() };
		intent.putExtra(ActivityShare.cUidList, uid);
		intent.putExtra(ActivityShare.cInteractive, true);
		startActivity(intent);
	}

	private void optionSubmit() {
		if (ActivityShare.registerDevice(this)) {
			int[] uid = new int[] { mAppInfo.getUid() };
			Intent intent = new Intent("biz.bokhorst.xprivacy.action.SUBMIT");
			intent.putExtra(ActivityShare.cUidList, uid);
			intent.putExtra(ActivityShare.cInteractive, true);
			startActivity(intent);
		}
	}

	private void optionFetch() {
		int[] uid = new int[] { mAppInfo.getUid() };
		Intent intent = new Intent("biz.bokhorst.xprivacy.action.FETCH");
		intent.putExtra(ActivityShare.cUidList, uid);
		intent.putExtra(ActivityShare.cInteractive, true);
		startActivity(intent);
	}

	private void optionAccounts() {
		AccountsTask accountsTask = new AccountsTask();
		accountsTask.executeOnExecutor(mExecutor, (Object) null);
	}

	private void optionApplications() {
		if (Util.hasProLicense(this) == null) {
			// Redirect to pro page
			Util.viewUri(this, ActivityMain.cProUri);
		} else {
			ApplicationsTask appsTask = new ApplicationsTask();
			appsTask.executeOnExecutor(mExecutor, (Object) null);
		}
	}

	private void optionContacts() {
		if (Util.hasProLicense(this) == null) {
			// Redirect to pro page
			Util.viewUri(this, ActivityMain.cProUri);
		} else {
			ContactsTask contactsTask = new ContactsTask();
			contactsTask.executeOnExecutor(mExecutor, (Object) null);
		}
	}

	private void optionSettings() {
		SettingsDialog.edit(ActivityApp.this, mAppInfo);
	}

	private void optionLaunch(int which) {
		Intent intentLaunch = getPackageManager().getLaunchIntentForPackage(mAppInfo.getPackageName().get(which));
		startActivity(intentLaunch);
	}

	private void optionAppSettings(int which) {
		Intent intentSettings = new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS,
				Uri.parse("package:" + mAppInfo.getPackageName().get(which)));
		startActivity(intentSettings);
	}

	private void optionKill(final int which) {
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
		alertDialogBuilder.setTitle(R.string.menu_app_kill);
		alertDialogBuilder.setMessage(R.string.msg_sure);
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
		alertDialogBuilder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int _which) {
				XApplication.manage(ActivityApp.this, mAppInfo.getPackageName().get(which),
						XApplication.cActionKillProcess);
			}
		});
		alertDialogBuilder.setNegativeButton(getString(android.R.string.cancel), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
			}
		});
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private void optionStore(int which) {
		Util.viewUri(this, Uri.parse("market://details?id=" + mAppInfo.getPackageName().get(which)));
	}

	// Tasks

	private class AccountsTask extends AsyncTask<Object, Object, Object> {
		private List<CharSequence> mListAccount;
		private Account[] mAccounts;
		private boolean[] mSelection;

		@Override
		protected Object doInBackground(Object... params) {
			// Get accounts
			mListAccount = new ArrayList<CharSequence>();
			AccountManager accountManager = AccountManager.get(ActivityApp.this);
			mAccounts = accountManager.getAccounts();
			mSelection = new boolean[mAccounts.length];
			for (int i = 0; i < mAccounts.length; i++)
				try {
					mListAccount.add(String.format("%s (%s)", mAccounts[i].name, mAccounts[i].type));
					String sha1 = Util.sha1(mAccounts[i].name + mAccounts[i].type);
					mSelection[i] = PrivacyManager.getSettingBool(mAppInfo.getUid(), PrivacyManager.cSettingAccount
							+ sha1, false, false);
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}
			return null;
		}

		@Override
		protected void onPostExecute(Object result) {
			if (!ActivityApp.this.isFinishing()) {
				// Build dialog
				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
				alertDialogBuilder.setTitle(R.string.menu_accounts);
				alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
				alertDialogBuilder.setMultiChoiceItems(mListAccount.toArray(new CharSequence[0]), mSelection,
						new DialogInterface.OnMultiChoiceClickListener() {
							public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
								try {
									Account account = mAccounts[whichButton];
									String sha1 = Util.sha1(account.name + account.type);
									PrivacyManager.setSetting(mAppInfo.getUid(), PrivacyManager.cSettingAccount + sha1,
											Boolean.toString(isChecked));
								} catch (Throwable ex) {
									Util.bug(null, ex);
									Toast toast = Toast.makeText(ActivityApp.this, ex.toString(), Toast.LENGTH_LONG);
									toast.show();
								}
							}
						});
				alertDialogBuilder.setPositiveButton(getString(R.string.msg_done),
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

			super.onPostExecute(result);
		}
	}

	private class ApplicationsTask extends AsyncTask<Object, Object, Object> {
		private CharSequence[] mApp;
		private String[] mPackage;
		private boolean[] mSelection;

		@Override
		protected Object doInBackground(Object... params) {
			// Get applications
			List<ApplicationInfoEx> listInfo = ApplicationInfoEx.getXApplicationList(ActivityApp.this, null);

			// Count packages
			int packages = 0;
			for (ApplicationInfoEx appInfo : listInfo)
				packages += appInfo.getPackageName().size();

			// Build selection list
			int i = 0;
			mApp = new CharSequence[packages];
			mPackage = new String[packages];
			mSelection = new boolean[packages];
			for (ApplicationInfoEx appInfo : listInfo)
				for (int p = 0; p < appInfo.getPackageName().size(); p++)
					try {
						String appName = appInfo.getApplicationName().get(p);
						String pkgName = appInfo.getPackageName().get(p);
						mApp[i] = String.format("%s (%s)", appName, pkgName);
						mPackage[i] = pkgName;
						mSelection[i] = PrivacyManager.getSettingBool(mAppInfo.getUid(),
								PrivacyManager.cSettingApplication + pkgName, false, false);
						i++;
					} catch (Throwable ex) {
						Util.bug(null, ex);
					}
			return null;
		}

		@Override
		protected void onPostExecute(Object result) {
			if (!ActivityApp.this.isFinishing()) {
				// Build dialog
				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
				alertDialogBuilder.setTitle(R.string.menu_applications);
				alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
				alertDialogBuilder.setMultiChoiceItems(mApp, mSelection,
						new DialogInterface.OnMultiChoiceClickListener() {
							public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
								try {
									PrivacyManager.setSetting(mAppInfo.getUid(), PrivacyManager.cSettingApplication
											+ mPackage[whichButton], Boolean.toString(isChecked));
								} catch (Throwable ex) {
									Util.bug(null, ex);
									Toast toast = Toast.makeText(ActivityApp.this, ex.toString(), Toast.LENGTH_LONG);
									toast.show();
								}
							}
						});
				alertDialogBuilder.setPositiveButton(getString(R.string.msg_done),
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

			super.onPostExecute(result);
		}
	}

	private class ContactsTask extends AsyncTask<Object, Object, Object> {
		private List<CharSequence> mListContact;
		private long[] mIds;
		private boolean[] mSelection;

		@Override
		protected Object doInBackground(Object... params) {
			// Map contacts
			Map<Long, String> mapContact = new LinkedHashMap<Long, String>();
			Cursor cursor = getContentResolver().query(ContactsContract.Contacts.CONTENT_URI,
					new String[] { ContactsContract.Contacts._ID, Phone.DISPLAY_NAME }, null, null, Phone.DISPLAY_NAME);
			if (cursor != null)
				try {
					while (cursor.moveToNext()) {
						long id = cursor.getLong(cursor.getColumnIndex(ContactsContract.Contacts._ID));
						String contact = cursor.getString(cursor.getColumnIndex(Phone.DISPLAY_NAME));
						if (contact == null)
							contact = "-";
						mapContact.put(id, contact);
					}
				} finally {
					cursor.close();
				}

			// Build dialog data
			mListContact = new ArrayList<CharSequence>();
			mIds = new long[mapContact.size()];
			mSelection = new boolean[mapContact.size()];
			int i = 0;
			for (Long id : mapContact.keySet()) {
				mListContact.add(mapContact.get(id));
				mIds[i] = id;
				mSelection[i++] = PrivacyManager.getSettingBool(mAppInfo.getUid(), PrivacyManager.cSettingContact + id,
						false, false);
			}
			return null;
		}

		@Override
		protected void onPostExecute(Object result) {
			if (!ActivityApp.this.isFinishing()) {
				// Build dialog
				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
				alertDialogBuilder.setTitle(R.string.menu_contacts);
				alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
				alertDialogBuilder.setMultiChoiceItems(mListContact.toArray(new CharSequence[0]), mSelection,
						new DialogInterface.OnMultiChoiceClickListener() {
							public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
								// Contact
								PrivacyManager.setSetting(mAppInfo.getUid(), PrivacyManager.cSettingContact
										+ mIds[whichButton], Boolean.toString(isChecked));

								// Raw contacts
								Cursor cursor = getContentResolver().query(ContactsContract.RawContacts.CONTENT_URI,
										new String[] { ContactsContract.RawContacts._ID },
										ContactsContract.RawContacts.CONTACT_ID + "=?",
										new String[] { String.valueOf(mIds[whichButton]) }, null);
								try {
									while (cursor.moveToNext())
										PrivacyManager.setSetting(mAppInfo.getUid(), PrivacyManager.cSettingRawContact
												+ cursor.getLong(0), Boolean.toString(isChecked));
								} finally {
									cursor.close();
								}
							}
						});
				alertDialogBuilder.setPositiveButton(getString(R.string.msg_done),
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

			super.onPostExecute(result);
		}
	}

	// Adapters

	private class RestrictionAdapter extends BaseExpandableListAdapter {
		private ApplicationInfoEx mAppInfo;
		private String mSelectedRestrictionName;
		private String mSelectedMethodName;
		private List<String> mRestrictions;
		private HashMap<Integer, List<Hook>> mHook;
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

		public RestrictionAdapter(int resource, ApplicationInfoEx appInfo, String selectedRestrictionName,
				String selectedMethodName) {
			mAppInfo = appInfo;
			mSelectedRestrictionName = selectedRestrictionName;
			mSelectedMethodName = selectedMethodName;
			mRestrictions = new ArrayList<String>();
			mHook = new LinkedHashMap<Integer, List<Hook>>();

			boolean fUsed = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingFUsed, false, false);
			boolean fPermission = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingFPermission, false, false);

			for (String rRestrictionName : PrivacyManager.getRestrictions(ActivityApp.this).values()) {
				boolean isUsed = (PrivacyManager.getUsage(mAppInfo.getUid(), rRestrictionName, null) > 0);
				boolean hasPermission = PrivacyManager.hasPermission(ActivityApp.this, mAppInfo, rRestrictionName);
				if (mSelectedRestrictionName != null
						|| ((fUsed ? isUsed : true) && (fPermission ? isUsed || hasPermission : true)))
					mRestrictions.add(rRestrictionName);
			}
		}

		@Override
		public Object getGroup(int groupPosition) {
			return mRestrictions.get(groupPosition);
		}

		@Override
		public int getGroupCount() {
			return mRestrictions.size();
		}

		@Override
		public long getGroupId(int groupPosition) {
			return groupPosition;
		}

		private class GroupViewHolder {
			private View row;
			private int position;
			public ImageView imgIndicator;
			public ImageView imgUsed;
			public ImageView imgGranted;
			public ImageView imgInfo;
			public TextView tvName;
			public ImageView imgCBName;
			public RelativeLayout rlName;

			public GroupViewHolder(View theRow, int thePosition) {
				row = theRow;
				position = thePosition;
				imgIndicator = (ImageView) row.findViewById(R.id.imgIndicator);
				imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
				imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
				imgInfo = (ImageView) row.findViewById(R.id.imgInfo);
				tvName = (TextView) row.findViewById(R.id.tvName);
				imgCBName = (ImageView) row.findViewById(R.id.imgCBName);
				rlName = (RelativeLayout) row.findViewById(R.id.rlName);
			}
		}

		private class GroupHolderTask extends AsyncTask<Object, Object, Object> {
			private int position;
			private GroupViewHolder holder;
			private String restrictionName;
			private boolean used;
			private boolean permission;
			private RState rstate;

			public GroupHolderTask(int thePosition, GroupViewHolder theHolder, String theRestrictionName) {
				position = thePosition;
				holder = theHolder;
				restrictionName = theRestrictionName;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (restrictionName != null) {
					// Get info
					used = (PrivacyManager.getUsage(mAppInfo.getUid(), restrictionName, null) != 0);
					permission = PrivacyManager.hasPermission(ActivityApp.this, mAppInfo, restrictionName);
					rstate = RState.get(mAppInfo.getUid(), restrictionName, null);

					return holder;
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.position == position && result != null) {
					// Set data
					holder.tvName.setTypeface(null, used ? Typeface.BOLD_ITALIC : Typeface.NORMAL);
					holder.imgUsed.setVisibility(used ? View.VISIBLE : View.INVISIBLE);
					holder.imgGranted.setVisibility(permission ? View.VISIBLE : View.INVISIBLE);

					// Display restriction
					if (!rstate.asked)
						holder.imgCBName.setImageBitmap(mCheck[3]); // ?
					else if (rstate.partial)
						holder.imgCBName.setImageBitmap(mCheck[1]); // Half
					else if (rstate.restricted)
						holder.imgCBName.setImageBitmap(mCheck[2]); // Full
					else
						holder.imgCBName.setImageBitmap(mCheck[0]); // Off
					holder.imgCBName.setVisibility(View.VISIBLE);

					// Listen for restriction changes
					holder.rlName.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							// Set change
							boolean ask = rstate.asked;
							boolean restrict = ask ? !rstate.restricted : rstate.restricted;

							List<Boolean> oldState = PrivacyManager.getRestartStates(mAppInfo.getUid(), restrictionName);
							if (!restrict)
								PrivacyManager.deleteRestrictions(mAppInfo.getUid(), restrictionName);
							PrivacyManager.setRestriction(mAppInfo.getUid(), restrictionName, null, restrict, !ask);
							List<Boolean> newState = PrivacyManager.getRestartStates(mAppInfo.getUid(), restrictionName);

							// Refresh display
							notifyDataSetChanged(); // Needed to update children

							// Notify restart
							if (!newState.equals(oldState))
								Toast.makeText(ActivityApp.this, getString(R.string.msg_restart), Toast.LENGTH_SHORT)
										.show();
						}
					});
				}
			}
		}

		@Override
		public View getGroupView(int groupPosition, boolean isExpanded, View convertView, ViewGroup parent) {
			GroupViewHolder holder;
			if (convertView == null) {
				convertView = mInflater.inflate(R.layout.restrictionentry, null);
				holder = new GroupViewHolder(convertView, groupPosition);
				convertView.setTag(holder);
			} else {
				holder = (GroupViewHolder) convertView.getTag();
				holder.position = groupPosition;
			}

			// Get entry
			final String restrictionName = (String) getGroup(groupPosition);

			// Indicator state
			holder.imgIndicator.setImageResource(getThemed(isExpanded ? R.attr.icon_expander_maximized
					: R.attr.icon_expander_minimized));

			// Disable indicator for empty groups
			if (getChildrenCount(groupPosition) == 0)
				holder.imgIndicator.setVisibility(View.INVISIBLE);
			else
				holder.imgIndicator.setVisibility(View.VISIBLE);

			// Display if used
			holder.tvName.setTypeface(null, Typeface.NORMAL);
			holder.imgUsed.setVisibility(View.INVISIBLE);

			// Check if permission
			holder.imgGranted.setVisibility(View.INVISIBLE);

			// Handle info
			holder.imgInfo.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					Util.viewUri(ActivityApp.this, Uri.parse(ActivityMain.cXUrl + "#" + restrictionName));
				}
			});

			// Display localized name
			TreeMap<String, String> tmRestriction = PrivacyManager.getRestrictions(ActivityApp.this);
			int index = new ArrayList<String>(tmRestriction.values()).indexOf(restrictionName);
			String title = (String) tmRestriction.navigableKeySet().toArray()[index];
			holder.tvName.setText(title);

			// Display restriction
			holder.imgCBName.setVisibility(View.INVISIBLE);

			// Async update
			new GroupHolderTask(groupPosition, holder, restrictionName).executeOnExecutor(mExecutor, (Object) null);

			return convertView;
		}

		private List<Hook> getHooks(int groupPosition) {
			if (!mHook.containsKey(groupPosition)) {
				boolean fUsed = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingFUsed, false, false);
				boolean fPermission = PrivacyManager
						.getSettingBool(0, PrivacyManager.cSettingFPermission, false, false);
				List<Hook> listMethod = new ArrayList<Hook>();
				String restrictionName = mRestrictions.get(groupPosition);
				for (Hook md : PrivacyManager.getHooks((String) getGroup(groupPosition))) {
					boolean isUsed = (PrivacyManager.getUsage(mAppInfo.getUid(), restrictionName, md.getName()) > 0);
					boolean hasPermission = PrivacyManager.hasPermission(ActivityApp.this, mAppInfo, md);
					if (mSelectedMethodName != null
							|| ((fUsed ? isUsed : true) && (fPermission ? isUsed || hasPermission : true)))
						listMethod.add(md);
				}
				mHook.put(groupPosition, listMethod);
			}
			return mHook.get(groupPosition);
		}

		@Override
		public Object getChild(int groupPosition, int childPosition) {
			return getHooks(groupPosition).get(childPosition);
		}

		@Override
		public long getChildId(int groupPosition, int childPosition) {
			return childPosition;
		}

		@Override
		public int getChildrenCount(int groupPosition) {
			return getHooks(groupPosition).size();
		}

		@Override
		public boolean isChildSelectable(int groupPosition, int childPosition) {
			return false;
		}

		private class ChildViewHolder {
			private View row;
			private int groupPosition;
			private int childPosition;
			public ImageView imgUsed;
			public ImageView imgGranted;
			public ImageView imgInfo;
			public TextView tvMethodName;
			public ImageView imgCBMethodName;
			public RelativeLayout rlMethodName;

			private ChildViewHolder(View theRow, int gPosition, int cPosition) {
				row = theRow;
				groupPosition = gPosition;
				childPosition = cPosition;
				imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
				imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
				imgInfo = (ImageView) row.findViewById(R.id.imgInfo);
				tvMethodName = (TextView) row.findViewById(R.id.tvMethodName);
				imgCBMethodName = (ImageView) row.findViewById(R.id.imgCBMethodName);
				rlMethodName = (RelativeLayout) row.findViewById(R.id.rlMethodName);
			}
		}

		private class ChildHolderTask extends AsyncTask<Object, Object, Object> {
			private int groupPosition;
			private int childPosition;
			private ChildViewHolder holder;
			private String restrictionName;
			private Hook md;
			private long lastUsage;
			private PRestriction parent;
			private boolean permission;
			private RState rstate;

			public ChildHolderTask(int gPosition, int cPosition, ChildViewHolder theHolder, String theRestrictionName) {
				groupPosition = gPosition;
				childPosition = cPosition;
				holder = theHolder;
				restrictionName = theRestrictionName;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (restrictionName != null) {
					// Get info
					md = (Hook) getChild(groupPosition, childPosition);
					lastUsage = PrivacyManager.getUsage(mAppInfo.getUid(), restrictionName, md.getName());
					parent = PrivacyManager.getRestrictionEx(mAppInfo.getUid(), restrictionName, null);
					permission = PrivacyManager.hasPermission(ActivityApp.this, mAppInfo, md);
					rstate = RState.get(mAppInfo.getUid(), restrictionName, md.getName());

					return holder;
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.groupPosition == groupPosition && holder.childPosition == childPosition && result != null) {
					// Set data
					if (lastUsage > 0) {
						CharSequence sLastUsage = DateUtils.getRelativeTimeSpanString(lastUsage, new Date().getTime(),
								DateUtils.SECOND_IN_MILLIS, 0);
						holder.tvMethodName.setText(String.format("%s (%s)", md.getName(), sLastUsage));
					}
					holder.tvMethodName.setEnabled(parent.restricted || !parent.asked);
					holder.imgUsed.setImageResource(getThemed(md.hasUsageData() ? R.attr.icon_used
							: R.attr.icon_used_grayed));
					holder.imgUsed.setVisibility(lastUsage == 0 && md.hasUsageData() ? View.INVISIBLE : View.VISIBLE);
					holder.tvMethodName.setTypeface(null, lastUsage == 0 ? Typeface.NORMAL : Typeface.BOLD_ITALIC);
					holder.imgGranted.setVisibility(permission ? View.VISIBLE : View.INVISIBLE);

					// Display restriction
					if (!rstate.asked)
						holder.imgCBMethodName.setImageBitmap(mCheck[3]); // ?
					else if (rstate.partial)
						holder.imgCBMethodName.setImageBitmap(mCheck[1]); // Half
					else if (rstate.restricted)
						holder.imgCBMethodName.setImageBitmap(mCheck[2]); // Full
					else
						holder.imgCBMethodName.setImageBitmap(mCheck[0]); // Off
					holder.imgCBMethodName.setVisibility(View.VISIBLE);

					// Listen for restriction changes
					holder.rlMethodName.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							// Set change
							boolean ask = rstate.asked;
							boolean restrict = ask ? !rstate.restricted : rstate.restricted;

							PrivacyManager.setRestriction(mAppInfo.getUid(), restrictionName, md.getName(), restrict,
									!ask);

							// Refresh display
							notifyDataSetChanged(); // Needed to update parent

							// Notify restart
							if (md.isRestartRequired())
								Toast.makeText(ActivityApp.this, getString(R.string.msg_restart), Toast.LENGTH_SHORT)
										.show();
						}
					});

					// Listen for long press
					holder.rlMethodName.setOnLongClickListener(new View.OnLongClickListener() {
						@Override
						public boolean onLongClick(View view) {
							md.toggleDangerous();

							// Reset background color
							if (md.isDangerous())
								holder.row.setBackgroundColor(getResources()
										.getColor(getThemed(R.attr.color_dangerous)));
							else
								holder.row.setBackgroundColor(Color.TRANSPARENT);

							return true;
						}
					});
				}
			}
		}

		@Override
		public View getChildView(int groupPosition, int childPosition, boolean isLastChild, View convertView,
				ViewGroup parent) {
			ChildViewHolder holder;
			if (convertView == null) {
				convertView = mInflater.inflate(R.layout.restrictionchild, null);
				holder = new ChildViewHolder(convertView, groupPosition, childPosition);
				convertView.setTag(holder);
			} else {
				holder = (ChildViewHolder) convertView.getTag();
				holder.groupPosition = groupPosition;
				holder.childPosition = childPosition;
			}

			// Get entry
			final String restrictionName = (String) getGroup(groupPosition);
			final Hook md = (Hook) getChild(groupPosition, childPosition);

			// Set background color
			if (md.isDangerous())
				holder.row.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
			else
				holder.row.setBackgroundColor(Color.TRANSPARENT);

			// Display method name
			holder.tvMethodName.setText(md.getName());
			holder.tvMethodName.setEnabled(false);
			holder.tvMethodName.setTypeface(null, Typeface.NORMAL);

			// Display if used
			holder.imgUsed.setVisibility(View.INVISIBLE);

			// Display if permissions
			holder.imgGranted.setVisibility(View.INVISIBLE);

			final String annotation = md.getAnnotation();
			if (annotation == null)
				holder.imgInfo.setVisibility(View.GONE);
			else {
				holder.imgInfo.setVisibility(View.VISIBLE);
				holder.imgInfo.setOnClickListener(new View.OnClickListener() {
					@Override
					public void onClick(View view) {
						LayoutInflater inflator = LayoutInflater.from(ActivityApp.this);
						View layout = inflator.inflate(R.layout.popup, null);

						TextView tvTitle = (TextView) layout.findViewById(R.id.tvTitle);
						tvTitle.setText(md.getName());

						TextView tvInfo = (TextView) layout.findViewById(R.id.tvInfo);
						tvInfo.setText(Html.fromHtml(annotation));
						tvInfo.setMovementMethod(LinkMovementMethod.getInstance());

						final PopupWindow popup = new PopupWindow(layout);
						popup.setHeight(WindowManager.LayoutParams.WRAP_CONTENT);
						popup.setWidth(WindowManager.LayoutParams.WRAP_CONTENT);

						Button btnOk = (Button) layout.findViewById(R.id.btnOk);
						btnOk.setOnClickListener(new View.OnClickListener() {
							@Override
							public void onClick(View v) {
								popup.dismiss();
							}
						});

						popup.showAtLocation(view, Gravity.CENTER_HORIZONTAL | Gravity.CENTER_VERTICAL, 0, 0);
					}
				});
			}

			// Display restriction
			holder.tvMethodName.setClickable(false);

			// Async update
			new ChildHolderTask(groupPosition, childPosition, holder, restrictionName).executeOnExecutor(mExecutor,
					(Object) null);

			return convertView;
		}

		@Override
		public boolean hasStableIds() {
			return true;
		}
	}
}

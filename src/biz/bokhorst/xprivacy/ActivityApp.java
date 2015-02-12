package biz.bokhorst.xprivacy;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.annotation.SuppressLint;
import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
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
import android.graphics.Color;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Environment;
import android.os.Process;
import android.provider.ContactsContract;
import android.provider.ContactsContract.CommonDataKinds.GroupMembership;
import android.provider.ContactsContract.CommonDataKinds.Phone;
import android.support.v4.app.NavUtils;
import android.support.v4.app.TaskStackBuilder;
import android.support.v7.widget.SwitchCompat;
import android.support.v7.widget.Toolbar;
import android.text.Html;
import android.text.TextUtils;
import android.text.format.DateUtils;
import android.text.method.LinkMovementMethod;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.view.Window;
import android.widget.BaseExpandableListAdapter;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.ExpandableListView;
import android.widget.ExpandableListView.OnGroupExpandListener;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

public class ActivityApp extends ActivityBase {
	private ApplicationInfoEx mAppInfo = null;
	private SwitchCompat swEnabled = null;
	private RestrictionAdapter mPrivacyListAdapter = null;

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

		final int userId = Util.getUserId(Process.myUid());

		// Check privacy service client
		if (!PrivacyService.checkClient())
			return;

		// Set layout
		setContentView(R.layout.restrictionlist);
		setSupportActionBar((Toolbar) findViewById(R.id.widgetToolbar));

		// Annotate
		Meta.annotate(this.getResources());

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

		// Set sub title
		getSupportActionBar().setSubtitle(TextUtils.join(", ", mAppInfo.getApplicationName()));

		// Handle info click
		ImageView imgInfo = (ImageView) findViewById(R.id.imgInfo);
		imgInfo.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				// Packages can be selected on the web site
				Util.viewUri(ActivityApp.this, Uri.parse(String.format(ActivityShare.getBaseURL() + "?package_name=%s",
						mAppInfo.getPackageName().get(0))));
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

		// Display on-demand state
		final ImageView imgCbOnDemand = (ImageView) findViewById(R.id.imgCbOnDemand);
		boolean isApp = PrivacyManager.isApplication(mAppInfo.getUid());
		boolean odSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemandSystem, false);
		boolean gondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);
		if ((isApp || odSystem) && gondemand) {
			boolean ondemand = PrivacyManager
					.getSettingBool(-mAppInfo.getUid(), PrivacyManager.cSettingOnDemand, false);
			imgCbOnDemand.setImageBitmap(ondemand ? getOnDemandCheckBox() : getOffCheckBox());

			imgCbOnDemand.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					boolean ondemand = !PrivacyManager.getSettingBool(-mAppInfo.getUid(),
							PrivacyManager.cSettingOnDemand, false);
					PrivacyManager.setSetting(mAppInfo.getUid(), PrivacyManager.cSettingOnDemand,
							Boolean.toString(ondemand));
					imgCbOnDemand.setImageBitmap(ondemand ? getOnDemandCheckBox() : getOffCheckBox());
					if (mPrivacyListAdapter != null)
						mPrivacyListAdapter.notifyDataSetChanged();
				}
			});
		} else
			imgCbOnDemand.setVisibility(View.GONE);

		// Display restriction state
		swEnabled = (SwitchCompat) findViewById(R.id.swEnable);
		swEnabled.setChecked(PrivacyManager.getSettingBool(mAppInfo.getUid(), PrivacyManager.cSettingRestricted, true));
		swEnabled.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				PrivacyManager.setSetting(mAppInfo.getUid(), PrivacyManager.cSettingRestricted,
						Boolean.toString(isChecked));
				if (mPrivacyListAdapter != null)
					mPrivacyListAdapter.notifyDataSetChanged();
				imgCbOnDemand.setEnabled(isChecked);
			}
		});
		imgCbOnDemand.setEnabled(swEnabled.isChecked());

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

		// Fill privacy expandable list view adapter
		final ExpandableListView elvRestriction = (ExpandableListView) findViewById(R.id.elvRestriction);
		elvRestriction.setGroupIndicator(null);
		mPrivacyListAdapter = new RestrictionAdapter(this, R.layout.restrictionentry, mAppInfo, restrictionName,
				methodName);
		elvRestriction.setAdapter(mPrivacyListAdapter);

		// Listen for group expand
		elvRestriction.setOnGroupExpandListener(new OnGroupExpandListener() {
			@Override
			public void onGroupExpand(final int groupPosition) {
				if (!PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingMethodExpert, false)) {
					AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
					alertDialogBuilder.setTitle(R.string.app_name);
					alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
					alertDialogBuilder.setMessage(R.string.msg_method_expert);
					alertDialogBuilder.setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							PrivacyManager.setSetting(userId, PrivacyManager.cSettingMethodExpert,
									Boolean.toString(true));
							if (mPrivacyListAdapter != null)
								mPrivacyListAdapter.notifyDataSetChanged();
						}
					});
					alertDialogBuilder.setNegativeButton(android.R.string.no, new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							elvRestriction.collapseGroup(groupPosition);
						}
					});
					alertDialogBuilder.setCancelable(false);

					AlertDialog alertDialog = alertDialogBuilder.create();
					alertDialog.show();

				}
			}
		});

		// Go to method
		if (restrictionName != null) {
			int groupPosition = new ArrayList<String>(PrivacyManager.getRestrictions(this).values())
					.indexOf(restrictionName);
			elvRestriction.setSelectedGroup(groupPosition);
			elvRestriction.expandGroup(groupPosition);
			if (methodName != null) {
				Version version = new Version(Util.getSelfVersionName(this));
				int childPosition = PrivacyManager.getHooks(restrictionName, version).indexOf(
						new Hook(restrictionName, methodName));
				elvRestriction.setSelectedChild(groupPosition, childPosition, true);
			}
		}

		// Listen for package add/remove
		IntentFilter iff = new IntentFilter();
		iff.addAction(Intent.ACTION_PACKAGE_REMOVED);
		iff.addDataScheme("package");
		registerReceiver(mPackageChangeReceiver, iff);
		mPackageChangeReceiverRegistered = true;

		// Up navigation
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);

		// Tutorial
		if (!PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingTutorialDetails, false)) {
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
				PrivacyManager.setSetting(userId, PrivacyManager.cSettingTutorialDetails, Boolean.TRUE.toString());
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
	}

	@Override
	protected void onNewIntent(Intent intent) {
		Bundle extras = intent.getExtras();
		if (extras != null && extras.containsKey(cAction) && extras.getInt(cAction) == cActionRefresh) {
			// Update on demand check box
			ImageView imgCbOnDemand = (ImageView) findViewById(R.id.imgCbOnDemand);
			boolean ondemand = PrivacyManager
					.getSettingBool(-mAppInfo.getUid(), PrivacyManager.cSettingOnDemand, false);
			imgCbOnDemand.setImageBitmap(ondemand ? getOnDemandCheckBox() : getOffCheckBox());

			// Update restriction list
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

		// Update switch
		if (swEnabled != null)
			swEnabled.setChecked(PrivacyManager.getSettingBool(mAppInfo.getUid(), PrivacyManager.cSettingRestricted,
					true));

		// Update on demand check box
		int userId = Util.getUserId(Process.myUid());
		ImageView imgCbOnDemand = (ImageView) findViewById(R.id.imgCbOnDemand);
		boolean isApp = PrivacyManager.isApplication(mAppInfo.getUid());
		boolean odSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemandSystem, false);
		boolean gondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);
		if ((isApp || odSystem) && gondemand) {
			boolean ondemand = PrivacyManager
					.getSettingBool(-mAppInfo.getUid(), PrivacyManager.cSettingOnDemand, false);
			imgCbOnDemand.setImageBitmap(ondemand ? getOnDemandCheckBox() : getOffCheckBox());
		}

		// Update list
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
	public boolean onCreateOptionsMenu(final Menu menu) {
		MenuInflater inflater = getMenuInflater();
		if (inflater != null && PrivacyService.checkClient()) {
			inflater.inflate(R.menu.app, menu);

			// Add all contact groups
			menu.findItem(R.id.menu_contacts).getSubMenu()
					.add(-1, R.id.menu_contacts, Menu.NONE, getString(R.string.menu_all));

			// Add other contact groups in the background
			new AsyncTask<Object, Object, Object>() {
				@Override
				protected Object doInBackground(Object... arg0) {
					try {
						String where = ContactsContract.Groups.GROUP_VISIBLE + " = 1";
						where += " AND " + ContactsContract.Groups.SUMMARY_COUNT + " > 0";
						Cursor cursor = getContentResolver().query(
								ContactsContract.Groups.CONTENT_SUMMARY_URI,
								new String[] { ContactsContract.Groups._ID, ContactsContract.Groups.TITLE,
										ContactsContract.Groups.ACCOUNT_NAME, ContactsContract.Groups.SUMMARY_COUNT },
								where, null,
								ContactsContract.Groups.TITLE + ", " + ContactsContract.Groups.ACCOUNT_NAME);

						if (cursor != null)
							try {
								while (cursor.moveToNext()) {
									int id = cursor.getInt(cursor.getColumnIndex(ContactsContract.Groups._ID));
									String title = cursor.getString(cursor
											.getColumnIndex(ContactsContract.Groups.TITLE));
									String account = cursor.getString(cursor
											.getColumnIndex(ContactsContract.Groups.ACCOUNT_NAME));
									menu.findItem(R.id.menu_contacts).getSubMenu()
											.add(id, R.id.menu_contacts, Menu.NONE, title + "/" + account);
								}
							} finally {
								cursor.close();
							}
					} catch (Throwable ex) {
						Util.bug(null, ex);
					}

					return null;
				}
			}.executeOnExecutor(mExecutor);

			return true;
		} else
			return false;
	}

	// Application context menu

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		super.onCreateContextMenu(menu, v, menuInfo);

		// Check if running
		boolean running = false;
		ActivityManager activityManager = (ActivityManager) this.getSystemService(ACTIVITY_SERVICE);
		for (RunningAppProcessInfo info : activityManager.getRunningAppProcesses())
			if (info.uid == mAppInfo.getUid())
				running = true;

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
			kill.setEnabled(running && PrivacyManager.isApplication(mAppInfo.getUid()));

			// Play store
			MenuItem store = appMenu.add(i, MENU_STORE, Menu.NONE, getString(R.string.menu_app_store));
			if (!Util.hasMarketLink(this, listPackageNames.get(i)))
				store.setEnabled(false);
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
				Toast.makeText(ActivityApp.this, getString(R.string.msg_done), Toast.LENGTH_LONG).show();
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

	// Options

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		boolean accountsRestricted = PrivacyManager.getRestrictionEx(mAppInfo.getUid(), PrivacyManager.cAccounts, null).restricted;
		boolean appsRestricted = PrivacyManager.getRestrictionEx(mAppInfo.getUid(), PrivacyManager.cSystem, null).restricted;
		boolean contactsRestricted = PrivacyManager.getRestrictionEx(mAppInfo.getUid(), PrivacyManager.cContacts, null).restricted;

		menu.findItem(R.id.menu_accounts).setEnabled(accountsRestricted);
		menu.findItem(R.id.menu_applications).setEnabled(appsRestricted);
		menu.findItem(R.id.menu_contacts).setEnabled(contactsRestricted);

		boolean mounted = Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState());
		menu.findItem(R.id.menu_export).setEnabled(mounted);
		menu.findItem(R.id.menu_import).setEnabled(mounted);

		menu.findItem(R.id.menu_submit).setEnabled(Util.hasValidFingerPrint(this));

		menu.findItem(R.id.menu_dump).setVisible(Util.isDebuggable(this));

		return super.onPrepareOptionsMenu(menu);
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
		case R.id.menu_usage:
			optionUsage();
			return true;
		case R.id.menu_accounts:
			optionAccounts();
			return true;
		case R.id.menu_applications:
			optionApplications();
			return true;
		case R.id.menu_contacts:
			if (item.getGroupId() != 0) {
				optionContacts(item.getGroupId());
				return true;
			} else
				return false;
		case R.id.menu_whitelists:
			optionWhitelists(null);
			return true;
		case R.id.menu_apply:
			optionTemplate();
			return true;
		case R.id.menu_clear:
			optionClear();
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
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	private void optionUsage() {
		Intent intent = new Intent(this, ActivityUsage.class);
		intent.putExtra(ActivityUsage.cUid, mAppInfo.getUid());
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

	private void optionContacts(int groupId) {
		if (Util.hasProLicense(this) == null) {
			// Redirect to pro page
			Util.viewUri(this, ActivityMain.cProUri);
		} else {
			ContactsTask contactsTask = new ContactsTask();
			contactsTask.executeOnExecutor(mExecutor, groupId);
		}
	}

	private void optionWhitelists(String type) {
		if (Util.hasProLicense(this) == null) {
			// Redirect to pro page
			Util.viewUri(this, ActivityMain.cProUri);
		} else {
			WhitelistTask whitelistsTask = new WhitelistTask(mAppInfo.getUid(), type, this);
			whitelistsTask.executeOnExecutor(mExecutor, (Object) null);
		}
	}

	private void optionTemplate() {
		Intent intent = new Intent(ActivityShare.ACTION_TOGGLE);
		intent.putExtra(ActivityShare.cInteractive, true);
		intent.putExtra(ActivityShare.cUidList, new int[] { mAppInfo.getUid() });
		intent.putExtra(ActivityShare.cChoice, ActivityShare.CHOICE_TEMPLATE);
		startActivity(intent);
	}

	private void optionClear() {
		Intent intent = new Intent(ActivityShare.ACTION_TOGGLE);
		intent.putExtra(ActivityShare.cInteractive, true);
		intent.putExtra(ActivityShare.cUidList, new int[] { mAppInfo.getUid() });
		intent.putExtra(ActivityShare.cChoice, ActivityShare.CHOICE_CLEAR);
		startActivity(intent);
	}

	private void optionExport() {
		Intent intent = new Intent(ActivityShare.ACTION_EXPORT);
		intent.putExtra(ActivityShare.cUidList, new int[] { mAppInfo.getUid() });
		intent.putExtra(ActivityShare.cInteractive, true);
		startActivity(intent);
	}

	private void optionImport() {
		Intent intent = new Intent(ActivityShare.ACTION_IMPORT);
		intent.putExtra(ActivityShare.cUidList, new int[] { mAppInfo.getUid() });
		intent.putExtra(ActivityShare.cInteractive, true);
		startActivity(intent);
	}

	private void optionSubmit() {
		if (ActivityShare.registerDevice(this)) {
			Intent intent = new Intent("biz.bokhorst.xprivacy.action.SUBMIT");
			intent.putExtra(ActivityShare.cUidList, new int[] { mAppInfo.getUid() });
			intent.putExtra(ActivityShare.cInteractive, true);
			startActivity(intent);
		}
	}

	private void optionFetch() {
		Intent intent = new Intent("biz.bokhorst.xprivacy.action.FETCH");
		intent.putExtra(ActivityShare.cUidList, new int[] { mAppInfo.getUid() });
		intent.putExtra(ActivityShare.cInteractive, true);
		startActivity(intent);
	}

	private void optionSettings() {
		Intent intent = new Intent(ActivityApp.this, ActivitySettings.class);
		intent.putExtra(ActivitySettings.cUid, mAppInfo.getUid());
		startActivity(intent);
	}

	private void optionDump() {
		try {
			PrivacyService.getClient().dump(mAppInfo.getUid());
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private void optionLegend() {
		// Show help
		Dialog dialog = new Dialog(ActivityApp.this);
		dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dialog.setTitle(R.string.menu_legend);
		dialog.setContentView(R.layout.legend);
		dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, getThemed(R.attr.icon_launcher));

		((ImageView) dialog.findViewById(R.id.imgHelpHalf)).setImageBitmap(getHalfCheckBox());
		((ImageView) dialog.findViewById(R.id.imgHelpOnDemand)).setImageBitmap(getOnDemandCheckBox());

		for (View child : Util.getViewsByTag((ViewGroup) dialog.findViewById(android.R.id.content), "main"))
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
		PrivacyManager.setSetting(userId, PrivacyManager.cSettingTutorialDetails, Boolean.FALSE.toString());
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
					mSelection[i] = PrivacyManager.getSettingBool(-mAppInfo.getUid(), Meta.cTypeAccount, sha1, false);
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
									PrivacyManager.setSetting(mAppInfo.getUid(), Meta.cTypeAccount, sha1,
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
								if (mPrivacyListAdapter != null)
									mPrivacyListAdapter.notifyDataSetChanged();
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
			int count = 0;
			Map<String, List<String>> mapApp = new HashMap<String, List<String>>();
			for (ApplicationInfoEx appInfo : ApplicationInfoEx.getXApplicationList(ActivityApp.this, null))
				for (int p = 0; p < appInfo.getPackageName().size(); p++) {
					String appName = appInfo.getApplicationName().get(p);
					List<String> listPkg;
					if (mapApp.containsKey(appName))
						listPkg = mapApp.get(appName);
					else {
						listPkg = new ArrayList<String>();
						mapApp.put(appName, listPkg);
					}
					listPkg.add(appInfo.getPackageName().get(p));
					count++;
				}

			// Sort applications
			List<String> listApp = new ArrayList<String>(mapApp.keySet());
			Collator collator = Collator.getInstance(Locale.getDefault());
			Collections.sort(listApp, collator);

			// Build selection arrays
			int i = 0;
			mApp = new CharSequence[count];
			mPackage = new String[count];
			mSelection = new boolean[count];
			for (String appName : listApp) {
				List<String> listPkg = mapApp.get(appName);
				Collections.sort(listPkg, collator);
				for (String pkgName : listPkg) {
					mApp[i] = (pkgName.equals(appName) ? appName : String.format("%s (%s)", appName, pkgName));
					mPackage[i] = pkgName;
					mSelection[i] = PrivacyManager.getSettingBool(-mAppInfo.getUid(), Meta.cTypeApplication, pkgName,
							false);
					i++;
				}
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
									PrivacyManager.setSetting(mAppInfo.getUid(), Meta.cTypeApplication,
											mPackage[whichButton], Boolean.toString(isChecked));
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
								if (mPrivacyListAdapter != null)
									mPrivacyListAdapter.notifyDataSetChanged();
							}
						});

				// Show dialog
				AlertDialog alertDialog = alertDialogBuilder.create();
				alertDialog.show();
			}

			super.onPostExecute(result);
		}
	}

	private class ContactsTask extends AsyncTask<Integer, Object, Object> {
		private List<CharSequence> mListContact;
		private long[] mIds;
		private boolean[] mSelection;

		@Override
		protected Object doInBackground(Integer... params) {
			// Map contacts
			Map<Long, String> mapContact = new LinkedHashMap<Long, String>();
			Cursor cursor;
			if (params[0] < 0)
				cursor = getContentResolver().query(ContactsContract.Contacts.CONTENT_URI,
						new String[] { ContactsContract.Contacts._ID, Phone.DISPLAY_NAME }, null, null,
						Phone.DISPLAY_NAME);
			else
				cursor = getContentResolver()
						.query(ContactsContract.Data.CONTENT_URI,
								new String[] { ContactsContract.Contacts._ID, Phone.DISPLAY_NAME,
										GroupMembership.GROUP_ROW_ID }, GroupMembership.GROUP_ROW_ID + "= ?",
								new String[] { Integer.toString(params[0]) }, Phone.DISPLAY_NAME);
			if (cursor != null)
				try {
					while (cursor.moveToNext()) {
						long id = cursor.getLong(cursor.getColumnIndex(ContactsContract.Contacts._ID));
						String contact = cursor.getString(cursor.getColumnIndex(Phone.DISPLAY_NAME));
						if (contact != null)
							mapContact.put(id, contact);
					}
				} finally {
					cursor.close();
				}

			// Build dialog data
			mListContact = new ArrayList<CharSequence>();
			mIds = new long[mapContact.size() + 1];
			mSelection = new boolean[mapContact.size() + 1];

			mListContact.add("[" + getString(R.string.menu_all) + "]");
			mIds[0] = -1;
			mSelection[0] = false;

			int i = 0;
			for (Long id : mapContact.keySet()) {
				mListContact.add(mapContact.get(id));
				mIds[i + 1] = id;
				mSelection[i++ + 1] = PrivacyManager.getSettingBool(-mAppInfo.getUid(), Meta.cTypeContact,
						Long.toString(id), false);
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
							public void onClick(final DialogInterface dialog, int whichButton, final boolean isChecked) {
								// Contact
								if (whichButton == 0) {
									((AlertDialog) dialog).getListView().setEnabled(false);
									((AlertDialog) dialog).getButton(AlertDialog.BUTTON_POSITIVE).setEnabled(false);
									((AlertDialog) dialog).setCancelable(false);

									new AsyncTask<Object, Object, Object>() {
										@Override
										protected Object doInBackground(Object... arg0) {
											for (int i = 1; i < mSelection.length; i++) {
												mSelection[i] = isChecked;
												PrivacyManager.setSetting(mAppInfo.getUid(), Meta.cTypeContact,
														Long.toString(mIds[i]), Boolean.toString(mSelection[i]));
											}
											return null;
										}

										@Override
										protected void onPostExecute(Object result) {
											for (int i = 1; i < mSelection.length; i++)
												((AlertDialog) dialog).getListView().setItemChecked(i, mSelection[i]);

											((AlertDialog) dialog).getListView().setEnabled(true);
											((AlertDialog) dialog).getButton(AlertDialog.BUTTON_POSITIVE).setEnabled(
													true);
											((AlertDialog) dialog).setCancelable(true);
										}
									}.executeOnExecutor(mExecutor);

								} else
									PrivacyManager.setSetting(mAppInfo.getUid(), Meta.cTypeContact,
											Long.toString(mIds[whichButton]), Boolean.toString(isChecked));
							}
						});
				alertDialogBuilder.setPositiveButton(getString(R.string.msg_done),
						new DialogInterface.OnClickListener() {
							@Override
							public void onClick(DialogInterface dialog, int which) {
								if (mPrivacyListAdapter != null)
									mPrivacyListAdapter.notifyDataSetChanged();
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
		private Context mContext;
		private ApplicationInfoEx mAppInfo;
		private String mSelectedRestrictionName;
		private String mSelectedMethodName;
		private List<String> mListRestriction;
		private HashMap<Integer, List<Hook>> mHook;
		private Version mVersion;
		private LayoutInflater mInflater;

		public RestrictionAdapter(Context context, int resource, ApplicationInfoEx appInfo,
				String selectedRestrictionName, String selectedMethodName) {
			mContext = context;
			mAppInfo = appInfo;
			mSelectedRestrictionName = selectedRestrictionName;
			mSelectedMethodName = selectedMethodName;
			mListRestriction = new ArrayList<String>();
			mHook = new LinkedHashMap<Integer, List<Hook>>();
			mVersion = new Version(Util.getSelfVersionName(context));
			mInflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);

			int userId = Util.getUserId(Process.myUid());
			boolean fUsed = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFUsed, false);
			boolean fPermission = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFPermission, false);

			for (String rRestrictionName : PrivacyManager.getRestrictions(mContext).values()) {
				boolean isUsed = (PrivacyManager.getUsage(mAppInfo.getUid(), rRestrictionName, null) > 0);
				boolean hasPermission = PrivacyManager.hasPermission(mContext, mAppInfo, rRestrictionName, mVersion);
				if (mSelectedRestrictionName != null
						|| ((fUsed ? isUsed : true) && (fPermission ? isUsed || hasPermission : true)))
					mListRestriction.add(rRestrictionName);
			}
		}

		@Override
		public Object getGroup(int groupPosition) {
			return mListRestriction.get(groupPosition);
		}

		@Override
		public int getGroupCount() {
			return mListRestriction.size();
		}

		@Override
		public long getGroupId(int groupPosition) {
			return groupPosition * 1000;
		}

		private class GroupViewHolder {
			private View row;
			private int position;
			public ImageView imgIndicator;
			public ImageView imgUsed;
			public ImageView imgGranted;
			public ImageView imgInfo;
			public TextView tvName;
			public ImageView imgWhitelist;
			public ImageView imgCbRestricted;
			public ProgressBar pbRunning;
			public ImageView imgCbAsk;
			public LinearLayout llName;

			public GroupViewHolder(View theRow, int thePosition) {
				row = theRow;
				position = thePosition;
				imgIndicator = (ImageView) row.findViewById(R.id.imgIndicator);
				imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
				imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
				imgInfo = (ImageView) row.findViewById(R.id.imgInfo);
				tvName = (TextView) row.findViewById(R.id.tvName);
				imgWhitelist = (ImageView) row.findViewById(R.id.imgWhitelist);
				imgCbRestricted = (ImageView) row.findViewById(R.id.imgCbRestricted);
				pbRunning = (ProgressBar) row.findViewById(R.id.pbRunning);
				imgCbAsk = (ImageView) row.findViewById(R.id.imgCbAsk);
				llName = (LinearLayout) row.findViewById(R.id.llName);
			}
		}

		private class GroupHolderTask extends AsyncTask<Object, Object, Object> {
			private int position;
			private GroupViewHolder holder;
			private String restrictionName;
			private boolean used;
			private boolean permission;
			private RState rstate;
			private boolean ondemand;
			private boolean whitelist;
			private boolean enabled;
			private boolean can;
			private boolean methodExpert;

			public GroupHolderTask(int thePosition, GroupViewHolder theHolder, String theRestrictionName) {
				position = thePosition;
				holder = theHolder;
				restrictionName = theRestrictionName;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (restrictionName != null) {
					// Get info
					int userId = Util.getUserId(Process.myUid());
					used = (PrivacyManager.getUsage(mAppInfo.getUid(), restrictionName, null) != 0);
					permission = PrivacyManager.hasPermission(mContext, mAppInfo, restrictionName, mVersion);
					rstate = new RState(mAppInfo.getUid(), restrictionName, null, mVersion);

					boolean isApp = PrivacyManager.isApplication(mAppInfo.getUid());
					boolean odSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemandSystem,
							false);
					boolean gondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);
					ondemand = ((isApp || odSystem) && gondemand);
					if (ondemand)
						ondemand = PrivacyManager.getSettingBool(-mAppInfo.getUid(), PrivacyManager.cSettingOnDemand,
								false);

					whitelist = false;
					String wName = null;
					if (PrivacyManager.cAccounts.equals(restrictionName))
						wName = Meta.cTypeAccount;
					else if (PrivacyManager.cSystem.equals(restrictionName))
						wName = Meta.cTypeApplication;
					else if (PrivacyManager.cContacts.equals(restrictionName))
						wName = Meta.cTypeContact;
					if (wName != null) {
						boolean blacklist = PrivacyManager.getSettingBool(-mAppInfo.getUid(),
								PrivacyManager.cSettingBlacklist, false);
						if (blacklist)
							whitelist = true;
						else
							for (PSetting setting : PrivacyManager.getSettingList(mAppInfo.getUid(), wName))
								if (Boolean.parseBoolean(setting.value)) {
									whitelist = true;
									break;
								}
					}
					if (!whitelist)
						for (Hook hook : PrivacyManager.getHooks(restrictionName, mVersion))
							if (hook.whitelist() != null)
								if (PrivacyManager.getSettingList(mAppInfo.getUid(), hook.whitelist()).size() > 0) {
									whitelist = true;
									break;
								}

					enabled = PrivacyManager.getSettingBool(mAppInfo.getUid(), PrivacyManager.cSettingRestricted, true);
					can = PrivacyManager.canRestrict(rstate.mUid, Process.myUid(), rstate.mRestrictionName, null, true);
					methodExpert = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingMethodExpert, false);

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

					// Show whitelist icon
					holder.imgWhitelist.setVisibility(whitelist ? View.VISIBLE : View.GONE);
					if (whitelist)
						holder.imgWhitelist.setOnClickListener(new View.OnClickListener() {
							@Override
							public void onClick(View view) {
								if (PrivacyManager.cAccounts.equals(restrictionName))
									optionAccounts();
								else if (PrivacyManager.cSystem.equals(restrictionName))
									optionApplications();
								else if (PrivacyManager.cContacts.equals(restrictionName))
									optionContacts(-1);
							}
						});
					else
						holder.imgWhitelist.setClickable(false);

					// Display restriction
					holder.imgCbRestricted.setImageBitmap(getCheckBoxImage(rstate, methodExpert));
					holder.imgCbRestricted.setVisibility(View.VISIBLE);
					if (ondemand) {
						holder.imgCbAsk.setImageBitmap(getAskBoxImage(rstate, methodExpert));
						holder.imgCbAsk.setVisibility(View.VISIBLE);
					} else
						holder.imgCbAsk.setVisibility(View.GONE);

					// Check if can be restricted
					holder.llName.setEnabled(enabled && can);
					holder.tvName.setEnabled(enabled && can);
					holder.imgCbAsk.setEnabled(enabled && can);

					// Listen for restriction changes
					holder.llName.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							holder.llName.setEnabled(false);
							holder.imgCbRestricted.setVisibility(View.GONE);
							holder.pbRunning.setVisibility(View.VISIBLE);

							new AsyncTask<Object, Object, Object>() {
								private List<Boolean> oldState;
								private List<Boolean> newState;

								@Override
								protected Object doInBackground(Object... arg0) {
									// Change restriction
									oldState = PrivacyManager.getRestartStates(mAppInfo.getUid(), restrictionName);
									rstate.toggleRestriction();
									newState = PrivacyManager.getRestartStates(mAppInfo.getUid(), restrictionName);
									return null;
								}

								@Override
								protected void onPostExecute(Object result) {
									// Refresh display
									// Needed to update children
									notifyDataSetChanged();

									// Notify restart
									if (!newState.equals(oldState))
										Toast.makeText(mContext, getString(R.string.msg_restart), Toast.LENGTH_LONG)
												.show();

									holder.pbRunning.setVisibility(View.GONE);
									holder.imgCbRestricted.setVisibility(View.VISIBLE);
									holder.llName.setEnabled(true);
								}
							}.executeOnExecutor(mExecutor);
						}
					});

					// Listen for ask changes
					if (ondemand)
						holder.imgCbAsk.setOnClickListener(new View.OnClickListener() {
							@Override
							public void onClick(View view) {
								holder.imgCbAsk.setVisibility(View.GONE);
								holder.pbRunning.setVisibility(View.VISIBLE);

								new AsyncTask<Object, Object, Object>() {
									@Override
									protected Object doInBackground(Object... arg0) {
										rstate.toggleAsked();
										return null;
									}

									@Override
									protected void onPostExecute(Object result) {
										// Needed to update children
										notifyDataSetChanged();

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
		}

		@Override
		@SuppressLint("InflateParams")
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

			// Display localized name
			TreeMap<String, String> tmRestriction = PrivacyManager.getRestrictions(mContext);
			int index = new ArrayList<String>(tmRestriction.values()).indexOf(restrictionName);
			final String title = (String) tmRestriction.navigableKeySet().toArray()[index];
			holder.tvName.setText(title);
			holder.imgWhitelist.setVisibility(View.GONE);

			// Display restriction
			holder.imgCbRestricted.setVisibility(View.INVISIBLE);
			holder.imgCbAsk.setVisibility(View.INVISIBLE);

			// Async update
			new GroupHolderTask(groupPosition, holder, restrictionName).executeOnExecutor(mExecutor, (Object) null);

			// Handle info
			holder.imgInfo.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					int stringId = getResources().getIdentifier("restrict_help_" + restrictionName, "string",
							getPackageName());

					// Build dialog
					Dialog dlgHelp = new Dialog(mContext);
					dlgHelp.requestWindowFeature(Window.FEATURE_LEFT_ICON);
					dlgHelp.setTitle(title);
					dlgHelp.setContentView(R.layout.helpcat);
					dlgHelp.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON,
							ActivityApp.this.getThemed(R.attr.icon_launcher));
					dlgHelp.setCancelable(true);

					// Set info
					TextView tvInfo = (TextView) dlgHelp.findViewById(R.id.tvInfo);
					tvInfo.setText(stringId);

					dlgHelp.show();
				}
			});

			return convertView;
		}

		private List<Hook> getHooks(int groupPosition) {
			if (!mHook.containsKey(groupPosition)) {
				int userId = Util.getUserId(Process.myUid());
				boolean fUsed = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFUsed, false);
				boolean fPermission = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingFPermission, false);
				List<Hook> listMethod = new ArrayList<Hook>();
				String restrictionName = mListRestriction.get(groupPosition);
				for (Hook hook : PrivacyManager.getHooks((String) getGroup(groupPosition), mVersion)) {
					// Filter
					boolean isUsed = (PrivacyManager.getUsage(mAppInfo.getUid(), restrictionName, hook.getName()) > 0);
					boolean hasPermission = PrivacyManager.hasPermission(mContext, mAppInfo, hook);
					if (mSelectedMethodName != null
							|| ((fUsed ? isUsed : true) && (fPermission ? isUsed || hasPermission : true)))
						listMethod.add(hook);
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
			return groupPosition * 1000 + childPosition;
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
			public ImageView imgUnsafe;
			public ImageView imgMethodWhitelist;
			public ImageView imgCbMethodRestricted;
			public ProgressBar pbRunning;
			public ImageView imgCbMethodAsk;
			public LinearLayout llMethodName;

			private ChildViewHolder(View theRow, int gPosition, int cPosition) {
				row = theRow;
				groupPosition = gPosition;
				childPosition = cPosition;
				imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
				imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
				imgInfo = (ImageView) row.findViewById(R.id.imgInfo);
				tvMethodName = (TextView) row.findViewById(R.id.tvMethodName);
				imgUnsafe = (ImageView) row.findViewById(R.id.imgUnsafe);
				imgMethodWhitelist = (ImageView) row.findViewById(R.id.imgMethodWhitelist);
				imgCbMethodRestricted = (ImageView) row.findViewById(R.id.imgCbMethodRestricted);
				pbRunning = (ProgressBar) row.findViewById(R.id.pbRunning);
				imgCbMethodAsk = (ImageView) row.findViewById(R.id.imgCbMethodAsk);
				llMethodName = (LinearLayout) row.findViewById(R.id.llMethodName);
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
			private boolean ondemand;
			private boolean whitelist;
			private boolean enabled;
			private boolean can;

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
					int userId = Util.getUserId(Process.myUid());
					md = (Hook) getChild(groupPosition, childPosition);
					lastUsage = PrivacyManager.getUsage(mAppInfo.getUid(), restrictionName, md.getName());
					parent = PrivacyManager.getRestrictionEx(mAppInfo.getUid(), restrictionName, null);
					permission = PrivacyManager.hasPermission(mContext, mAppInfo, md);
					rstate = new RState(mAppInfo.getUid(), restrictionName, md.getName(), mVersion);

					boolean isApp = PrivacyManager.isApplication(mAppInfo.getUid());
					boolean odSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemandSystem,
							false);
					boolean gondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);

					ondemand = ((isApp || odSystem) && gondemand);
					if (ondemand)
						ondemand = PrivacyManager.getSettingBool(-mAppInfo.getUid(), PrivacyManager.cSettingOnDemand,
								false);

					if (md.whitelist() == null)
						whitelist = false;
					else
						whitelist = PrivacyManager.listWhitelisted(mAppInfo.getUid(), md.whitelist()).size() > 0;

					enabled = PrivacyManager.getSettingBool(mAppInfo.getUid(), PrivacyManager.cSettingRestricted, true);
					can = PrivacyManager.canRestrict(rstate.mUid, Process.myUid(), rstate.mRestrictionName,
							rstate.mMethodName, true);
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
					holder.llMethodName.setEnabled(parent.restricted);
					holder.tvMethodName.setEnabled(parent.restricted);
					holder.imgCbMethodAsk.setEnabled(!parent.asked);

					holder.imgUsed.setImageResource(getThemed(md.hasUsageData() && enabled && can ? R.attr.icon_used
							: R.attr.icon_used_grayed));
					holder.imgUsed.setVisibility(lastUsage == 0 && md.hasUsageData() && enabled && can ? View.INVISIBLE
							: View.VISIBLE);
					holder.tvMethodName.setTypeface(null, lastUsage == 0 ? Typeface.NORMAL : Typeface.BOLD_ITALIC);
					holder.imgGranted.setVisibility(permission ? View.VISIBLE : View.INVISIBLE);

					// Show whitelist icon
					holder.imgMethodWhitelist.setVisibility(whitelist ? View.VISIBLE : View.INVISIBLE);
					if (whitelist)
						holder.imgMethodWhitelist.setOnClickListener(new View.OnClickListener() {
							@Override
							public void onClick(View view) {
								ActivityApp.this.optionWhitelists(md.whitelist());
							}
						});
					else
						holder.imgMethodWhitelist.setClickable(false);

					// Display restriction
					holder.imgCbMethodRestricted.setImageBitmap(getCheckBoxImage(rstate, true));
					holder.imgCbMethodRestricted.setVisibility(View.VISIBLE);

					// Show asked state
					if (ondemand) {
						holder.imgCbMethodAsk.setImageBitmap(getAskBoxImage(rstate, true));
						holder.imgCbMethodAsk.setVisibility(md.canOnDemand() ? View.VISIBLE : View.INVISIBLE);
					} else
						holder.imgCbMethodAsk.setVisibility(View.GONE);

					holder.llMethodName.setEnabled(enabled && can);
					holder.tvMethodName.setEnabled(enabled && can);
					holder.imgCbMethodAsk.setEnabled(enabled && can);

					// Listen for restriction changes
					if (parent.restricted)
						holder.llMethodName.setOnClickListener(new View.OnClickListener() {
							@Override
							public void onClick(View view) {
								holder.llMethodName.setEnabled(false);
								holder.imgCbMethodRestricted.setVisibility(View.GONE);
								holder.pbRunning.setVisibility(View.VISIBLE);

								new AsyncTask<Object, Object, Object>() {
									@Override
									protected Object doInBackground(Object... arg0) {
										// Change restriction
										rstate.toggleRestriction();
										return null;
									}

									@Override
									protected void onPostExecute(Object result) {
										// Refresh display
										// Needed to update parent
										notifyDataSetChanged();

										// Notify restart
										if (md.isRestartRequired())
											Toast.makeText(mContext, getString(R.string.msg_restart), Toast.LENGTH_LONG)
													.show();

										holder.pbRunning.setVisibility(View.GONE);
										holder.imgCbMethodRestricted.setVisibility(View.VISIBLE);
										holder.llMethodName.setEnabled(true);
									}
								}.executeOnExecutor(mExecutor);
							}
						});
					else
						holder.llMethodName.setClickable(false);

					// Listen for ask changes
					if (ondemand && !parent.asked)
						holder.imgCbMethodAsk.setOnClickListener(new View.OnClickListener() {
							@Override
							public void onClick(View view) {
								holder.imgCbMethodAsk.setVisibility(View.GONE);
								holder.pbRunning.setVisibility(View.VISIBLE);

								new AsyncTask<Object, Object, Object>() {
									@Override
									protected Object doInBackground(Object... arg0) {
										rstate.toggleAsked();
										return null;
									}

									@Override
									protected void onPostExecute(Object result) {
										// Needed to update parent
										notifyDataSetChanged();

										holder.pbRunning.setVisibility(View.GONE);
										holder.imgCbMethodAsk.setVisibility(View.VISIBLE);
									}
								}.executeOnExecutor(mExecutor);
							}
						});
					else
						holder.imgCbMethodAsk.setClickable(false);
				}
			}
		}

		@Override
		@SuppressLint("InflateParams")
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
			final Hook hook = (Hook) getChild(groupPosition, childPosition);

			// Set background color
			if (hook.isDangerous())
				holder.row.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
			else
				holder.row.setBackgroundColor(Color.TRANSPARENT);

			holder.llMethodName.setEnabled(false);
			holder.tvMethodName.setEnabled(false);
			holder.imgCbMethodAsk.setEnabled(false);

			// Display method name
			holder.tvMethodName.setText(hook.getName());
			holder.tvMethodName.setTypeface(null, Typeface.NORMAL);

			// Display if used
			holder.imgUsed.setVisibility(View.INVISIBLE);

			// Hide if permissions
			holder.imgGranted.setVisibility(View.INVISIBLE);

			// Function help
			if (hook.getAnnotation() == null)
				holder.imgInfo.setVisibility(View.GONE);
			else {
				holder.imgInfo.setVisibility(View.VISIBLE);
				holder.imgInfo.setOnClickListener(new View.OnClickListener() {
					@Override
					public void onClick(View view) {
						View parent = ActivityApp.this.findViewById(android.R.id.content);
						showHelp(ActivityApp.this, parent, hook);
					}
				});
			}

			// Show if unsafe
			holder.imgUnsafe.setVisibility(hook != null && hook.isUnsafe() ? View.VISIBLE : View.INVISIBLE);

			// Hide whitelist icon
			holder.imgMethodWhitelist.setVisibility(View.INVISIBLE);

			// Display restriction
			holder.llMethodName.setClickable(false);
			holder.imgCbMethodRestricted.setVisibility(View.INVISIBLE);
			holder.imgCbMethodAsk.setVisibility(View.INVISIBLE);

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

	@SuppressLint("InflateParams")
	public static void showHelp(ActivityBase context, View parent, Hook hook) {
		// Build dialog
		Dialog dlgHelp = new Dialog(context);
		dlgHelp.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgHelp.setTitle(R.string.app_name);
		dlgHelp.setContentView(R.layout.helpfunc);
		dlgHelp.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, context.getThemed(R.attr.icon_launcher));
		dlgHelp.setCancelable(true);

		// Set title
		TextView tvTitle = (TextView) dlgHelp.findViewById(R.id.tvTitle);
		tvTitle.setText(hook.getName());

		// Set info
		TextView tvInfo = (TextView) dlgHelp.findViewById(R.id.tvInfo);
		tvInfo.setText(Html.fromHtml(hook.getAnnotation()));
		tvInfo.setMovementMethod(LinkMovementMethod.getInstance());

		// Set permissions
		String[] permissions = hook.getPermissions();
		if (permissions != null && permissions.length > 0)
			if (!permissions[0].equals("")) {
				TextView tvPermissions = (TextView) dlgHelp.findViewById(R.id.tvPermissions);
				tvPermissions.setText(Html.fromHtml(TextUtils.join("<br />", permissions)));
			}

		dlgHelp.show();
	}
}

package biz.bokhorst.xprivacy;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

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

import android.accounts.Account;
import android.accounts.AccountManager;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.NotificationManager;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.provider.ContactsContract;
import android.provider.ContactsContract.CommonDataKinds.Phone;
import android.provider.Settings.Secure;
import android.support.v4.app.NavUtils;
import android.support.v4.app.TaskStackBuilder;
import android.text.format.DateUtils;
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
import android.widget.CheckedTextView;
import android.widget.ExpandableListView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

public class ActivityApp extends Activity {

	private int mThemeId;
	private ApplicationInfoEx mAppInfo = null;
	private RestrictionAdapter mPrivacyListAdapter = null;
	private Bitmap[] mCheck;

	public static final String cPackageName = "PackageName";
	public static final String cRestrictionName = "RestrictionName";
	public static final String cMethodName = "MethodName";
	public static final String cActionClear = "Clear";

	private static final int ACTIVITY_FETCH = 1;

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

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Set theme
		String themeName = PrivacyManager.getSetting(null, this, 0, PrivacyManager.cSettingTheme, "", false);
		mThemeId = (themeName.equals("Dark") ? R.style.CustomTheme : R.style.CustomTheme_Light);
		setTheme(mThemeId);

		// Set layout
		setContentView(R.layout.restrictionlist);

		// Get arguments
		Bundle extras = getIntent().getExtras();
		String packageName = extras.getString(cPackageName);
		String restrictionName = (extras.containsKey(cRestrictionName) ? extras.getString(cRestrictionName) : null);
		String methodName = (extras.containsKey(cMethodName) ? extras.getString(cMethodName) : null);

		// Get app info
		try {
			mAppInfo = new ApplicationInfoEx(this, packageName);
		} catch (NameNotFoundException ignored) {
			finish();
			return;
		}

		// Set title
		setTitle(String.format("%s - %s", getString(R.string.app_name), mAppInfo.getFirstApplicationName()));

		// Handle info click
		ImageView imgInfo = (ImageView) findViewById(R.id.imgInfo);
		imgInfo.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				Intent infoIntent = new Intent(Intent.ACTION_VIEW);
				infoIntent.setData(Uri.parse(String.format(ActivityShare.BASE_URL + "?package_name=%s",
						mAppInfo.getPackageName())));
				startActivity(infoIntent);
			}
		});

		// Display app name
		TextView tvAppName = (TextView) findViewById(R.id.tvApp);
		tvAppName.setText(mAppInfo.toString());

		// Background color
		if (mAppInfo.isSystem()) {
			LinearLayout llInfo = (LinearLayout) findViewById(R.id.llInfo);
			llInfo.setBackgroundColor(getResources().getColor(Util.getThemed(this, R.attr.color_dangerous)));
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
		tvVersion.setText(mAppInfo.getVersion(this));

		// Display package name
		TextView tvPackageName = (TextView) findViewById(R.id.tvPackageName);
		tvPackageName.setText(mAppInfo.getPackageName());

		// Fill privacy list view adapter
		final ExpandableListView lvRestriction = (ExpandableListView) findViewById(R.id.elvRestriction);
		lvRestriction.setGroupIndicator(null);
		mPrivacyListAdapter = new RestrictionAdapter(R.layout.restrictionentry, mAppInfo, restrictionName, methodName);
		lvRestriction.setAdapter(mPrivacyListAdapter);
		if (restrictionName != null) {
			int groupPosition = PrivacyManager.getRestrictions().indexOf(restrictionName);
			lvRestriction.expandGroup(groupPosition);
			lvRestriction.setSelectedGroup(groupPosition);
			if (methodName != null) {
				int childPosition = PrivacyManager.getMethods(restrictionName).indexOf(
						new PrivacyManager.MethodDescription(methodName));
				lvRestriction.setSelectedChild(groupPosition, childPosition, true);
			}
		}

		// Up navigation
		getActionBar().setDisplayHomeAsUpEnabled(true);

		mCheck = Util.getTriStateCheckBox(this);

		// Tutorial
		if (!PrivacyManager.getSettingBool(null, this, 0, PrivacyManager.cSettingTutorialDetails, false, false)) {
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
				PrivacyManager.setSetting(null, ActivityApp.this, 0, PrivacyManager.cSettingTutorialDetails,
						Boolean.TRUE.toString());
			}
		};
		((Button) findViewById(R.id.btnTutorialHeader)).setOnClickListener(listener);
		((Button) findViewById(R.id.btnTutorialDetails)).setOnClickListener(listener);

		// Clear
		if (extras.containsKey(cActionClear)) {
			NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
			notificationManager.cancel(mAppInfo.getUid());
			optionClear();
		}
	}

	@Override
	protected void onNewIntent(Intent intent) {
		super.onNewIntent(intent);
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mPrivacyListAdapter != null)
			mPrivacyListAdapter.notifyDataSetChanged();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.app, menu);

		return true;
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		super.onCreateContextMenu(menu, v, menuInfo);
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.app_icon, menu);

		// Launch
		PackageManager pm = getPackageManager();
		if (pm.getLaunchIntentForPackage(mAppInfo.getPackageName()) == null)
			menu.findItem(R.id.menu_app_launch).setEnabled(false);

		// Play
		boolean hasMarketLink = Util.hasMarketLink(this, mAppInfo.getPackageName());
		menu.findItem(R.id.menu_app_store).setEnabled(hasMarketLink);
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		// Accounts
		boolean accountsRestricted = PrivacyManager.getRestricted(null, this, mAppInfo.getUid(),
				PrivacyManager.cAccounts, null, false, false);
		boolean appsRestricted = PrivacyManager.getRestricted(null, this, mAppInfo.getUid(), PrivacyManager.cSystem,
				null, false, false);
		boolean contactsRestricted = PrivacyManager.getRestricted(null, this, mAppInfo.getUid(),
				PrivacyManager.cContacts, null, false, false);
		menu.findItem(R.id.menu_accounts).setEnabled(accountsRestricted);
		menu.findItem(R.id.menu_applications).setEnabled(appsRestricted);
		menu.findItem(R.id.menu_contacts).setEnabled(contactsRestricted);

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
		case R.id.menu_submit:
			optionSubmit();
			return true;
		case R.id.menu_fetch:
			optionFetch();
			return true;
		case R.id.menu_app_launch:
			optionLaunch();
			return true;
		case R.id.menu_app_settings:
			optionSettings();
			return true;
		case R.id.menu_app_store:
			optionStore();
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
			SettingsDialog.edit(ActivityApp.this, mAppInfo);
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_app_launch:
			optionLaunch();
			return true;
		case R.id.menu_app_settings:
			optionSettings();
			return true;
		case R.id.menu_app_store:
			optionStore();
			return true;
		default:
			return super.onContextItemSelected(item);
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent dataIntent) {
		super.onActivityResult(requestCode, resultCode, dataIntent);
		if (requestCode == ACTIVITY_FETCH) {
			if (mPrivacyListAdapter != null)
				mPrivacyListAdapter.notifyDataSetChanged();

			String errorMessage = null;
			if (dataIntent != null && dataIntent.hasExtra(ActivityShare.cErrorMessage))
				errorMessage = dataIntent.getStringExtra(ActivityShare.cErrorMessage);

			String text = String.format("%s: %s", getString(R.string.menu_fetch),
					errorMessage == null ? getString(R.string.msg_done) : errorMessage);
			Toast toast = Toast.makeText(this, text, Toast.LENGTH_LONG);
			toast.show();
		}
	}

	// Options

	private void optionHelp() {
		// Show help
		Dialog dialog = new Dialog(ActivityApp.this);
		dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dialog.setTitle(getString(R.string.menu_help));
		dialog.setContentView(R.layout.help);
		dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, Util.getThemed(this, R.attr.icon_launcher));
		ImageView imgHelpHalf = (ImageView) dialog.findViewById(R.id.imgHelpHalf);
		imgHelpHalf.setImageBitmap(mCheck[1]);
		dialog.setCancelable(true);
		dialog.show();
	}

	private void optionTutorial() {
		((RelativeLayout) findViewById(R.id.rlTutorialHeader)).setVisibility(View.VISIBLE);
		((RelativeLayout) findViewById(R.id.rlTutorialDetails)).setVisibility(View.VISIBLE);
		PrivacyManager.setSetting(null, this, 0, PrivacyManager.cSettingTutorialDetails, Boolean.FALSE.toString());
	}

	private void optionApply() {
		// Get toggle
		boolean some = false;
		final List<String> listRestriction = PrivacyManager.getRestrictions();
		for (String restrictionName : listRestriction) {
			String templateName = PrivacyManager.cSettingTemplate + "." + restrictionName;
			if (PrivacyManager.getSettingBool(null, ActivityApp.this, 0, templateName, true, false))
				if (PrivacyManager.getRestricted(null, ActivityApp.this, mAppInfo.getUid(), restrictionName, null,
						false, false)) {
					some = true;
					break;
				}
		}
		final boolean restricted = !some;

		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
		alertDialogBuilder.setTitle(getString(restricted ? R.string.menu_apply : R.string.menu_clear_all));
		alertDialogBuilder.setMessage(getString(R.string.msg_sure));
		alertDialogBuilder.setIcon(Util.getThemed(this, R.attr.icon_launcher));
		alertDialogBuilder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				// Do toggle
				boolean restart = false;
				for (String restrictionName : listRestriction) {
					String templateName = PrivacyManager.cSettingTemplate + "." + restrictionName;
					if (PrivacyManager.getSettingBool(null, ActivityApp.this, 0, templateName, true, false))
						restart = PrivacyManager.setRestricted(null, ActivityApp.this, mAppInfo.getUid(),
								restrictionName, null, restricted) || restart;
				}

				// Refresh display
				if (mPrivacyListAdapter != null)
					mPrivacyListAdapter.notifyDataSetChanged();

				// Notify restart
				if (restart)
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
		alertDialogBuilder.setTitle(getString(R.string.menu_clear_all));
		alertDialogBuilder.setMessage(getString(R.string.msg_sure));
		alertDialogBuilder.setIcon(Util.getThemed(this, R.attr.icon_launcher));
		alertDialogBuilder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				boolean restart = PrivacyManager.deleteRestrictions(ActivityApp.this, mAppInfo.getUid());

				// Refresh display
				if (mPrivacyListAdapter != null)
					mPrivacyListAdapter.notifyDataSetChanged();

				// Notify restart
				if (restart)
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
		intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
		startActivity(intent);
	}

	private void optionSubmit() {
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
		alertDialogBuilder.setTitle(getString(R.string.menu_submit));
		alertDialogBuilder.setMessage(getString(R.string.msg_sure));
		alertDialogBuilder.setIcon(Util.getThemed(this, R.attr.icon_launcher));
		alertDialogBuilder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				SubmitTask submitTask = new SubmitTask();
				submitTask.executeOnExecutor(mExecutor, mAppInfo);
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

	private void optionFetch() {
		if (Util.getProLicense() == null) {
			// Redirect to pro page
			Intent browserIntent = new Intent(Intent.ACTION_VIEW, ActivityMain.cProUri);
			startActivity(browserIntent);
		} else {
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
			alertDialogBuilder.setTitle(getString(R.string.menu_fetch));
			alertDialogBuilder.setMessage(getString(R.string.msg_sure));
			alertDialogBuilder.setIcon(Util.getThemed(this, R.attr.icon_launcher));
			alertDialogBuilder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int which) {
					Intent intent = new Intent("biz.bokhorst.xprivacy.action.FETCH");
					intent.putExtra(ActivityShare.cPackageName, mAppInfo.getPackageName());
					startActivityForResult(intent, ACTIVITY_FETCH);
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
		}
	}

	private void optionAccounts() {
		AccountsTask accountsTask = new AccountsTask();
		accountsTask.executeOnExecutor(mExecutor, (Object) null);
	}

	private void optionApplications() {
		if (Util.getProLicense() == null) {
			// Redirect to pro page
			Intent browserIntent = new Intent(Intent.ACTION_VIEW, ActivityMain.cProUri);
			startActivity(browserIntent);
		} else {
			ApplicationsTask appsTask = new ApplicationsTask();
			appsTask.executeOnExecutor(mExecutor, (Object) null);
		}
	}

	private void optionContacts() {
		if (Util.getProLicense() == null) {
			// Redirect to pro page
			Intent browserIntent = new Intent(Intent.ACTION_VIEW, ActivityMain.cProUri);
			startActivity(browserIntent);
		} else {
			ContactsTask contactsTask = new ContactsTask();
			contactsTask.executeOnExecutor(mExecutor, (Object) null);
		}
	}

	private void optionLaunch() {
		Intent intentLaunch = getPackageManager().getLaunchIntentForPackage(mAppInfo.getPackageName());
		startActivity(intentLaunch);
	}

	private void optionSettings() {
		Intent intentSettings = new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS,
				Uri.parse("package:" + mAppInfo.getPackageName()));
		startActivity(intentSettings);
	}

	private void optionStore() {
		Intent intentStore = new Intent(Intent.ACTION_VIEW, Uri.parse("market://details?id="
				+ mAppInfo.getPackageName()));
		startActivity(intentStore);
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
					mSelection[i] = PrivacyManager.getSettingBool(null, ActivityApp.this, 0,
							String.format("Account.%d.%s", mAppInfo.getUid(), sha1), false, false);
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}
			return null;
		}

		@Override
		protected void onPostExecute(Object result) {
			// Build dialog
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
			alertDialogBuilder.setTitle(getString(R.string.menu_accounts));
			alertDialogBuilder.setIcon(Util.getThemed(ActivityApp.this, R.attr.icon_launcher));
			alertDialogBuilder.setMultiChoiceItems(mListAccount.toArray(new CharSequence[0]), mSelection,
					new DialogInterface.OnMultiChoiceClickListener() {
						public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
							try {
								Account account = mAccounts[whichButton];
								String sha1 = Util.sha1(account.name + account.type);
								PrivacyManager.setSetting(null, ActivityApp.this, 0,
										String.format("Account.%d.%s", mAppInfo.getUid(), sha1),
										Boolean.toString(isChecked));
							} catch (Throwable ex) {
								Util.bug(null, ex);
								Toast toast = Toast.makeText(ActivityApp.this, ex.toString(), Toast.LENGTH_LONG);
								toast.show();
							}
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

			super.onPostExecute(result);
		}
	}

	private class ApplicationsTask extends AsyncTask<Object, Object, Object> {
		private List<ApplicationInfo> mListInfo;
		private List<CharSequence> mListApp;
		private boolean[] mSelection;

		@Override
		protected Object doInBackground(Object... params) {
			// Get applications
			final PackageManager pm = ActivityApp.this.getPackageManager();
			mListInfo = pm.getInstalledApplications(PackageManager.GET_META_DATA);
			Collections.sort(mListInfo, new Comparator<ApplicationInfo>() {
				public int compare(ApplicationInfo info1, ApplicationInfo info2) {
					return ((String) pm.getApplicationLabel(info1)).compareTo(((String) pm.getApplicationLabel(info2)));
				}
			});

			// Build selection list
			mListApp = new ArrayList<CharSequence>();
			mSelection = new boolean[mListInfo.size()];
			for (int i = 0; i < mListInfo.size(); i++)
				try {
					mListApp.add(String.format("%s (%s)", pm.getApplicationLabel(mListInfo.get(i)),
							mListInfo.get(i).packageName));
					mSelection[i] = PrivacyManager.getSettingBool(null, ActivityApp.this, 0,
							String.format("Application.%d.%s", mAppInfo.getUid(), mListInfo.get(i).packageName), false,
							false);
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}
			return null;
		}

		@Override
		protected void onPostExecute(Object result) {
			// Build dialog
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
			alertDialogBuilder.setTitle(getString(R.string.menu_applications));
			alertDialogBuilder.setIcon(Util.getThemed(ActivityApp.this, R.attr.icon_launcher));
			alertDialogBuilder.setMultiChoiceItems(mListApp.toArray(new CharSequence[0]), mSelection,
					new DialogInterface.OnMultiChoiceClickListener() {
						public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
							try {
								PrivacyManager.setSetting(
										null,
										ActivityApp.this,
										0,
										String.format("Application.%d.%s", mAppInfo.getUid(),
												mListInfo.get(whichButton).packageName), Boolean.toString(isChecked));
							} catch (Throwable ex) {
								Util.bug(null, ex);
								Toast toast = Toast.makeText(ActivityApp.this, ex.toString(), Toast.LENGTH_LONG);
								toast.show();
							}
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
				mSelection[i++] = PrivacyManager.getSettingBool(null, ActivityApp.this, 0,
						String.format("Contact.%d.%d", mAppInfo.getUid(), id), false, false);
			}
			return null;
		}

		@Override
		protected void onPostExecute(Object result) {
			// Build dialog
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
			alertDialogBuilder.setTitle(getString(R.string.menu_contacts));
			alertDialogBuilder.setIcon(Util.getThemed(ActivityApp.this, R.attr.icon_launcher));
			alertDialogBuilder.setMultiChoiceItems(mListContact.toArray(new CharSequence[0]), mSelection,
					new DialogInterface.OnMultiChoiceClickListener() {
						public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
							// Contact
							PrivacyManager.setSetting(null, ActivityApp.this, 0,
									String.format("Contact.%d.%d", mAppInfo.getUid(), mIds[whichButton]),
									Boolean.toString(isChecked));

							// Raw contacts
							Cursor cursor = getContentResolver().query(ContactsContract.RawContacts.CONTENT_URI,
									new String[] { ContactsContract.RawContacts._ID },
									ContactsContract.RawContacts.CONTACT_ID + "=?",
									new String[] { String.valueOf(mIds[whichButton]) }, null);
							try {
								while (cursor.moveToNext()) {
									PrivacyManager.setSetting(null, ActivityApp.this, 0,
											String.format("RawContact.%d.%d", mAppInfo.getUid(), cursor.getLong(0)),
											Boolean.toString(isChecked));
								}
							} finally {
								cursor.close();
							}
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

			super.onPostExecute(result);
		}
	}

	@SuppressLint("DefaultLocale")
	private class SubmitTask extends AsyncTask<ApplicationInfoEx, Object, String> {
		@Override
		protected String doInBackground(ApplicationInfoEx... params) {
			try {
				// Encode restrictions
				int uid = params[0].getUid();
				JSONArray jSettings = new JSONArray();
				for (String restrictionName : PrivacyManager.getRestrictions()) {
					boolean restricted = PrivacyManager.getRestricted(null, ActivityApp.this, uid, restrictionName,
							null, false, false);
					// Category
					long used = PrivacyManager.getUsed(ActivityApp.this, uid, restrictionName, null);
					JSONObject jRestriction = new JSONObject();
					jRestriction.put("restriction", restrictionName);
					jRestriction.put("restricted", restricted);
					jRestriction.put("used", used);
					jSettings.put(jRestriction);

					// Methods
					for (PrivacyManager.MethodDescription md : PrivacyManager.getMethods(restrictionName)) {
						boolean mRestricted = restricted
								&& PrivacyManager.getRestricted(null, ActivityApp.this, uid, restrictionName,
										md.getName(), false, false);
						long mUsed = PrivacyManager.getUsed(ActivityApp.this, uid, restrictionName, md.getName());
						JSONObject jMethod = new JSONObject();
						jMethod.put("restriction", restrictionName);
						jMethod.put("method", md.getName());
						jMethod.put("restricted", mRestricted);
						jMethod.put("used", mUsed);
						jSettings.put(jMethod);
					}
				}

				// Get data
				PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
				String android_id = Secure.getString(ActivityApp.this.getContentResolver(), Secure.ANDROID_ID);

				// Encode package
				JSONObject jRoot = new JSONObject();
				jRoot.put("protocol_version", 4);
				jRoot.put("android_id", Util.md5(android_id).toLowerCase());
				jRoot.put("android_sdk", Build.VERSION.SDK_INT);
				jRoot.put("xprivacy_version", pInfo.versionCode);
				jRoot.put("application_name", params[0].getFirstApplicationName());
				jRoot.put("package_name", params[0].getPackageName());
				jRoot.put("package_version", params[0].getVersion(ActivityApp.this));
				jRoot.put("settings", jSettings);

				// Submit
				HttpParams httpParams = new BasicHttpParams();
				HttpConnectionParams.setConnectionTimeout(httpParams, ActivityShare.TIMEOUT_MILLISEC);
				HttpConnectionParams.setSoTimeout(httpParams, ActivityShare.TIMEOUT_MILLISEC);
				HttpClient httpclient = new DefaultHttpClient(httpParams);

				HttpPost httpost = new HttpPost(ActivityShare.BASE_URL + "?format=json&action=submit");
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
						PrivacyManager.setSetting(null, ActivityApp.this, mAppInfo.getUid(),
								PrivacyManager.cSettingState, Integer.toString(ActivityMain.STATE_SHARED));
						return getString(R.string.msg_done);
					} else
						return status.getString("error");
				} else {
					// Failed
					response.getEntity().getContent().close();
					return statusLine.getReasonPhrase();
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
				return ex.toString();
			}
		}

		@Override
		protected void onPostExecute(String result) {
			Toast toast = Toast.makeText(ActivityApp.this,
					String.format("%s: %s", getString(R.string.menu_submit), result), Toast.LENGTH_LONG);
			toast.show();
			super.onPostExecute(result);
		}
	}

	// Adapters

	private class RestrictionAdapter extends BaseExpandableListAdapter {
		private ApplicationInfoEx mAppInfo;
		private String mSelectedRestrictionName;
		private String mSelectedMethodName;
		private List<String> mRestrictions;
		private HashMap<Integer, List<PrivacyManager.MethodDescription>> mMethodDescription;
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

		public RestrictionAdapter(int resource, ApplicationInfoEx appInfo, String selectedRestrictionName,
				String selectedMethodName) {
			mAppInfo = appInfo;
			mSelectedRestrictionName = selectedRestrictionName;
			mSelectedMethodName = selectedMethodName;
			mRestrictions = new ArrayList<String>();
			mMethodDescription = new LinkedHashMap<Integer, List<PrivacyManager.MethodDescription>>();

			boolean fUsed = PrivacyManager.getSettingBool(null, ActivityApp.this, 0, PrivacyManager.cSettingFUsed,
					false, false);
			boolean fPermission = PrivacyManager.getSettingBool(null, ActivityApp.this, 0,
					PrivacyManager.cSettingFPermission, false, false);

			for (String rRestrictionName : PrivacyManager.getRestrictions()) {
				boolean isUsed = (PrivacyManager.getUsed(ActivityApp.this, mAppInfo.getUid(), rRestrictionName, null) > 0);
				boolean hasPermission = PrivacyManager.hasPermission(ActivityApp.this, mAppInfo.getPackageName(),
						rRestrictionName);
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
			private boolean allRestricted = true;
			private boolean someRestricted = false;

			public GroupHolderTask(int thePosition, GroupViewHolder theHolder, String theRestrictionName) {
				position = thePosition;
				holder = theHolder;
				restrictionName = theRestrictionName;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (holder.position == position && restrictionName != null) {
					// Get info
					used = (PrivacyManager.getUsed(holder.row.getContext(), mAppInfo.getUid(), restrictionName, null) != 0);
					permission = PrivacyManager.hasPermission(holder.row.getContext(), mAppInfo.getPackageName(),
							restrictionName);

					for (boolean restricted : PrivacyManager.getRestricted(holder.row.getContext(), mAppInfo.getUid(),
							restrictionName)) {
						allRestricted = (allRestricted && restricted);
						someRestricted = (someRestricted || restricted);
					}
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.position == position && restrictionName != null) {
					// Set data
					holder.tvName.setTypeface(null, used ? Typeface.BOLD_ITALIC : Typeface.NORMAL);
					holder.imgUsed.setVisibility(used ? View.VISIBLE : View.INVISIBLE);
					holder.imgGranted.setVisibility(permission ? View.VISIBLE : View.INVISIBLE);

					// Display restriction
					if (allRestricted)
						holder.imgCBName.setImageBitmap(mCheck[2]); // Full
					else if (someRestricted)
						holder.imgCBName.setImageBitmap(mCheck[1]); // Half
					else
						holder.imgCBName.setImageBitmap(mCheck[0]); // Off
					holder.imgCBName.setVisibility(View.VISIBLE);

					// Listen for restriction changes
					holder.rlName.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							// Get all/some restricted
							boolean allRestricted = true;
							boolean someRestricted = false;
							for (boolean restricted : PrivacyManager.getRestricted(view.getContext(),
									mAppInfo.getUid(), restrictionName)) {
								allRestricted = (allRestricted && restricted);
								someRestricted = (someRestricted || restricted);
							}
							boolean restart = PrivacyManager.setRestricted(null, view.getContext(), mAppInfo.getUid(),
									restrictionName, null, !someRestricted);

							// Update all/some restricted
							allRestricted = true;
							someRestricted = false;
							for (boolean restricted : PrivacyManager.getRestricted(holder.row.getContext(),
									mAppInfo.getUid(), restrictionName)) {
								allRestricted = (allRestricted && restricted);
								someRestricted = (someRestricted || restricted);
							}

							// Display restriction
							if (allRestricted)
								holder.imgCBName.setImageBitmap(mCheck[2]); // Full
							else if (someRestricted)
								holder.imgCBName.setImageBitmap(mCheck[1]); // Half
							else
								holder.imgCBName.setImageBitmap(mCheck[0]); // Off

							// Refresh display
							notifyDataSetChanged(); // Needed to update childs

							// Notify restart
							if (restart)
								Toast.makeText(view.getContext(), getString(R.string.msg_restart), Toast.LENGTH_SHORT)
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
			holder.imgIndicator.setImageResource(Util.getThemed(ActivityApp.this,
					isExpanded ? R.attr.icon_expander_maximized : R.attr.icon_expander_minimized));

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
					Intent infoIntent = new Intent(Intent.ACTION_VIEW);
					infoIntent.setData(Uri.parse(ActivityMain.cXUrl + "#" + restrictionName));
					startActivity(infoIntent);
				}
			});

			// Display localized name
			holder.tvName.setText(PrivacyManager.getLocalizedName(holder.row.getContext(), restrictionName));

			// Display restriction
			holder.imgCBName.setVisibility(View.INVISIBLE);

			// Async update
			new GroupHolderTask(groupPosition, holder, restrictionName).executeOnExecutor(mExecutor, (Object) null);

			return convertView;
		}

		private List<PrivacyManager.MethodDescription> getMethodDescriptions(int groupPosition) {
			if (!mMethodDescription.containsKey(groupPosition)) {
				boolean fUsed = PrivacyManager.getSettingBool(null, ActivityApp.this, 0, PrivacyManager.cSettingFUsed,
						false, false);
				boolean fPermission = PrivacyManager.getSettingBool(null, ActivityApp.this, 0,
						PrivacyManager.cSettingFPermission, false, false);
				List<PrivacyManager.MethodDescription> listMethod = new ArrayList<PrivacyManager.MethodDescription>();
				String restrictionName = mRestrictions.get(groupPosition);
				for (PrivacyManager.MethodDescription md : PrivacyManager.getMethods((String) getGroup(groupPosition))) {
					boolean isUsed = (PrivacyManager.getUsed(ActivityApp.this, mAppInfo.getUid(), restrictionName,
							md.getName()) > 0);
					boolean hasPermission = PrivacyManager.hasPermission(ActivityApp.this, mAppInfo.getPackageName(),
							md);
					if (mSelectedMethodName != null
							|| ((fUsed ? isUsed : true) && (fPermission ? isUsed || hasPermission : true)))
						listMethod.add(md);
				}
				mMethodDescription.put(groupPosition, listMethod);
			}
			return mMethodDescription.get(groupPosition);
		}

		@Override
		public Object getChild(int groupPosition, int childPosition) {
			return getMethodDescriptions(groupPosition).get(childPosition);
		}

		@Override
		public long getChildId(int groupPosition, int childPosition) {
			return childPosition;
		}

		@Override
		public int getChildrenCount(int groupPosition) {
			return getMethodDescriptions(groupPosition).size();
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
			public CheckedTextView ctvMethodName;

			private ChildViewHolder(View theRow, int gPosition, int cPosition) {
				row = theRow;
				groupPosition = gPosition;
				childPosition = cPosition;
				imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
				imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
				ctvMethodName = (CheckedTextView) row.findViewById(R.id.ctvMethodName);
			}
		}

		private class ChildHolderTask extends AsyncTask<Object, Object, Object> {
			private int groupPosition;
			private int childPosition;
			private ChildViewHolder holder;
			private String restrictionName;
			private PrivacyManager.MethodDescription md;
			private long lastUsage;
			private boolean parentRestricted;
			private boolean permission;
			private boolean restricted;

			public ChildHolderTask(int gPosition, int cPosition, ChildViewHolder theHolder, String theRestrictionName) {
				groupPosition = gPosition;
				childPosition = cPosition;
				holder = theHolder;
				restrictionName = theRestrictionName;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (holder.groupPosition == groupPosition && holder.childPosition == childPosition
						&& restrictionName != null) {
					// Get info
					md = (PrivacyManager.MethodDescription) getChild(groupPosition, childPosition);
					lastUsage = PrivacyManager.getUsed(holder.row.getContext(), mAppInfo.getUid(), restrictionName,
							md.getName());
					parentRestricted = PrivacyManager.getRestricted(null, holder.row.getContext(), mAppInfo.getUid(),
							restrictionName, null, false, false);
					permission = PrivacyManager.hasPermission(holder.row.getContext(), mAppInfo.getPackageName(), md);
					restricted = PrivacyManager.getRestricted(null, holder.row.getContext(), mAppInfo.getUid(),
							restrictionName, md.getName(), false, false);
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.groupPosition == groupPosition && holder.childPosition == childPosition
						&& restrictionName != null) {
					// Set data
					if (lastUsage > 0) {
						CharSequence sLastUsage = DateUtils.getRelativeTimeSpanString(lastUsage, new Date().getTime(),
								DateUtils.SECOND_IN_MILLIS, 0);
						holder.ctvMethodName.setText(String.format("%s (%s)", md.getName(), sLastUsage));
					}
					holder.ctvMethodName.setEnabled(parentRestricted);
					holder.imgUsed.setVisibility(lastUsage == 0 ? View.INVISIBLE : View.VISIBLE);
					holder.ctvMethodName.setTypeface(null, lastUsage == 0 ? Typeface.NORMAL : Typeface.BOLD_ITALIC);
					holder.imgGranted.setVisibility(permission ? View.VISIBLE : View.INVISIBLE);
					holder.ctvMethodName.setChecked(restricted);

					// Listen for restriction changes
					holder.ctvMethodName.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							boolean restricted = PrivacyManager.getRestricted(null, view.getContext(),
									mAppInfo.getUid(), restrictionName, md.getName(), false, false);
							restricted = !restricted;
							holder.ctvMethodName.setChecked(restricted);
							boolean restart = PrivacyManager.setRestricted(null, view.getContext(), mAppInfo.getUid(),
									restrictionName, md.getName(), restricted);

							// Refresh display
							notifyDataSetChanged(); // Needed to update parent

							// Notify restart
							if (restart)
								Toast.makeText(view.getContext(), getString(R.string.msg_restart), Toast.LENGTH_SHORT)
										.show();
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
			final PrivacyManager.MethodDescription md = (PrivacyManager.MethodDescription) getChild(groupPosition,
					childPosition);

			// Set background color
			if (md.isDangerous())
				holder.row.setBackgroundColor(getResources().getColor(
						Util.getThemed(ActivityApp.this, R.attr.color_dangerous)));
			else
				holder.row.setBackgroundColor(Color.TRANSPARENT);

			// Display method name
			holder.ctvMethodName.setText(md.getName());
			holder.ctvMethodName.setEnabled(false);
			holder.ctvMethodName.setTypeface(null, Typeface.NORMAL);

			// Display if used
			holder.imgUsed.setVisibility(View.INVISIBLE);

			// Display if permissions
			holder.imgGranted.setVisibility(View.INVISIBLE);

			// Display restriction
			holder.ctvMethodName.setChecked(false);
			holder.ctvMethodName.setClickable(false);

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

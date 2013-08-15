package biz.bokhorst.xprivacy;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
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
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.Notification;
import android.app.NotificationManager;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
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
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.TaskStackBuilder;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.BaseExpandableListAdapter;
import android.widget.CheckedTextView;
import android.widget.ExpandableListView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

public class ActivityApp extends Activity {

	private int mThemeId;
	private boolean mNotified;
	private ApplicationInfoEx mAppInfo;
	private RestrictionAdapter mPrivacyListAdapter = null;

	public static final String cNotified = "Notified";
	public static final String cPackageName = "PackageName";
	public static final String cRestrictionName = "RestrictionName";
	public static final String cMethodName = "MethodName";

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
		String themeName = PrivacyManager.getSetting(null, this, PrivacyManager.cSettingTheme, "", false);
		mThemeId = (themeName.equals("Dark") ? R.style.CustomTheme : R.style.CustomTheme_Light);
		setTheme(mThemeId);

		// Set layout
		setContentView(R.layout.restrictionlist);

		// Get arguments
		Bundle extras = getIntent().getExtras();
		mNotified = (extras.containsKey(cNotified) ? extras.getBoolean(cNotified) : false);
		String restrictionName = (extras.containsKey(cRestrictionName) ? extras.getString(cRestrictionName) : null);
		String methodName = (extras.containsKey(cMethodName) ? extras.getString(cMethodName) : null);

		// Get app info
		mAppInfo = new ApplicationInfoEx(this, extras.getString(cPackageName));
		if (!mAppInfo.getIsInstalled()) {
			finish();
			return;
		}

		// Handle info click
		ImageView imgInfo = (ImageView) findViewById(R.id.imgInfo);
		imgInfo.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				Intent infoIntent = new Intent(Intent.ACTION_VIEW);
				infoIntent.setData(Uri.parse(String.format(
						"http://updates.faircode.eu/xprivacy?application_name=%s&package_name=%s",
						mAppInfo.getFirstApplicationName(), mAppInfo.getPackageName())));
				startActivity(infoIntent);
			}
		});

		// Display app name
		TextView tvAppName = (TextView) findViewById(R.id.tvApp);
		tvAppName.setText(mAppInfo.toString());

		// Handle help
		ImageView ivHelp = (ImageView) findViewById(R.id.ivHelp);
		ivHelp.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				Dialog dialog = new Dialog(ActivityApp.this);
				dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
				dialog.setTitle(getString(R.string.help_application));
				dialog.setContentView(R.layout.help);
				dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, getThemed(R.attr.icon_launcher));
				dialog.setCancelable(true);
				dialog.show();
			}
		});

		// Background color
		if (mAppInfo.getIsSystem()) {
			LinearLayout llInfo = (LinearLayout) findViewById(R.id.llInfo);
			llInfo.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
		}

		// Display app icon
		ImageView imgIcon = (ImageView) findViewById(R.id.imgIcon);
		imgIcon.setImageDrawable(mAppInfo.getIcon());

		// Handle icon click
		imgIcon.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				Intent intentApp = getPackageManager().getLaunchIntentForPackage(mAppInfo.getPackageName());
				if (intentApp != null) {
					intentApp.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					view.getContext().startActivity(intentApp);
				}
			}
		});

		// Check if internet access
		if (!mAppInfo.hasInternet()) {
			ImageView imgInternet = (ImageView) findViewById(R.id.imgInternet);
			imgInternet.setVisibility(View.INVISIBLE);
		}

		// Check if frozen
		if (!mAppInfo.isFrozen()) {
			ImageView imgFrozen = (ImageView) findViewById(R.id.imgFrozen);
			imgFrozen.setVisibility(View.INVISIBLE);
		}

		// Display version
		TextView tvVersion = (TextView) findViewById(R.id.tvVersion);
		tvVersion.setText(mAppInfo.getVersion());

		// Display package name
		TextView tvPackageName = (TextView) findViewById(R.id.tvPackageName);
		tvPackageName.setText(mAppInfo.getPackageName());

		// Get applicable restrictions
		boolean fPermission = PrivacyManager
				.getSettingBool(null, this, PrivacyManager.cSettingFPermission, true, false);
		if (restrictionName != null && methodName != null)
			fPermission = false;
		List<String> listRestriction = new ArrayList<String>();
		for (String rRestrictionName : PrivacyManager.getRestrictions(true))
			if (fPermission ? PrivacyManager.hasPermission(this, mAppInfo.getPackageName(), rRestrictionName)
					|| PrivacyManager.getUsed(this, mAppInfo.getUid(), rRestrictionName, null) > 0 : true)
				listRestriction.add(rRestrictionName);

		// Fill privacy list view adapter
		final ExpandableListView lvRestriction = (ExpandableListView) findViewById(R.id.elvRestriction);
		lvRestriction.setGroupIndicator(null);
		mPrivacyListAdapter = new RestrictionAdapter(R.layout.restrictionentry, mAppInfo, listRestriction);
		lvRestriction.setAdapter(mPrivacyListAdapter);
		if (restrictionName != null && methodName != null) {
			int groupPosition = PrivacyManager.getRestrictions(true).indexOf(restrictionName);
			int childPosition = PrivacyManager.getMethods(restrictionName).indexOf(
					new PrivacyManager.MethodDescription(methodName));
			lvRestriction.expandGroup(groupPosition);
			lvRestriction.setSelectedChild(groupPosition, childPosition, true);
		}

		// Up navigation
		getActionBar().setDisplayHomeAsUpEnabled(true);
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mPrivacyListAdapter != null)
			mPrivacyListAdapter.notifyDataSetChanged();
	}

	@Override
	protected void onStop() {
		if (mNotified)
			finish();
		super.onStop();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.app, menu);

		// Launch
		PackageManager pm = getPackageManager();
		if (pm.getLaunchIntentForPackage(mAppInfo.getPackageName()) == null)
			menu.findItem(R.id.menu_app_launch).setEnabled(false);

		// Play
		boolean hasMarketLink = Util.hasMarketLink(this, mAppInfo.getPackageName());
		menu.findItem(R.id.menu_app_store).setEnabled(hasMarketLink);

		return true;
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		// Accounts
		boolean accountsRestricted = PrivacyManager.getRestricted(null, this, mAppInfo.getUid(),
				PrivacyManager.cAccounts, null, false, false);
		boolean contactsRestricted = PrivacyManager.getRestricted(null, this, mAppInfo.getUid(),
				PrivacyManager.cContacts, null, false, false);
		menu.findItem(R.id.menu_accounts).setEnabled(accountsRestricted);
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
		case R.id.menu_all:
			optionAll();
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
		case R.id.menu_contacts:
			optionContacts();
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	// Options

	private void optionAll() {
		List<String> listRestriction = PrivacyManager.getRestrictions(false);

		// Get toggle
		boolean restricted = false;
		for (String restrictionName : listRestriction)
			if (PrivacyManager.getSettingBool(null, this, String.format("Template.%s", restrictionName), true, false))
				if (PrivacyManager.getRestricted(null, this, mAppInfo.getUid(), restrictionName, null, false, false)) {
					restricted = true;
					break;
				}

		// Do toggle
		restricted = !restricted;
		for (String restrictionName : listRestriction)
			if (PrivacyManager.getSettingBool(null, this, String.format("Template.%s", restrictionName), true, false))
				PrivacyManager.setRestricted(null, this, mAppInfo.getUid(), restrictionName, null, restricted);

		// Refresh display
		if (mPrivacyListAdapter != null)
			mPrivacyListAdapter.notifyDataSetChanged();
	}

	private void optionClear() {
		PrivacyManager.deleteRestrictions(this, mAppInfo.getUid());
		if (mPrivacyListAdapter != null)
			mPrivacyListAdapter.notifyDataSetChanged();
	}

	private void optionUsage() {
		Intent intent = new Intent(this, ActivityUsage.class);
		intent.putExtra(ActivityUsage.cUid, mAppInfo.getUid());
		startActivity(intent);
	}

	private void optionSubmit() {
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(this);
		alertDialogBuilder.setTitle(getString(R.string.app_name));
		alertDialogBuilder.setMessage(getString(R.string.msg_sure));
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
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
		if (Util.getLicense() == null) {
			// Redirect to pro page
			Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse("http://www.faircode.eu/xprivacy/"));
			startActivity(browserIntent);
		} else {
			// Initiate fetch
			FetchTask fetchTask = new FetchTask();
			fetchTask.executeOnExecutor(mExecutor, mAppInfo);
		}
	}

	private void optionAccounts() {
		AccountsTask accountsTask = new AccountsTask();
		accountsTask.executeOnExecutor(mExecutor, (Object) null);
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

	private void optionContacts() {
		ContactsTask contactsTask = new ContactsTask();
		contactsTask.executeOnExecutor(mExecutor, (Object) null);
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
			AccountManager accountManager = AccountManager.get(getApplicationContext());
			mAccounts = accountManager.getAccounts();
			mSelection = new boolean[mAccounts.length];
			for (int i = 0; i < mAccounts.length; i++)
				try {
					mListAccount.add(String.format("%s (%s)", mAccounts[i].name, mAccounts[i].type));
					String sha1 = Util.sha1(mAccounts[i].name + mAccounts[i].type);
					mSelection[i] = PrivacyManager.getSettingBool(null, ActivityApp.this,
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
			alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
			alertDialogBuilder.setMultiChoiceItems(mListAccount.toArray(new CharSequence[0]), mSelection,
					new DialogInterface.OnMultiChoiceClickListener() {
						public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
							try {
								Account account = mAccounts[whichButton];
								String sha1 = Util.sha1(account.name + account.type);
								PrivacyManager.setSetting(null, ActivityApp.this,
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
				mSelection[i++] = PrivacyManager.getSettingBool(null, ActivityApp.this,
						String.format("Contact.%d.%d", mAppInfo.getUid(), id), false, false);
			}
			return null;
		}

		@Override
		protected void onPostExecute(Object result) {
			// Build dialog
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityApp.this);
			alertDialogBuilder.setTitle(getString(R.string.menu_contacts));
			alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
			alertDialogBuilder.setMultiChoiceItems(mListContact.toArray(new CharSequence[0]), mSelection,
					new DialogInterface.OnMultiChoiceClickListener() {
						public void onClick(DialogInterface dialog, int whichButton, boolean isChecked) {
							// Contact
							PrivacyManager.setSetting(null, ActivityApp.this,
									String.format("Contact.%d.%d", mAppInfo.getUid(), mIds[whichButton]),
									Boolean.toString(isChecked));

							// Raw contacts
							Cursor cursor = getContentResolver().query(ContactsContract.RawContacts.CONTENT_URI,
									new String[] { ContactsContract.RawContacts._ID },
									ContactsContract.RawContacts.CONTACT_ID + "=?",
									new String[] { String.valueOf(mIds[whichButton]) }, null);
							try {
								while (cursor.moveToNext()) {
									PrivacyManager.setSetting(null, ActivityApp.this,
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

	private class SubmitTask extends AsyncTask<ApplicationInfoEx, Object, Object> {
		private final static int NOTIFY_ID = 5;

		@Override
		protected void onPreExecute() {
			notify(null);
		}

		@Override
		protected Object doInBackground(ApplicationInfoEx... params) {
			try {
				// Encode restrictions
				int uid = params[0].getUid();
				JSONArray jSettings = new JSONArray();
				for (String restrictionName : PrivacyManager.getRestrictions(true)) {
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
										md.getMethodName(), false, false);
						long mUsed = PrivacyManager.getUsed(ActivityApp.this, uid, restrictionName, md.getMethodName());
						JSONObject jMethod = new JSONObject();
						jMethod.put("restriction", restrictionName);
						jMethod.put("method", md.getMethodName());
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
				jRoot.put("protocol_version", 3);
				jRoot.put("android_id", android_id);
				jRoot.put("android_sdk", Build.VERSION.SDK_INT);
				jRoot.put("xprivacy_version", pInfo.versionCode);
				jRoot.put("application_name", params[0].getFirstApplicationName());
				jRoot.put("package_name", params[0].getPackageName());
				jRoot.put("package_version", params[0].getVersion());
				jRoot.put("settings", jSettings);

				// Submit
				int TIMEOUT_MILLISEC = 45000; // 45 seconds
				HttpParams httpParams = new BasicHttpParams();
				HttpConnectionParams.setConnectionTimeout(httpParams, TIMEOUT_MILLISEC);
				HttpConnectionParams.setSoTimeout(httpParams, TIMEOUT_MILLISEC);
				HttpClient httpclient = new DefaultHttpClient(httpParams);

				HttpPost httpost = new HttpPost("http://updates.faircode.eu/xprivacy?format=json&action=submit");
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
					return new JSONObject(out.toString("UTF-8"));
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
		protected void onPostExecute(Object result) {
			notify(result);
			super.onPostExecute(result);
		}

		private void notify(Object result) {
			NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(ActivityApp.this);
			notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
			notificationBuilder.setContentTitle(getString(R.string.menu_submit));
			if (result == null) {
				notificationBuilder.setContentText(mAppInfo.getFirstApplicationName());
			} else if (result.getClass().equals(JSONObject.class)) {
				JSONObject status = (JSONObject) result;
				try {
					if (status.getBoolean("ok"))
						notificationBuilder.setContentText(String.format("%s: %s", mAppInfo.getFirstApplicationName(),
								getString(R.string.msg_done)));
					else
						notificationBuilder.setContentText(String.format("%s: %s", mAppInfo.getFirstApplicationName(),
								status.getString("error")));
				} catch (Throwable ex) {
					notificationBuilder.setContentText(String.format("%s: %s", mAppInfo.getFirstApplicationName(), ex));
				}
			} else
				notificationBuilder.setContentText(result.toString());
			notificationBuilder.setWhen(System.currentTimeMillis());
			notificationBuilder.setAutoCancel(true);
			Notification notification = notificationBuilder.build();
			NotificationManager notificationManager = (NotificationManager) ActivityApp.this
					.getSystemService(Context.NOTIFICATION_SERVICE);
			notificationManager.notify(NOTIFY_ID, notification);
		}
	}

	private class FetchTask extends AsyncTask<ApplicationInfoEx, Object, Object> {
		private ApplicationInfoEx mAppInfo;

		@Override
		protected Object doInBackground(ApplicationInfoEx... params) {
			try {
				// Get data
				mAppInfo = params[0];
				PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
				String android_id = Secure.getString(ActivityApp.this.getContentResolver(), Secure.ANDROID_ID);
				String[] license = Util.getLicense();

				// Encode package
				JSONObject jRoot = new JSONObject();
				jRoot.put("protocol_version", 3);
				jRoot.put("android_id", android_id);
				jRoot.put("android_sdk", Build.VERSION.SDK_INT);
				jRoot.put("xprivacy_version", pInfo.versionCode);
				jRoot.put("application_name", mAppInfo.getFirstApplicationName());
				jRoot.put("package_name", mAppInfo.getPackageName());
				jRoot.put("package_version", mAppInfo.getVersion());
				jRoot.put("email", license[1]);
				jRoot.put("signature", license[2]);

				// Fetch
				int TIMEOUT_MILLISEC = 45000; // 45 seconds
				HttpParams httpParams = new BasicHttpParams();
				HttpConnectionParams.setConnectionTimeout(httpParams, TIMEOUT_MILLISEC);
				HttpConnectionParams.setSoTimeout(httpParams, TIMEOUT_MILLISEC);
				HttpClient httpclient = new DefaultHttpClient(httpParams);

				HttpPost httpost = new HttpPost("http://updates.faircode.eu/xprivacy.php?format=json&action=fetch");
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
					return new JSONObject(out.toString("UTF-8"));
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
		protected void onPostExecute(Object result) {
			try {
				if (result.getClass().equals(JSONObject.class)) {
					JSONObject status = (JSONObject) result;
					if (status.getBoolean("ok")) {
						// Delete existing restrictions
						PrivacyManager.deleteRestrictions(ActivityApp.this, mAppInfo.getUid());

						// Set fetched restrictions
						JSONArray settings = status.getJSONArray("settings");
						for (int i = 0; i < settings.length(); i++) {
							JSONObject entry = settings.getJSONObject(i);
							String restrictionName = entry.getString("restriction");
							String methodName = entry.has("method") ? entry.getString("method") : null;
							int voted_restricted = entry.getInt("restricted");
							int voted_not_restricted = entry.getInt("not_restricted");
							boolean restricted = (voted_restricted > voted_not_restricted);
							if (methodName == null || restricted)
								PrivacyManager.setRestricted(null, ActivityApp.this, mAppInfo.getUid(),
										restrictionName, methodName, restricted);
							if (mPrivacyListAdapter != null)
								mPrivacyListAdapter.notifyDataSetChanged();
						}
					} else
						throw new Exception(status.getString("error"));
				} else
					throw (Throwable) result;
			} catch (Throwable ex) {
				Util.bug(null, ex);
				Toast toast = Toast.makeText(ActivityApp.this, ex.toString(), Toast.LENGTH_LONG);
				toast.show();
			}
			super.onPostExecute(result);
		}
	}

	// Adapters

	private class RestrictionAdapter extends BaseExpandableListAdapter {
		private ApplicationInfoEx mAppInfo;
		private List<String> mRestrictions;
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

		public RestrictionAdapter(int resource, ApplicationInfoEx appInfo, List<String> restrictions) {
			mAppInfo = appInfo;
			mRestrictions = restrictions;
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
			public CheckedTextView ctvRestriction;

			public GroupViewHolder(View theRow, int thePosition) {
				row = theRow;
				position = thePosition;
				imgIndicator = (ImageView) row.findViewById(R.id.imgIndicator);
				imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
				imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
				imgInfo = (ImageView) row.findViewById(R.id.imgInfo);
				ctvRestriction = (CheckedTextView) row.findViewById(R.id.ctvName);
			}
		}

		private class GroupHolderTask extends AsyncTask<Object, Object, Object> {
			private int position;
			private GroupViewHolder holder;
			private String restrictionName;
			private boolean used;
			private boolean permission;
			private boolean restricted;

			public GroupHolderTask(int thePosition, GroupViewHolder theHolder, String theRestrictionName) {
				position = thePosition;
				holder = theHolder;
				restrictionName = theRestrictionName;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (holder.position == position) {
					// Get info
					used = (PrivacyManager.getUsed(holder.row.getContext(), mAppInfo.getUid(), restrictionName, null) != 0);
					permission = PrivacyManager.hasPermission(holder.row.getContext(), mAppInfo.getPackageName(),
							restrictionName);
					restricted = PrivacyManager.getRestricted(null, holder.row.getContext(), mAppInfo.getUid(),
							restrictionName, null, false, false);
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.position == position && restrictionName != null) {
					// Set data
					holder.ctvRestriction.setTypeface(null, used ? Typeface.BOLD_ITALIC : Typeface.NORMAL);
					holder.imgUsed.setVisibility(used ? View.VISIBLE : View.INVISIBLE);
					holder.imgGranted.setVisibility(permission ? View.VISIBLE : View.INVISIBLE);
					holder.ctvRestriction.setChecked(restricted);

					// Listen for restriction changes
					holder.ctvRestriction.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							boolean restricted = PrivacyManager.getRestricted(null, view.getContext(),
									mAppInfo.getUid(), restrictionName, null, false, false);
							restricted = !restricted;
							holder.ctvRestriction.setChecked(restricted);
							PrivacyManager.setRestricted(null, view.getContext(), mAppInfo.getUid(), restrictionName,
									null, restricted);
							notifyDataSetChanged(); // Needed to update childs
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

			// Set background color
			if (PrivacyManager.isDangerousRestriction(restrictionName))
				holder.row.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
			else
				holder.row.setBackgroundColor(Color.TRANSPARENT);

			// Indicator state
			holder.imgIndicator.setImageResource(getThemed(isExpanded ? R.attr.icon_expander_maximized
					: R.attr.icon_expander_minimized));

			// Disable indicator for empty groups
			if (getChildrenCount(groupPosition) == 0)
				holder.imgIndicator.setVisibility(View.INVISIBLE);
			else
				holder.imgIndicator.setVisibility(View.VISIBLE);

			// Display if used
			holder.ctvRestriction.setTypeface(null, Typeface.NORMAL);
			holder.imgUsed.setVisibility(View.INVISIBLE);

			// Check if permission
			holder.imgGranted.setVisibility(View.INVISIBLE);

			// Handle info
			holder.imgInfo.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					Intent infoIntent = new Intent(Intent.ACTION_VIEW);
					infoIntent.setData(Uri.parse(String.format("http://wiki.faircode.eu/index.php?title=%s",
							restrictionName)));
					startActivity(infoIntent);
				}
			});

			// Display localized name
			holder.ctvRestriction.setText(PrivacyManager.getLocalizedName(holder.row.getContext(), restrictionName));

			// Display restriction
			holder.ctvRestriction.setChecked(false);
			holder.ctvRestriction.setClickable(false);

			// Async update
			new GroupHolderTask(groupPosition, holder, restrictionName).executeOnExecutor(mExecutor, (Object) null);

			return convertView;
		}

		@Override
		public Object getChild(int groupPosition, int childPosition) {
			return PrivacyManager.getMethods((String) getGroup(groupPosition)).get(childPosition);
		}

		@Override
		public long getChildId(int groupPosition, int childPosition) {
			return childPosition;
		}

		@Override
		public int getChildrenCount(int groupPosition) {
			return PrivacyManager.getMethods((String) getGroup(groupPosition)).size();
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
				if (holder.groupPosition == groupPosition && holder.childPosition == childPosition) {
					// Get info
					md = (PrivacyManager.MethodDescription) getChild(groupPosition, childPosition);
					lastUsage = PrivacyManager.getUsed(holder.row.getContext(), mAppInfo.getUid(), restrictionName,
							md.getMethodName());
					parentRestricted = PrivacyManager.getRestricted(null, holder.row.getContext(), mAppInfo.getUid(),
							restrictionName, null, false, false);
					permission = PrivacyManager.hasPermission(holder.row.getContext(), mAppInfo.getPackageName(), md);
					restricted = PrivacyManager.getRestricted(null, holder.row.getContext(), mAppInfo.getUid(),
							restrictionName, md.getMethodName(), false, false);
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.groupPosition == groupPosition && holder.childPosition == childPosition
						&& restrictionName != null && md != null) {
					// Set data
					if (lastUsage > 0) {
						Date date = new Date(lastUsage);
						SimpleDateFormat format = new SimpleDateFormat("dd/HH:mm", Locale.ROOT);
						holder.ctvMethodName.setText(String.format("%s %s", md.getMethodName(), format.format(date)));
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
									mAppInfo.getUid(), restrictionName, md.getMethodName(), false, false);
							restricted = !restricted;
							holder.ctvMethodName.setChecked(restricted);
							PrivacyManager.setRestricted(null, view.getContext(), mAppInfo.getUid(), restrictionName,
									md.getMethodName(), restricted);
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
			if (PrivacyManager.isDangerousMethod(restrictionName, md.getMethodName()))
				holder.row.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
			else
				holder.row.setBackgroundColor(Color.TRANSPARENT);

			// Display method name
			holder.ctvMethodName.setText(md.getMethodName());
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

	// Helper methods

	private int getThemed(int attr) {
		TypedValue typedvalueattr = new TypedValue();
		getTheme().resolveAttribute(attr, typedvalueattr, true);
		return typedvalueattr.resourceId;
	}
}

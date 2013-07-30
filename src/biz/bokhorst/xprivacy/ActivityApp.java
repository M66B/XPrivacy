package biz.bokhorst.xprivacy;

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

import android.accounts.Account;
import android.accounts.AccountManager;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Color;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Process;
import android.provider.ContactsContract;
import android.provider.ContactsContract.CommonDataKinds.Phone;
import android.support.v4.app.NavUtils;
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

	public static final String cNotified = "notified";
	public static final String cPackageName = "PackageName";

	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors(),
			new PriorityThreadFactor());

	private static class PriorityThreadFactor implements ThreadFactory {
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
				infoIntent.setData(Uri.parse(String.format("http://wiki.faircode.eu/index.php?title=%s",
						mAppInfo.toString())));
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
		List<String> listRestriction = new ArrayList<String>();
		for (String restrictionName : PrivacyManager.getRestrictions(true))
			if (fPermission ? PrivacyManager.hasPermission(this, mAppInfo.getPackageName(), restrictionName)
					|| PrivacyManager.getUsed(this, mAppInfo.getUid(), restrictionName, null) > 0 : true)
				listRestriction.add(restrictionName);

		// Fill privacy list view adapter
		final ExpandableListView lvRestriction = (ExpandableListView) findViewById(R.id.elvRestriction);
		lvRestriction.setGroupIndicator(null);
		mPrivacyListAdapter = new RestrictionAdapter(R.layout.restrictionentry, mAppInfo, listRestriction);
		lvRestriction.setAdapter(mPrivacyListAdapter);

		// Up navigation
		getActionBar().setDisplayHomeAsUpEnabled(true);
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
		case R.id.menu_usage:
			optionUsage();
			return true;
		case R.id.menu_app_launch:
			Intent LaunchIntent = getPackageManager().getLaunchIntentForPackage(mAppInfo.getPackageName());
			startActivity(LaunchIntent);
			return true;
		case R.id.menu_app_settings:
			startActivity(new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS,
					Uri.parse("package:" + mAppInfo.getPackageName())));
			return true;
		case R.id.menu_app_store:
			startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse("market://details?id=" + mAppInfo.getPackageName())));
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

	private void optionUsage() {
		Intent intent = new Intent(this, ActivityUsage.class);
		intent.putExtra(ActivityUsage.cUid, mAppInfo.getUid());
		startActivity(intent);
	}

	private void optionAccounts() {
		AccountsTask accountsTask = new AccountsTask();
		accountsTask.executeOnExecutor(mExecutor, (Object) null);
	}

	private void optionContacts() {
		ContactsTask contactsTask = new ContactsTask();
		contactsTask.executeOnExecutor(mExecutor, (Object) null);
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mPrivacyListAdapter != null)
			mPrivacyListAdapter.notifyDataSetChanged();
	}

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

	private class AccountsTask extends AsyncTask<Object, Object, Object> {
		private List<CharSequence> mListAccount;
		private Account[] mAccounts;
		private boolean[] mSelection;

		@Override
		protected Object doInBackground(Object... params) {
			// Elevate priority
			Process.setThreadPriority(Process.THREAD_PRIORITY_BACKGROUND + Process.THREAD_PRIORITY_MORE_FAVORABLE);

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
			// Elevate priority
			Process.setThreadPriority(Process.THREAD_PRIORITY_BACKGROUND + Process.THREAD_PRIORITY_MORE_FAVORABLE);

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

	public int getThemed(int attr) {
		TypedValue typedvalueattr = new TypedValue();
		getTheme().resolveAttribute(attr, typedvalueattr, true);
		return typedvalueattr.resourceId;
	}
}

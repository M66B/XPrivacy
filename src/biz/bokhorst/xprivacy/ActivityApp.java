package biz.bokhorst.xprivacy;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import android.app.Activity;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;
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

public class ActivityApp extends Activity {

	private int mThemeId;
	private ApplicationInfoEx mAppInfo;
	private RestrictionAdapter mPrivacyListAdapter = null;

	public static final String cPackageName = "PackageName";

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set theme
		mThemeId = Integer.parseInt(PrivacyManager.getSetting(null, this, PrivacyManager.cSettingTheme,
				Integer.toString(R.style.CustomTheme_Light), false));
		if (mThemeId != R.style.CustomTheme_Light && mThemeId != R.style.CustomTheme)
			mThemeId = R.style.CustomTheme_Light;
		setTheme(mThemeId);

		// Set layout
		setContentView(R.layout.restrictionlist);

		// Get package name
		Bundle extras = getIntent().getExtras();
		final String packageName = extras.getString(cPackageName);

		// Get app info
		mAppInfo = new ApplicationInfoEx(this, packageName);
		if (!mAppInfo.getIsInstalled()) {
			finish();
			return;
		}

		// Background color
		if (mAppInfo.getIsSystem()) {
			LinearLayout llInfo = (LinearLayout) findViewById(R.id.llInfo);
			llInfo.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_system)));
		}

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

		// Display app icon
		ImageView imgIcon = (ImageView) findViewById(R.id.imgAppEntryIcon);
		imgIcon.setImageDrawable(mAppInfo.getDrawable());

		// Handle icon click
		imgIcon.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				Intent intentApp = getPackageManager().getLaunchIntentForPackage(packageName);
				if (intentApp != null) {
					intentApp.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					view.getContext().startActivity(intentApp);
				}
			}
		});

		// Check if internet access
		ImageView imgInternet = (ImageView) findViewById(R.id.imgAppEntryInternet);
		if (!PrivacyManager.hasInternet(this, packageName))
			imgInternet.setVisibility(View.INVISIBLE);

		// Display version
		TextView tvVersion = (TextView) findViewById(R.id.tvVersion);
		tvVersion.setText(mAppInfo.getVersion());

		// Display package name / uid
		TextView tvPackageName = (TextView) findViewById(R.id.tvPackageName);
		tvPackageName.setText(String.format("%s %d", packageName, mAppInfo.getUid()));

		// Fill privacy list view adapter
		final ExpandableListView lvRestriction = (ExpandableListView) findViewById(R.id.elvRestriction);
		lvRestriction.setGroupIndicator(null);
		mPrivacyListAdapter = new RestrictionAdapter(R.layout.restrictionentry, mAppInfo,
				PrivacyManager.getRestrictions());
		lvRestriction.setAdapter(mPrivacyListAdapter);

		// Up navigation
		// getActionBar().setDisplayHomeAsUpEnabled(true);
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
			List<String> listRestriction = PrivacyManager.getRestrictions();

			// Get toggle
			boolean restricted = false;
			for (String restrictionName : listRestriction)
				if (PrivacyManager.getRestricted(null, this, mAppInfo.getUid(), restrictionName, null, false, false)) {
					restricted = true;
					break;
				}

			// Do toggle
			restricted = !restricted;
			for (String restrictionName : listRestriction)
				PrivacyManager.setRestricted(null, this, mAppInfo.getUid(), restrictionName, null, restricted);

			// Refresh display
			if (mPrivacyListAdapter != null)
				mPrivacyListAdapter.notifyDataSetChanged();

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
		default:
			return super.onOptionsItemSelected(item);
		}
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
		private boolean mExpert;

		public RestrictionAdapter(int resource, ApplicationInfoEx appInfo, List<String> restrictions) {
			mAppInfo = appInfo;
			mRestrictions = restrictions;
			mExpert = Boolean.parseBoolean(PrivacyManager.getSetting(null, null, PrivacyManager.cSettingExpert,
					Boolean.FALSE.toString(), false));
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

		@Override
		public View getGroupView(int groupPosition, boolean isExpanded, View convertView, ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.restrictionentry, parent, false);
			ImageView imgIndicator = (ImageView) row.findViewById(R.id.imgIndicator);
			ImageView imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
			ImageView imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
			ImageView imgInfo = (ImageView) row.findViewById(R.id.imgInfo);
			final CheckedTextView ctvRestriction = (CheckedTextView) row.findViewById(R.id.ctvName);

			// Indicator state
			imgIndicator.setImageResource(getThemed(isExpanded ? R.attr.icon_expander_maximized
					: R.attr.icon_expander_minimized));

			// Disable indicator for empty groups
			if (mExpert) {
				if (getChildrenCount(groupPosition) == 0)
					imgIndicator.setVisibility(View.INVISIBLE);
			} else
				imgIndicator.setVisibility(View.GONE);

			// Get entry
			final String restrictionName = (String) getGroup(groupPosition);

			// Display if restriction granted
			if (!PrivacyManager.hasPermission(row.getContext(), mAppInfo.getPackageName(), restrictionName))
				imgGranted.setVisibility(View.INVISIBLE);

			// Display if used
			if (PrivacyManager.getUsed(row.getContext(), mAppInfo.getUid(), restrictionName, null) != 0)
				ctvRestriction.setTypeface(null, Typeface.BOLD_ITALIC);
			else
				imgUsed.setVisibility(View.INVISIBLE);

			// Handle info
			imgInfo.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					Intent infoIntent = new Intent(Intent.ACTION_VIEW);
					infoIntent.setData(Uri.parse(String.format("http://wiki.faircode.eu/index.php?title=%s",
							restrictionName)));
					startActivity(infoIntent);
				}
			});

			// Display localized name
			ctvRestriction.setText(PrivacyManager.getLocalizedName(row.getContext(), restrictionName));

			// Display restriction
			boolean restricted = PrivacyManager.getRestricted(null, row.getContext(), mAppInfo.getUid(),
					restrictionName, null, false, false);
			ctvRestriction.setChecked(restricted);

			// Listen for restriction changes
			ctvRestriction.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					boolean restricted = PrivacyManager.getRestricted(null, view.getContext(), mAppInfo.getUid(),
							restrictionName, null, false, false);
					restricted = !restricted;
					ctvRestriction.setChecked(restricted);
					PrivacyManager.setRestricted(null, view.getContext(), mAppInfo.getUid(), restrictionName, null,
							restricted);
					notifyDataSetChanged();
				}
			});

			return row;
		}

		@Override
		public Object getChild(int groupPosition, int childPosition) {
			return PrivacyManager.getMethodNames((String) getGroup(groupPosition)).get(childPosition);
		}

		@Override
		public long getChildId(int groupPosition, int childPosition) {
			return childPosition;
		}

		@Override
		public int getChildrenCount(int groupPosition) {
			return (mExpert ? PrivacyManager.getMethodNames((String) getGroup(groupPosition)).size() : 0);
		}

		@Override
		public boolean isChildSelectable(int groupPosition, int childPosition) {
			return false;
		}

		@Override
		public View getChildView(int groupPosition, int childPosition, boolean isLastChild, View convertView,
				ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.restrictionchild, parent, false);
			final CheckedTextView ctvMethodName = (CheckedTextView) row.findViewById(R.id.ctvMethodName);

			// Get entry
			final String restrictionName = (String) getGroup(groupPosition);
			final String methodName = (String) getChild(groupPosition, childPosition);
			long lastUsage = PrivacyManager.getUsed(row.getContext(), mAppInfo.getUid(), restrictionName, methodName);

			// Display method name
			if (lastUsage == 0)
				ctvMethodName.setText(methodName);
			else {
				Date date = new Date(lastUsage);
				SimpleDateFormat format = new SimpleDateFormat("dd/HH:mm", Locale.US);
				ctvMethodName.setText(String.format("%s %s", methodName, format.format(date)));
			}

			boolean parentRestricted = PrivacyManager.getRestricted(null, row.getContext(), mAppInfo.getUid(),
					restrictionName, null, false, false);
			ctvMethodName.setEnabled(parentRestricted);

			// Display if used
			if (lastUsage != 0)
				ctvMethodName.setTypeface(null, Typeface.BOLD_ITALIC);

			// Display restriction
			boolean restricted = PrivacyManager.getRestricted(null, row.getContext(), mAppInfo.getUid(),
					restrictionName, methodName, false, false);
			ctvMethodName.setChecked(restricted);

			// Listen for restriction changes
			ctvMethodName.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					boolean restricted = PrivacyManager.getRestricted(null, view.getContext(), mAppInfo.getUid(),
							restrictionName, methodName, false, false);
					restricted = !restricted;
					ctvMethodName.setChecked(restricted);
					PrivacyManager.setRestricted(null, view.getContext(), mAppInfo.getUid(), restrictionName,
							methodName, restricted);
				}
			});

			return row;
		}

		@Override
		public boolean hasStableIds() {
			return true;
		}
	}

	public int getThemed(int attr) {
		TypedValue typedvalueattr = new TypedValue();
		getTheme().resolveAttribute(attr, typedvalueattr, true);
		return typedvalueattr.resourceId;
	}
}

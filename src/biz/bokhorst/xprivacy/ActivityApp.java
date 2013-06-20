package biz.bokhorst.xprivacy;

import java.util.List;

import android.app.Activity;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.BaseExpandableListAdapter;
import android.widget.CheckedTextView;
import android.widget.ExpandableListView;
import android.widget.ImageView;
import android.widget.TextView;

public class ActivityApp extends Activity {

	public static final String cPackageName = "PackageName";

	private RestrictionAdapter mPrivacyListAdapter = null;

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xrestrictionlist);

		// Get package name
		Bundle extras = getIntent().getExtras();
		final String packageName = extras.getString(cPackageName);

		// Get app info
		XApplicationInfo xAppInfo = new XApplicationInfo(packageName, this);

		// Display app name
		TextView tvAppName = (TextView) findViewById(R.id.tvApp);
		tvAppName.setText(xAppInfo.toString());

		// Display version
		TextView tvVersion = (TextView) findViewById(R.id.tvVersion);
		tvVersion.setText(xAppInfo.getVersion());

		// Display package name / uid
		TextView tvPackageName = (TextView) findViewById(R.id.tvPackageName);
		tvPackageName.setText(String.format("%s %d", packageName, xAppInfo.getUid()));

		// Handle help
		tvAppName.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				Dialog dialog = new Dialog(ActivityApp.this);
				dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
				dialog.setTitle(getString(R.string.help_application));
				dialog.setContentView(R.layout.xhelp);
				dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, R.drawable.ic_launcher);
				dialog.setCancelable(true);
				dialog.show();
			}
		});

		// Display app icon
		ImageView imgIcon = (ImageView) findViewById(R.id.imgAppEntryIcon);
		imgIcon.setImageDrawable(xAppInfo.getDrawable());

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
		if (!XRestriction.hasInternet(this, packageName))
			imgInternet.setVisibility(View.INVISIBLE);

		// Fill privacy list view adapter
		final ExpandableListView lvRestriction = (ExpandableListView) findViewById(R.id.elvRestriction);
		lvRestriction.setGroupIndicator(null);
		mPrivacyListAdapter = new RestrictionAdapter(this, R.layout.xrestrictionentry, xAppInfo,
				XRestriction.getRestrictions(this));
		lvRestriction.setAdapter(mPrivacyListAdapter);
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mPrivacyListAdapter != null)
			mPrivacyListAdapter.notifyDataSetChanged();
	}

	private class RestrictionAdapter extends BaseExpandableListAdapter {
		private Context mContext;
		private XApplicationInfo mAppInfo;
		private List<String> mRestrictions;
		private boolean mExpert;

		public RestrictionAdapter(Context context, int resource, XApplicationInfo appInfo, List<String> restrictions) {
			mAppInfo = appInfo;
			mRestrictions = restrictions;
			mExpert = Boolean.parseBoolean(XRestriction.getSetting(null, context, XRestriction.cSettingExpert,
					Boolean.FALSE.toString()));
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
			View row = inflater.inflate(R.layout.xrestrictionentry, parent, false);
			ImageView imgIndicator = (ImageView) row.findViewById(R.id.imgIndicator);
			ImageView imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
			ImageView imgGranted = (ImageView) row.findViewById(R.id.imgGranted);
			final CheckedTextView ctvRestriction = (CheckedTextView) row.findViewById(R.id.ctvName);

			// Indicator state
			imgIndicator.setImageResource(isExpanded ? android.R.drawable.arrow_up_float
					: android.R.drawable.arrow_down_float);

			// Disable indicator for empty groups
			if (mExpert) {
				if (getChildrenCount(groupPosition) == 0)
					imgIndicator.setVisibility(View.INVISIBLE);
			} else
				imgIndicator.setVisibility(View.GONE);

			// Get entry
			final String restrictionName = (String) getGroup(groupPosition);

			// Display localized name
			ctvRestriction.setText(XRestriction.getLocalizedName(row.getContext(), restrictionName));

			// Display if restriction granted
			if (!XRestriction.hasPermission(row.getContext(), mAppInfo.getPackageName(), restrictionName))
				imgGranted.setVisibility(View.INVISIBLE);

			// Display if used
			if (XRestriction.isUsed(row.getContext(), mAppInfo.getUid(), restrictionName, null))
				ctvRestriction.setTypeface(null, Typeface.BOLD_ITALIC);
			else
				imgUsed.setVisibility(View.INVISIBLE);

			// Display restriction
			boolean restricted = XRestriction.getRestricted(null, row.getContext(), mAppInfo.getUid(), restrictionName,
					null, false, false);
			ctvRestriction.setChecked(restricted);

			// Listen for restriction changes
			ctvRestriction.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					boolean restricted = XRestriction.getRestricted(null, view.getContext(), mAppInfo.getUid(),
							restrictionName, null, false, false);
					restricted = !restricted;
					ctvRestriction.setChecked(restricted);
					XRestriction.setRestricted(null, view.getContext(), mAppInfo.getUid(), restrictionName, null,
							restricted);
					notifyDataSetChanged();
				}
			});

			return row;
		}

		@Override
		public Object getChild(int groupPosition, int childPosition) {
			return XRestriction.getMethodNames(mContext, (String) getGroup(groupPosition)).get(childPosition);
		}

		@Override
		public long getChildId(int groupPosition, int childPosition) {
			return childPosition;
		}

		@Override
		public int getChildrenCount(int groupPosition) {
			return (mExpert ? XRestriction.getMethodNames(mContext, (String) getGroup(groupPosition)).size() : 0);
		}

		@Override
		public boolean isChildSelectable(int groupPosition, int childPosition) {
			return false;
		}

		@Override
		public View getChildView(int groupPosition, int childPosition, boolean isLastChild, View convertView,
				ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.xrestrictionchild, parent, false);
			final CheckedTextView ctvMethodName = (CheckedTextView) row.findViewById(R.id.ctvMethodName);

			// Get entry
			final String restrictionName = (String) getGroup(groupPosition);
			final String methodName = (String) getChild(groupPosition, childPosition);

			// Display method name
			ctvMethodName.setText(methodName);

			boolean parentRestricted = XRestriction.getRestricted(null, row.getContext(), mAppInfo.getUid(),
					restrictionName, null, false, false);
			ctvMethodName.setEnabled(parentRestricted);

			// Display if used
			if (XRestriction.isUsed(row.getContext(), mAppInfo.getUid(), restrictionName, methodName))
				ctvMethodName.setTypeface(null, Typeface.BOLD_ITALIC);

			// Display restriction
			boolean restricted = XRestriction.getRestricted(null, row.getContext(), mAppInfo.getUid(), restrictionName,
					methodName, false, false);
			ctvMethodName.setChecked(restricted);

			// Listen for restriction changes
			ctvMethodName.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					boolean restricted = XRestriction.getRestricted(null, view.getContext(), mAppInfo.getUid(),
							restrictionName, methodName, false, false);
					restricted = !restricted;
					ctvMethodName.setChecked(restricted);
					XRestriction.setRestricted(null, view.getContext(), mAppInfo.getUid(), restrictionName, methodName,
							restricted);
				}
			});

			return row;
		}

		@Override
		public boolean hasStableIds() {
			return true;
		}
	}
}

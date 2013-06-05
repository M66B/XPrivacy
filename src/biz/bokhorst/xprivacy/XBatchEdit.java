package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.R;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.SparseArray;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.CheckedTextView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

public class XBatchEdit extends Activity {

	public static final String cRestrictionName = "Restriction";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xbatchedit);

		// Get restriction name
		Bundle extras = getIntent().getExtras();
		final String restrictionName = extras.getString(cRestrictionName);

		// Display restriction name
		TextView tvRestriction = (TextView) findViewById(R.id.tvRestriction);
		tvRestriction.setText(XRestriction.getLocalizedName(this, restrictionName));

		// Legend
		TextView tvUsed = (TextView) findViewById(R.id.tvUsed);
		tvUsed.setTypeface(null, Typeface.BOLD_ITALIC);
		TextView tvInternet = (TextView) findViewById(R.id.tvInternet);
		tvInternet.setTextColor(Color.GRAY);

		// Get app list
		SparseArray<XApplicationInfo> mapApp = new SparseArray<XApplicationInfo>();
		List<XApplicationInfo> listApp = new ArrayList<XApplicationInfo>();
		for (ApplicationInfo appInfo : getPackageManager().getInstalledApplications(PackageManager.GET_META_DATA)) {
			XApplicationInfo xAppInfo = mapApp.get(appInfo.uid);
			if (xAppInfo == null) {
				xAppInfo = new XApplicationInfo(appInfo, restrictionName, getPackageManager());
				mapApp.put(appInfo.uid, xAppInfo);
				listApp.add(xAppInfo);
			} else
				xAppInfo.AddApplicationName((String) getPackageManager().getApplicationLabel(appInfo));
		}
		Collections.sort(listApp);

		// Fill app list view adapter
		final ListView lvApp = (ListView) findViewById(R.id.lvApp);
		AppListAdapter appAdapter = new AppListAdapter(this, R.layout.xappentry, listApp, restrictionName);
		lvApp.setAdapter(appAdapter);

		// Set restriction values
		for (int position = 0; position < lvApp.getAdapter().getCount(); position++) {
			XApplicationInfo appInfo = (XApplicationInfo) lvApp.getItemAtPosition(position);
			lvApp.setItemChecked(position,
					XRestriction.getRestricted(null, this, appInfo.getUid(), restrictionName, null, false));
		}
	}

	private class AppListAdapter extends ArrayAdapter<XApplicationInfo> {

		private String mRestrictionName;

		public AppListAdapter(Context context, int resource, List<XApplicationInfo> objects, String restrictionName) {
			super(context, resource, objects);
			mRestrictionName = restrictionName;
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.xappentry, parent, false);
			ImageView imgIcon = (ImageView) row.findViewById(R.id.imgAppEntryIcon);
			final CheckedTextView ctvApp = (CheckedTextView) row.findViewById(R.id.tvAppEntryName);

			// Get entry
			final XApplicationInfo appEntry = getItem(position);

			// Set icon
			imgIcon.setImageDrawable(appEntry.getDrawable());

			// Set icon/title
			ctvApp.setText(appEntry.toString());

			// Check if internet access
			if (!appEntry.hasInternet())
				ctvApp.setTextColor(Color.GRAY);

			// Check if used
			if (appEntry.isUsed())
				ctvApp.setTypeface(null, Typeface.BOLD_ITALIC);

			// Listen for restriction changes
			ctvApp.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					boolean restricted = XRestriction.getRestricted(null, view.getContext(), appEntry.getUid(),
							mRestrictionName, null, false);
					restricted = !restricted;
					ctvApp.setChecked(restricted);
					XRestriction.setRestricted(null, view.getContext(), appEntry.getUid(), mRestrictionName, null,
							restricted);
				}
			});

			// Long click: app settings
			ctvApp.setLongClickable(true);
			ctvApp.setOnLongClickListener(new View.OnLongClickListener() {
				@Override
				public boolean onLongClick(View view) {
					Intent intentSettings = new Intent(view.getContext(), XAppSettings.class);
					intentSettings.putExtra(XAppSettings.cPackageName, appEntry.getPackageName());
					intentSettings.putExtra(XAppSettings.cRestrictionExclude, mRestrictionName);
					intentSettings.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					view.getContext().startActivity(intentSettings);
					return true;
				}
			});

			return row;
		}
	}

	private class XApplicationInfo implements Comparable<XApplicationInfo> {
		private Drawable mDrawable;
		private List<String> mListApplicationName;
		private String mPackageName;
		private boolean mHasInternet;
		private boolean mIsUsed;
		private int mUid;

		public XApplicationInfo(ApplicationInfo appInfo, String restrictionName, PackageManager packageManager) {
			mDrawable = appInfo.loadIcon(packageManager);
			mListApplicationName = new ArrayList<String>();
			mListApplicationName.add((String) packageManager.getApplicationLabel(appInfo));
			mPackageName = appInfo.packageName;
			mHasInternet = XRestriction.hasInternet(getBaseContext(), appInfo.packageName);
			mIsUsed = XRestriction.isUsed(getBaseContext(), appInfo.uid, restrictionName);
			mUid = appInfo.uid;
		}

		public void AddApplicationName(String Name) {
			mListApplicationName.add(Name);
		}

		public String getPackageName() {
			return mPackageName;
		}

		public Drawable getDrawable() {
			return mDrawable;
		}

		public boolean hasInternet() {
			return mHasInternet;
		}

		public boolean isUsed() {
			return mIsUsed;
		}

		public int getUid() {
			return mUid;
		}

		@Override
		@SuppressLint("DefaultLocale")
		public String toString() {
			return String.format("%s (%d)", TextUtils.join(", ", mListApplicationName), mUid);
		}

		@Override
		public int compareTo(XApplicationInfo other) {
			return toString().compareToIgnoreCase(other.toString());
		}
	}
}

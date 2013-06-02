package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.R;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.app.Activity;
import android.content.Context;
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

	public static final String cExtraPermissionName = "Permission";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xbatchedit);

		// Get permission name
		Bundle extras = getIntent().getExtras();
		final String permissionName = extras.getString(cExtraPermissionName);

		// Display permission name
		TextView tvPermission = (TextView) findViewById(R.id.tvPermission);
		tvPermission.setText(XPermissions.getLocalizedName(getBaseContext(), permissionName));

		// Legend
		TextView tvUsed = (TextView) findViewById(R.id.tvUsed);
		tvUsed.setTypeface(null, Typeface.BOLD_ITALIC);
		TextView tvInternet = (TextView) findViewById(R.id.tvInternet);
		tvInternet.setTextColor(Color.GRAY);

		// Get app list
		PackageManager pm = getBaseContext().getPackageManager();
		SparseArray<XApplicationInfo> mapApp = new SparseArray<XApplicationInfo>();
		List<XApplicationInfo> listApp = new ArrayList<XApplicationInfo>();
		for (ApplicationInfo appInfo : pm.getInstalledApplications(PackageManager.GET_META_DATA)) {
			XApplicationInfo xAppInfo = mapApp.get(appInfo.uid);
			if (xAppInfo == null) {
				xAppInfo = new XApplicationInfo(appInfo, permissionName, pm);
				mapApp.put(appInfo.uid, xAppInfo);
				listApp.add(xAppInfo);
			} else
				xAppInfo.AddApplicationName((String) pm.getApplicationLabel(appInfo));
		}
		Collections.sort(listApp);

		// Fill app list view adapter
		ListView lvApp = (ListView) findViewById(R.id.lvApp);
		AppListAdapter appAdapter = new AppListAdapter(getBaseContext(), R.layout.xappentry, listApp, permissionName);
		lvApp.setAdapter(appAdapter);
	}

	private class AppListAdapter extends ArrayAdapter<XApplicationInfo> {

		private String mPermissionName;

		public AppListAdapter(Context context, int resource, List<XApplicationInfo> objects, String permissionName) {
			super(context, resource, objects);
			mPermissionName = permissionName;
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.xappentry, parent, false);
			ImageView imgIcon = (ImageView) row.findViewById(R.id.imgAppEntryIcon);
			final CheckedTextView tvApp = (CheckedTextView) row.findViewById(R.id.tvAppEntryName);

			// Get entry
			final XApplicationInfo appEntry = getItem(position);

			// Set icon
			imgIcon.setImageDrawable(appEntry.getDrawable());

			// Set icon/title
			tvApp.setText(appEntry.toString());

			// Check if internet access
			if (!appEntry.hasInternet())
				tvApp.setTextColor(Color.GRAY);

			// Check if used
			if (appEntry.isUsed())
				tvApp.setTypeface(null, Typeface.BOLD_ITALIC);

			// Set privacy
			boolean allowed = XPermissions
					.getAllowed(null, getBaseContext(), appEntry.getUid(), mPermissionName, false);
			tvApp.setChecked(!allowed);

			// Change privacy
			tvApp.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View v) {
					boolean allowed = XPermissions.getAllowed(null, getBaseContext(), appEntry.getUid(),
							mPermissionName, false);
					allowed = !allowed;
					tvApp.setChecked(!allowed);
					XPermissions.setAllowed(null, getBaseContext(), appEntry.getUid(), mPermissionName, allowed);
				}
			});

			return row;
		}
	}

	private class XApplicationInfo implements Comparable<XApplicationInfo> {
		private Drawable mDrawable;
		private List<String> mListApplicationName;
		private boolean mHasInternet;
		private boolean mIsUsed;
		private int mUid;

		public XApplicationInfo(ApplicationInfo appInfo, String permissionName, PackageManager packageManager) {
			mDrawable = appInfo.loadIcon(packageManager);
			mListApplicationName = new ArrayList<String>();
			mListApplicationName.add((String) packageManager.getApplicationLabel(appInfo));
			mHasInternet = XPermissions.hasInternet(getBaseContext(), appInfo.packageName);
			mIsUsed = XPermissions.isUsed(getBaseContext(), appInfo.uid, permissionName);
			mUid = appInfo.uid;
		}

		public void AddApplicationName(String Name) {
			mListApplicationName.add(Name);
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
		public String toString() {
			return String.format("%s (%d)", TextUtils.join(", ", mListApplicationName), mUid);
		}

		@Override
		public int compareTo(XApplicationInfo other) {
			return toString().compareToIgnoreCase(other.toString());
		}
	}
}

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
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
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
		final List<XApplicationInfo> listApp = new ArrayList<XApplicationInfo>();
		for (ApplicationInfo appInfo : pm.getInstalledApplications(PackageManager.GET_META_DATA))
			listApp.add(new XApplicationInfo(appInfo, permissionName, pm));
		Collections.sort(listApp);

		// Fill app list view adapter
		final ListView lvApp = (ListView) findViewById(R.id.lvApp);
		AppListAdapter appAdapter = new AppListAdapter(getBaseContext(),
				android.R.layout.simple_list_item_multiple_choice, listApp);
		lvApp.setAdapter(appAdapter);

		// Set privacy values
		for (int position = 0; position < lvApp.getAdapter().getCount(); position++)
			lvApp.setItemChecked(position, !XPermissions.getAllowed(null, getBaseContext(), listApp.get(position)
					.getUid(), permissionName, false));

		// Listen for privacy changes
		lvApp.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				boolean allowed = !lvApp.isItemChecked(position);
				XPermissions
						.setAllowed(null, getBaseContext(), listApp.get(position).getUid(), permissionName, allowed);
			}
		});
	}

	private class XApplicationInfo implements Comparable<XApplicationInfo> {
		private String mApplicationName;
		private boolean mHasInternet;
		private boolean mIsUsed;
		private int mUid;

		public XApplicationInfo(ApplicationInfo appInfo, String permissionName, PackageManager packageManager) {
			mApplicationName = (String) packageManager.getApplicationLabel(appInfo);
			mHasInternet = XPermissions.hasInternet(getBaseContext(), appInfo.packageName);
			mIsUsed = XPermissions.isUsed(getBaseContext(), appInfo.uid, permissionName);
			mUid = appInfo.uid;
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
			return mApplicationName;
		}

		@Override
		public int compareTo(XApplicationInfo other) {
			return toString().compareTo(other.toString());
		}
	}

	private class AppListAdapter extends ArrayAdapter<XApplicationInfo> {
		public AppListAdapter(Context context, int resource, List<XApplicationInfo> objects) {
			super(context, resource, objects);
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(android.R.layout.simple_list_item_multiple_choice, parent, false);
			TextView tvApp = (TextView) row.findViewById(android.R.id.text1);

			// Get entry
			XApplicationInfo appEntry = getItem(position);

			// Set title
			tvApp.setText(appEntry.toString());

			// Check if internet access
			if (!appEntry.hasInternet())
				tvApp.setTextColor(Color.GRAY);

			// Check if used
			if (appEntry.isUsed())
				tvApp.setTypeface(null, Typeface.BOLD_ITALIC);

			return row;
		}
	}
}

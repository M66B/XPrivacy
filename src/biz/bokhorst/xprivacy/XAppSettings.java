package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.ContentResolver;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.Resources;
import android.database.Cursor;
import android.graphics.Color;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.AdapterView.OnItemClickListener;

public class XAppSettings extends Activity {

	public static final String cExtraPackageName = "PackageName";

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xappsettings);

		// Get package name
		Bundle extras = getIntent().getExtras();
		String packageName = extras.getString(cExtraPackageName);

		// Get app info
		PackageManager pm = getBaseContext().getPackageManager();
		final ApplicationInfo appInfo;
		try {
			appInfo = pm.getApplicationInfo(packageName, 0);
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
			return;
		}

		// Display app name
		TextView tvAppName = (TextView) findViewById(R.id.tvAppName);
		tvAppName.setText(pm.getApplicationLabel(appInfo));

		// Check if internet access
		if (pm.checkPermission("android.permission.INTERNET", packageName) == PackageManager.PERMISSION_GRANTED)
			findViewById(R.id.tvInternet).setVisibility(View.GONE);

		// Fill privacy list view adapter
		ListView lvPrivacy = (ListView) findViewById(R.id.lvPrivacy);
		List<String> permissionsList = new ArrayList<String>(XPermissions.cPermissions.keySet());
		PermissionsAdapter privacyListAdapter = new PermissionsAdapter(getBaseContext(),
				android.R.layout.simple_list_item_multiple_choice, permissionsList, appInfo);
		lvPrivacy.setAdapter(privacyListAdapter);

		// Set privacy values
		for (int position = 0; position < lvPrivacy.getAdapter().getCount(); position++) {
			String permissionName = (String) lvPrivacy.getItemAtPosition(position);
			lvPrivacy.setItemChecked(position,
					XUtil.getAllowed(null, getBaseContext(), appInfo.uid, permissionName, false));
		}

		// Listen for privacy changes
		lvPrivacy.setOnItemClickListener(new OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				ListView privacyListView = (ListView) parent;
				String permissionName = (String) privacyListView.getItemAtPosition(position);
				boolean allowed = privacyListView.isItemChecked(position);
				XUtil.setAllowed(null, getBaseContext(), appInfo.uid, permissionName, allowed);
			}
		});
	}

	private class PermissionsAdapter extends ArrayAdapter<String> {
		private List<String> mPermissions;
		ApplicationInfo mAppInfo;

		public PermissionsAdapter(Context context, int resource, List<String> objects, ApplicationInfo appInfo) {
			super(context, resource, objects);
			this.mPermissions = objects;
			mAppInfo = appInfo;
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			// Get view info
			String permissionName = mPermissions.get(position);
			TextView textView = (TextView) super.getView(position, convertView, parent);

			// Get resources
			PackageManager pm = textView.getContext().getPackageManager();
			String packageName = this.getClass().getPackage().getName();
			Resources resources;
			try {
				resources = pm.getResourcesForApplication(packageName);
			} catch (NameNotFoundException e) {
				e.printStackTrace();
				return textView;
			}

			// Localize text
			int stringId = resources.getIdentifier("perm_" + permissionName, "string", packageName);
			if (stringId != 0)
				textView.setText(resources.getString(stringId));

			// Check if permission granted
			String[] aPermissions = XPermissions.cPermissions.get(permissionName);
			boolean permissionGranted = (aPermissions.length == 0);
			for (String aPermission : aPermissions)
				if (pm.checkPermission("android.permission." + aPermission, mAppInfo.packageName) == PackageManager.PERMISSION_GRANTED) {
					permissionGranted = true;
					break;
				}
			if (!permissionGranted)
				textView.setTextColor(Color.GRAY);

			// Check last usage
			ContentResolver cr = textView.getContext().getContentResolver();
			Cursor cursor = cr.query(XPrivacyProvider.URI_LASTUSED, null, permissionName,
					new String[] { Integer.toString(mAppInfo.uid) }, null);
			if (cursor.moveToNext()) {
				long lastUsage = cursor.getLong(cursor.getColumnIndex(XPrivacyProvider.COL_LASTUSED));
				cursor.close();
				if (lastUsage != 0)
					textView.setTypeface(null, Typeface.BOLD_ITALIC);
			}

			return textView;
		}
	}
}

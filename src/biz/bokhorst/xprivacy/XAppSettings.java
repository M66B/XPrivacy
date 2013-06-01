package biz.bokhorst.xprivacy;

import java.util.ArrayList;
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
		TextView tvAppName = (TextView) findViewById(R.id.tvApp);
		tvAppName.setText(pm.getApplicationLabel(appInfo));

		// Check if internet access
		if (XPermissions.hasInternet(getBaseContext(), packageName))
			findViewById(R.id.tvInternet).setVisibility(View.GONE);

		// Legend
		TextView tvUsed = (TextView) findViewById(R.id.tvUsed);
		tvUsed.setTypeface(null, Typeface.BOLD_ITALIC);
		TextView tvGranted = (TextView) findViewById(R.id.tvGranted);
		tvGranted.setTextColor(Color.GRAY);

		// Fill privacy list view adapter
		final ListView lvPrivacy = (ListView) findViewById(R.id.lvPermission);
		List<String> listPermission = new ArrayList<String>(XPermissions.cPermissions.keySet());
		PermissionAdapter privacyListAdapter = new PermissionAdapter(getBaseContext(),
				android.R.layout.simple_list_item_multiple_choice, appInfo, listPermission);
		lvPrivacy.setAdapter(privacyListAdapter);

		// Set privacy values
		for (int position = 0; position < lvPrivacy.getAdapter().getCount(); position++) {
			String permissionName = (String) lvPrivacy.getItemAtPosition(position);
			lvPrivacy.setItemChecked(position,
					!XPermissions.getAllowed(null, getBaseContext(), appInfo.uid, permissionName, false));
		}

		// Listen for privacy changes
		lvPrivacy.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				String permissionName = (String) lvPrivacy.getItemAtPosition(position);
				boolean allowed = !lvPrivacy.isItemChecked(position);
				XPermissions.setAllowed(null, getBaseContext(), appInfo.uid, permissionName, allowed);
			}
		});
	}

	private class PermissionAdapter extends ArrayAdapter<String> {
		private ApplicationInfo mAppInfo;

		public PermissionAdapter(Context context, int resource, ApplicationInfo appInfo, List<String> objects) {
			super(context, resource, objects);
			mAppInfo = appInfo;
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(android.R.layout.simple_list_item_multiple_choice, parent, false);
			TextView tvPermission = (TextView) row.findViewById(android.R.id.text1);

			// Display localize name
			String permissionName = getItem(position);
			tvPermission.setText(XPermissions.getLocalizedName(getBaseContext(), permissionName));

			// Display if permission granted
			if (!XPermissions.isGranted(getBaseContext(), mAppInfo.packageName, permissionName))
				tvPermission.setTextColor(Color.GRAY);

			// Display if used
			if (XPermissions.isUsed(getBaseContext(), mAppInfo.uid, permissionName))
				tvPermission.setTypeface(null, Typeface.BOLD_ITALIC);

			return row;
		}
	}
}

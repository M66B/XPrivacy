package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

public class XMain extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xmain);

		// Fill permission list view adapter
		final List<String> listPermission = new ArrayList<String>(XPermissions.cPermissions.keySet());
		final ListView lvPermission = (ListView) findViewById(R.id.lvPermission);
		PermissionAdapter permissionAdapter = new PermissionAdapter(getBaseContext(),
				android.R.layout.simple_list_item_1, listPermission);
		lvPermission.setAdapter(permissionAdapter);

		// Listen for privacy changes
		lvPermission.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

				String permissionName = listPermission.get(position);
				Intent intentBatch = new Intent(getBaseContext(), XBatchEdit.class);
				intentBatch.putExtra(XBatchEdit.cExtraPermissionName, permissionName);
				intentBatch.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
				startActivity(intentBatch);
			}
		});

		// Show version
		try {
			PackageInfo pInfo = getBaseContext().getPackageManager().getPackageInfo(getPackageName(), 0);
			TextView tvVersion = (TextView) findViewById(R.id.tvVersion);
			tvVersion.setText(String.format(getString(R.string.app_version), pInfo.versionName, pInfo.versionCode));
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
		}
	}

	private class PermissionAdapter extends ArrayAdapter<String> {

		public PermissionAdapter(Context context, int resource, List<String> objects) {
			super(context, resource, objects);
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			View row = convertView;
			if (row == null) {
				LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(
						Context.LAYOUT_INFLATER_SERVICE);
				row = inflater.inflate(android.R.layout.simple_list_item_1, parent, false);
			}
			TextView tvPermission = (TextView) row.findViewById(android.R.id.text1);

			// Display localize name
			String permissionName = getItem(position);
			tvPermission.setText(XPermissions.getLocalizedName(getBaseContext(), permissionName));

			return row;
		}
	}
}

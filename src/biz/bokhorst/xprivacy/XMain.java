package biz.bokhorst.xprivacy;

import java.util.List;

import android.os.Bundle;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
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

	private final static int cXposedMinVersion = 34;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xmain);

		// Show version
		try {
			PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
			TextView tvVersion = (TextView) findViewById(R.id.tvVersion);
			tvVersion.setText(String.format(getString(R.string.app_version), pInfo.versionName, pInfo.versionCode));
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
		}

		// Check Xposed version
		int xVersion = XUtil.getXposedVersion();
		if (xVersion < cXposedMinVersion) {
			AlertDialog alertDialog = new AlertDialog.Builder(this).create();
			alertDialog.setTitle(getString(R.string.app_name));
			alertDialog.setMessage(String.format(getString(R.string.app_notxposed), cXposedMinVersion));
			alertDialog.setIcon(R.drawable.ic_launcher);
			alertDialog.setButton(AlertDialog.BUTTON_POSITIVE, "OK", new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int which) {
				}
			});
			alertDialog.show();
		}

		// Show Xposed version
		TextView tvXVersion = (TextView) findViewById(R.id.tvXVersion);
		tvXVersion.setText(String.format(getString(R.string.app_xversion), xVersion));

		// Fill restriction list view adapter
		final List<String> listRestriction = XRestriction.getRestrictions();
		final ListView lvRestriction = (ListView) findViewById(R.id.lvRestriction);
		RestrictionAdapter restrictionAdapter = new RestrictionAdapter(getBaseContext(),
				android.R.layout.simple_list_item_1, listRestriction);
		lvRestriction.setAdapter(restrictionAdapter);

		// Listen for privacy changes
		lvRestriction.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

				String restrictionName = listRestriction.get(position);
				Intent intentBatch = new Intent(view.getContext(), XBatchEdit.class);
				intentBatch.putExtra(XBatchEdit.cRestrictionName, restrictionName);
				intentBatch.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
				startActivity(intentBatch);
			}
		});

	}

	private class RestrictionAdapter extends ArrayAdapter<String> {

		public RestrictionAdapter(Context context, int resource, List<String> objects) {
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
			TextView tvRestriction = (TextView) row.findViewById(android.R.id.text1);

			// Display localize name
			String restrictionName = getItem(position);
			tvRestriction.setText(XRestriction.getLocalizedName(row.getContext(), restrictionName));

			return row;
		}
	}
}

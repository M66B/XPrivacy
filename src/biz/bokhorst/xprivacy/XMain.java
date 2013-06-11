package biz.bokhorst.xprivacy;

import java.util.List;

import android.net.Uri;
import android.os.Bundle;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
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

		// Fill restriction list view adapter
		final List<String> listRestriction = XRestriction.getRestrictions();
		final ListView lvRestriction = (ListView) findViewById(R.id.lvRestriction);
		RestrictionAdapter restrictionAdapter = new RestrictionAdapter(getBaseContext(),
				android.R.layout.simple_list_item_1, listRestriction);
		lvRestriction.setAdapter(restrictionAdapter);

		// Click: batch edit
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

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.xmain, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		try {
			switch (item.getItemId()) {
			case R.id.menu_report:
				// Report issue
				Intent browserIntent = new Intent(Intent.ACTION_VIEW,
						Uri.parse("https://github.com/M66B/XPrivacy/issues"));
				startActivity(browserIntent);
				return true;
			case R.id.menu_about:
				// About
				Dialog dialog = new Dialog(this);
				dialog.setTitle(getString(R.string.app_name));
				dialog.setContentView(R.layout.xabout);

				// Show version
				try {
					PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
					TextView tvVersion = (TextView) dialog.findViewById(R.id.tvVersion);
					tvVersion.setText(String.format(getString(R.string.app_version), pInfo.versionName,
							pInfo.versionCode));
				} catch (Throwable ex) {
					XUtil.bug(null, ex);
				}

				// Show Xposed version
				int xVersion = XUtil.getXposedVersion();
				TextView tvXVersion = (TextView) dialog.findViewById(R.id.tvXVersion);
				tvXVersion.setText(String.format(getString(R.string.app_xversion), xVersion));

				dialog.setCancelable(true);
				dialog.show();
				return true;
			default:
				return super.onOptionsItemSelected(item);
			}
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
			return true;
		}
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

			// Get entry
			String restrictionName = getItem(position);

			// Display localize name
			tvRestriction.setText(XRestriction.getLocalizedName(row.getContext(), restrictionName));

			return row;
		}
	}
}

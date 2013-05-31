package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.CheckBox;
import android.widget.Spinner;
import android.widget.TextView;

public class XMain extends Activity {
	private static final String PREF_NAME = "Preferences";
	private static final String PREF_DEBUG = "Preference.Debug";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xmain);

		// Batch edit
		List<String> listPermission = new ArrayList<String>();
		listPermission.add(getString(R.string.app_select));
		listPermission.addAll(XPermissions.cPermissions.keySet());
		ArrayAdapter<String> spinAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item,
				listPermission);
		final Spinner spinPermission = (Spinner) findViewById(R.id.spinPermission);
		spinPermission.setAdapter(spinAdapter);
		spinPermission.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {

			@Override
			public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
				if (position != 0) {
					String permission = XPermissions.cPermissions.keySet().toArray(new String[0])[position - 1];
					Intent intentBatch = new Intent(getBaseContext(), XBatchEdit.class);
					intentBatch.putExtra(XBatchEdit.cExtraPermissionName, permission);
					intentBatch.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					startActivity(intentBatch);
				}
			}

			@Override
			public void onNothingSelected(AdapterView<?> arg0) {
			}
		});

		// Check box default deny
		final SharedPreferences prefs = getBaseContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
		final CheckBox cbDebug = (CheckBox) findViewById(R.id.cbDebug);
		cbDebug.setChecked(prefs.getBoolean(PREF_DEBUG, false));
		cbDebug.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View view) {
				SharedPreferences.Editor editor = prefs.edit();
				editor.putBoolean(PREF_DEBUG, cbDebug.isChecked());
				editor.commit();
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
}

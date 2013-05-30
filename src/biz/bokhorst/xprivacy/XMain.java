package biz.bokhorst.xprivacy;

import android.os.Bundle;
import android.app.Activity;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.CheckBox;
import android.widget.TextView;

public class XMain extends Activity {
	private static final String PREF_NAME = "Preferences";
	private static final String PREF_DEFAULTDENY = "Preference.DefaultDeny";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.xmain);

		final SharedPreferences prefs = getBaseContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);

		// Check box default deny
		final CheckBox cbDefaultDeny = (CheckBox) findViewById(R.id.cbDefaultDeny);
		cbDefaultDeny.setEnabled(false);
		cbDefaultDeny.setChecked(prefs.getBoolean(PREF_DEFAULTDENY, false));
		cbDefaultDeny.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View view) {
				SharedPreferences.Editor editor = prefs.edit();
				editor.putBoolean(PREF_DEFAULTDENY, cbDefaultDeny.isChecked());
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

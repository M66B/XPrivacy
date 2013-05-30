package biz.bokhorst.xprivacy;

import android.os.Bundle;
import android.app.Activity;
import android.content.Context;
import android.content.SharedPreferences;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CheckBox;

public class XMain extends Activity {
	private static final String PREF_NAME = "Preferences";
	private static final String PREF_DEFAULTDENY = "Preference.DefaultDeny";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.xmain);

		final SharedPreferences prefs = getBaseContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);

		final CheckBox cbDefaultDeny = (CheckBox) (Button) findViewById(R.id.cbDefaultDeny);
		cbDefaultDeny.setChecked(prefs.getBoolean(PREF_DEFAULTDENY, false));
		cbDefaultDeny.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View view) {
				SharedPreferences.Editor editor = prefs.edit();
				editor.putBoolean(PREF_DEFAULTDENY, cbDefaultDeny.isChecked());
				editor.commit();
			}
		});
	}
}

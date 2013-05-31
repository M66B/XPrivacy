package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.R;
import android.app.Activity;
import android.os.Bundle;
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
		String permissioName = extras.getString(cExtraPermissionName);

		// Get package name
		TextView tvPermission = (TextView) findViewById(R.id.tvPermission);
		tvPermission.setText(permissioName);
	}
}

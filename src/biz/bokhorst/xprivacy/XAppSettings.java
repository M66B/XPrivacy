package biz.bokhorst.xprivacy;

import java.util.List;

import android.app.Activity;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.CheckedTextView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

public class XAppSettings extends Activity {

	public static final String cPackageName = "PackageName";
	public static final String cRestrictionExclude = "RestrictionExclude";

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xappsettings);

		// Get package name
		Bundle extras = getIntent().getExtras();
		String packageName = extras.getString(cPackageName);
		String restrictionExclude = extras.getString(cRestrictionExclude);

		// Get app info
		final ApplicationInfo appInfo;
		try {
			appInfo = getPackageManager().getApplicationInfo(packageName, 0);
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
			return;
		}

		// Display app name
		TextView tvAppName = (TextView) findViewById(R.id.tvApp);
		tvAppName.setText(String.format("%s (%d)", getPackageManager().getApplicationLabel(appInfo), appInfo.uid));

		// Check if internet access
		if (XRestriction.hasInternet(this, packageName))
			findViewById(R.id.tvInternet).setVisibility(View.GONE);

		// Build list with restrictions
		List<String> listRestriction = XRestriction.getRestrictions();
		if (restrictionExclude != null)
			listRestriction.remove(restrictionExclude);

		// Fill privacy list view adapter
		final ListView lvRestriction = (ListView) findViewById(R.id.lvRestriction);
		RestrictionAdapter privacyListAdapter = new RestrictionAdapter(this, R.layout.xappentry, appInfo,
				listRestriction);
		lvRestriction.setAdapter(privacyListAdapter);
	}

	private class RestrictionAdapter extends ArrayAdapter<String> {
		private ApplicationInfo mAppInfo;

		public RestrictionAdapter(Context context, int resource, ApplicationInfo appInfo, List<String> objects) {
			super(context, resource, objects);
			mAppInfo = appInfo;
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.xappentry, parent, false);
			ImageView imgGranted = (ImageView) row.findViewById(R.id.imgAppEntryGranted);
			ImageView imgUsed = (ImageView) row.findViewById(R.id.imgAppEntryUsed);
			final CheckedTextView ctvRestriction = (CheckedTextView) row.findViewById(R.id.tvAppEntryName);

			// Get entry
			final String restrictionName = getItem(position);

			// Display localized name
			ctvRestriction.setText(XRestriction.getLocalizedName(row.getContext(), restrictionName));

			// Display if restriction granted
			if (!XRestriction.hasPermission(row.getContext(), mAppInfo.packageName, restrictionName))
				imgGranted.setVisibility(View.INVISIBLE);

			// Display if used
			if (XRestriction.isUsed(row.getContext(), mAppInfo.uid, restrictionName))
				ctvRestriction.setTypeface(null, Typeface.BOLD_ITALIC);
			else
				imgUsed.setVisibility(View.INVISIBLE);

			// Display restriction
			boolean restricted = XRestriction.getRestricted(null, row.getContext(), mAppInfo.uid, restrictionName,
					false);
			ctvRestriction.setChecked(restricted);

			// Listen for restriction changes
			ctvRestriction.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					boolean restricted = XRestriction.getRestricted(null, view.getContext(), mAppInfo.uid,
							restrictionName, false);
					restricted = !restricted;
					ctvRestriction.setChecked(restricted);
					XRestriction.setRestricted(null, view.getContext(), mAppInfo.uid, restrictionName, restricted);
				}
			});

			return row;
		}
	}
}

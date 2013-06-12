package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.ContentResolver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.pm.ApplicationInfo;
import android.database.Cursor;
import android.graphics.Typeface;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.ArrayAdapter;
import android.widget.CheckedTextView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

public class XAppEdit extends Activity {

	public static final String cPackageName = "PackageName";

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xapplist);

		// Get package name
		Bundle extras = getIntent().getExtras();
		String packageName = extras.getString(cPackageName);

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

		// Handle help
		tvAppName.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				Dialog dialog = new Dialog(XAppEdit.this);
				dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
				dialog.setTitle(getString(R.string.help_application));
				dialog.setContentView(R.layout.xhelp);
				dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, R.drawable.ic_launcher);
				dialog.setCancelable(true);
				dialog.show();
			}
		});

		// Display app icon
		ImageView imgIcon = (ImageView) findViewById(R.id.imgAppEntryIcon);
		imgIcon.setImageDrawable(appInfo.loadIcon(getPackageManager()));

		// Check if internet access
		ImageView imgInternet = (ImageView) findViewById(R.id.imgAppEntryInternet);
		if (!XRestriction.hasInternet(this, packageName))
			imgInternet.setVisibility(View.INVISIBLE);

		// Fill privacy list view adapter
		final ListView lvRestriction = (ListView) findViewById(R.id.lvRestriction);
		RestrictionAdapter privacyListAdapter = new RestrictionAdapter(this, R.layout.xappentry, appInfo,
				XRestriction.getRestrictions(this));
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

			// Handle used click
			imgUsed.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					// Get audit
					ContentResolver contentResolver = view.getContext().getContentResolver();
					Cursor cursor = contentResolver.query(XPrivacyProvider.URI_AUDIT, null, restrictionName,
							new String[] { Integer.toString(mAppInfo.uid) }, null);
					List<String> listAudit = new ArrayList<String>();
					while (cursor.moveToNext())
						listAudit.add(cursor.getString(cursor.getColumnIndex(XPrivacyProvider.COL_METHOD)));
					cursor.close();
					Collections.sort(listAudit);

					// Display audit
					String localRestrictionName = XRestriction.getLocalizedName(view.getContext(), restrictionName);
					AlertDialog alertDialog = new AlertDialog.Builder(XAppEdit.this).create();
					alertDialog.setTitle(localRestrictionName);
					alertDialog.setIcon(R.drawable.ic_launcher);
					alertDialog.setMessage(TextUtils.join("\n", listAudit));
					alertDialog.setButton(AlertDialog.BUTTON_POSITIVE, "OK", new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
						}
					});
					alertDialog.show();
				}
			});

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

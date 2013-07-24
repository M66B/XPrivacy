package biz.bokhorst.xprivacy;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.NavUtils;
import android.support.v4.app.TaskStackBuilder;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

public class ActivityUsage extends Activity {

	private int mThemeId;
	private UsageAdapter mUsageAdapter;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Set theme
		String themeName = PrivacyManager.getSetting(null, this, PrivacyManager.cSettingTheme, "", false);
		mThemeId = (themeName.equals("Dark") ? R.style.CustomTheme : R.style.CustomTheme_Light);
		setTheme(mThemeId);

		// Set layout
		setContentView(R.layout.usagelist);

		// Provide content
		ListView lvUsage = (ListView) findViewById(R.id.lvUsage);
		List<PrivacyManager.UsageData> listUsageData = PrivacyManager.getUsed(this);
		mUsageAdapter = new UsageAdapter(this, R.layout.usageentry, listUsageData);
		lvUsage.setAdapter(mUsageAdapter);

		// Up navigation
		getActionBar().setDisplayHomeAsUpEnabled(true);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case android.R.id.home:
			Intent upIntent = NavUtils.getParentActivityIntent(this);
			if (upIntent != null)
				if (NavUtils.shouldUpRecreateTask(this, upIntent))
					TaskStackBuilder.create(this).addNextIntentWithParentStack(upIntent).startActivities();
				else
					NavUtils.navigateUpTo(this, upIntent);
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mUsageAdapter != null)
			mUsageAdapter.notifyDataSetChanged();
	}

	private class UsageAdapter extends ArrayAdapter<PrivacyManager.UsageData> {
		public UsageAdapter(Context context, int textViewResourceId, List<PrivacyManager.UsageData> objects) {
			super(context, textViewResourceId, objects);
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			// Get layout
			LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.usageentry, parent, false);
			TextView tvTime = (TextView) row.findViewById(R.id.tvTime);
			TextView tvApp = (TextView) row.findViewById(R.id.tvApp);
			TextView tvRestriction = (TextView) row.findViewById(R.id.tvRestriction);
			TextView tvMethod = (TextView) row.findViewById(R.id.tvMethod);
			TextView tvRestricted = (TextView) row.findViewById(R.id.tvRestricted);

			// Get data
			PrivacyManager.UsageData usageData = getItem(position);

			// Build entry
			Date date = new Date(usageData.getTimeStamp());
			SimpleDateFormat format = new SimpleDateFormat("dd HH:mm:ss", Locale.ROOT);
			tvTime.setText(format.format(date));
			tvApp.setText(Integer.toString(usageData.getUid()));
			tvRestriction.setText(usageData.getRestrictionName());
			tvMethod.setText(usageData.getMethodName());
			tvRestricted.setText(Boolean.toString(usageData.getRestricted()));

			return row;
		}
	}
}

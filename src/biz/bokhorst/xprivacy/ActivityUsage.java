package biz.bokhorst.xprivacy;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Process;
import android.support.v4.app.NavUtils;
import android.support.v4.app.TaskStackBuilder;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Filter;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

public class ActivityUsage extends Activity {
	private int mThemeId;
	private boolean mAll = true;
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

		// Start task to get usage data
		UsageTask usageTask = new UsageTask();
		usageTask.execute();

		// Listen for clicks
		ListView lvUsage = (ListView) findViewById(R.id.lvUsage);
		lvUsage.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> adapter, View view, int position, long arg) {
				PrivacyManager.UsageData usageData = mUsageAdapter.getItem(position);
				String[] packageName = getPackageManager().getPackagesForUid(usageData.getUid());
				if (packageName != null && packageName.length > 0) {
					Intent intent = new Intent(ActivityUsage.this, ActivityApp.class);
					intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					intent.putExtra(ActivityApp.cPackageName, packageName[0]);
					startActivity(intent);
				}
			}
		});

		// Up navigation
		getActionBar().setDisplayHomeAsUpEnabled(true);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.usage, menu);
		return true;
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
		case R.id.menu_all:
			mAll = !mAll;
			if (mUsageAdapter != null)
				mUsageAdapter.getFilter().filter(Boolean.toString(mAll));
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

	private class UsageTask extends AsyncTask<Object, Object, List<PrivacyManager.UsageData>> {
		@Override
		protected List<PrivacyManager.UsageData> doInBackground(Object... arg0) {
			Process.setThreadPriority(Process.THREAD_PRIORITY_BACKGROUND + Process.THREAD_PRIORITY_MORE_FAVORABLE);
			return PrivacyManager.getUsed(ActivityUsage.this);
		}

		@Override
		protected void onPostExecute(List<PrivacyManager.UsageData> listUsageData) {
			super.onPostExecute(listUsageData);

			mUsageAdapter = new UsageAdapter(ActivityUsage.this, R.layout.usageentry, listUsageData);
			ListView lvUsage = (ListView) findViewById(R.id.lvUsage);
			lvUsage.setAdapter(mUsageAdapter);
		}
	}

	private class UsageAdapter extends ArrayAdapter<PrivacyManager.UsageData> {
		private List<PrivacyManager.UsageData> mListUsageData;

		public UsageAdapter(Context context, int textViewResourceId, List<PrivacyManager.UsageData> objects) {
			super(context, textViewResourceId, objects);
			mListUsageData = objects;
		}

		@Override
		public Filter getFilter() {
			return new UsageFilter();
		}

		private class UsageFilter extends Filter {
			public UsageFilter() {
			}

			@Override
			protected FilterResults performFiltering(CharSequence constraint) {
				FilterResults results = new FilterResults();

				// Get argument
				boolean all = Boolean.parseBoolean((String) constraint);

				// Match applications
				List<PrivacyManager.UsageData> lstResult = new ArrayList<PrivacyManager.UsageData>();
				for (PrivacyManager.UsageData usageData : UsageAdapter.this.mListUsageData) {
					if (all ? true : usageData.getRestricted())
						lstResult.add(usageData);
				}

				synchronized (this) {
					results.values = lstResult;
					results.count = lstResult.size();
				}

				return results;
			}

			@Override
			@SuppressWarnings("unchecked")
			protected void publishResults(CharSequence constraint, FilterResults results) {
				clear();
				if (results.values == null)
					notifyDataSetInvalidated();
				else {
					addAll((ArrayList<PrivacyManager.UsageData>) results.values);
					notifyDataSetChanged();
				}
			}
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			// Get layout
			LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.usageentry, parent, false);
			TextView tvTime = (TextView) row.findViewById(R.id.tvTime);
			TextView tvApp = (TextView) row.findViewById(R.id.tvApp);
			TextView tvRestriction = (TextView) row.findViewById(R.id.tvRestriction);
			ImageView imgRestricted = (ImageView) row.findViewById(R.id.imgRestricted);

			// Get data
			PrivacyManager.UsageData usageData = getItem(position);

			// Build entry
			Date date = new Date(usageData.getTimeStamp());
			SimpleDateFormat format = new SimpleDateFormat("HH:mm:ss", Locale.ROOT);
			tvTime.setText(format.format(date));
			tvApp.setText(Integer.toString(usageData.getUid()));
			tvRestriction.setText(String.format("%s/%s", usageData.getRestrictionName(), usageData.getMethodName()));
			imgRestricted.setVisibility(usageData.getRestricted() ? View.VISIBLE : View.INVISIBLE);

			return row;
		}
	}
}

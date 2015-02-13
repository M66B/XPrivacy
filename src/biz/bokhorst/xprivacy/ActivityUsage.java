package biz.bokhorst.xprivacy;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Process;
import android.support.v4.app.NavUtils;
import android.support.v4.app.TaskStackBuilder;
import android.support.v7.widget.Toolbar;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Filter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

public class ActivityUsage extends ActivityBase {
	private boolean mAll = true;
	private int mUid;
	private String mRestrictionName;
	private UsageAdapter mUsageAdapter;

	public static final String cUid = "Uid";
	public static final String cRestriction = "Restriction";

	private static ExecutorService mExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors(),
			new PriorityThreadFactory());

	private static class PriorityThreadFactory implements ThreadFactory {
		@Override
		public Thread newThread(Runnable r) {
			Thread t = new Thread(r);
			t.setPriority(Thread.NORM_PRIORITY);
			return t;
		}
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Check privacy service client
		if (!PrivacyService.checkClient())
			return;

		// Set layout
		setContentView(R.layout.usagelist);
		setSupportActionBar((Toolbar) findViewById(R.id.widgetToolbar));

		// Get uid
		Bundle extras = getIntent().getExtras();
		mUid = (extras == null ? 0 : extras.getInt(cUid, 0));
		mRestrictionName = (extras == null ? null : extras.getString(cRestriction));

		// Show title
		updateTitle();

		// Start task to get usage data
		UsageTask usageTask = new UsageTask();
		usageTask.executeOnExecutor(mExecutor, (Object) null);

		// Up navigation
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mUsageAdapter != null)
			mUsageAdapter.notifyDataSetChanged();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		if (inflater != null && PrivacyService.checkClient()) {
			inflater.inflate(R.menu.usage, menu);
			return true;
		} else
			return false;
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		menu.findItem(R.id.menu_whitelists).setVisible(mUid != 0);
		return super.onPrepareOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		UsageTask usageTask;
		switch (item.getItemId()) {
		case android.R.id.home:
			Intent upIntent = NavUtils.getParentActivityIntent(this);
			if (upIntent != null)
				if (NavUtils.shouldUpRecreateTask(this, upIntent))
					TaskStackBuilder.create(this).addNextIntentWithParentStack(upIntent).startActivities();
				else
					NavUtils.navigateUpTo(this, upIntent);
			return true;

		case R.id.menu_usage_all:
			mAll = !mAll;
			if (mUsageAdapter != null)
				mUsageAdapter.getFilter().filter(Boolean.toString(mAll));
			return true;

		case R.id.menu_refresh:
			updateTitle();
			usageTask = new UsageTask();
			usageTask.executeOnExecutor(mExecutor, (Object) null);
			return true;

		case R.id.menu_clear:
			PrivacyManager.deleteUsage(mUid);
			usageTask = new UsageTask();
			usageTask.executeOnExecutor(mExecutor, (Object) null);
			return true;

		case R.id.menu_whitelists:
			if (Util.hasProLicense(this) == null) {
				// Redirect to pro page
				Util.viewUri(this, ActivityMain.cProUri);
			} else {
				WhitelistTask whitelistsTask = new WhitelistTask(mUid, null, this);
				whitelistsTask.executeOnExecutor(mExecutor, (Object) null);
			}
			return true;

		case R.id.menu_settings:
			Intent intent = new Intent(this, ActivitySettings.class);
			startActivity(intent);
			return true;

		default:
			return super.onOptionsItemSelected(item);
		}
	}

	// Tasks

	private class UsageTask extends AsyncTask<Object, Object, List<PRestriction>> {
		@Override
		protected List<PRestriction> doInBackground(Object... arg0) {
			List<PRestriction> listUsageData = new ArrayList<PRestriction>();
			for (PRestriction usageData : PrivacyManager.getUsageList(ActivityUsage.this, mUid, mRestrictionName))
				listUsageData.add(usageData);
			return listUsageData;
		}

		@Override
		protected void onPostExecute(List<PRestriction> listUsageData) {
			if (!ActivityUsage.this.isFinishing()) {
				mUsageAdapter = new UsageAdapter(ActivityUsage.this, R.layout.usageentry, listUsageData);
				ListView lvUsage = (ListView) findViewById(R.id.lvUsage);
				lvUsage.setAdapter(mUsageAdapter);
				mUsageAdapter.getFilter().filter(Boolean.toString(mAll));
			}

			super.onPostExecute(listUsageData);
		}
	}

	// Adapters

	private class UsageAdapter extends ArrayAdapter<PRestriction> {
		private boolean mHasProLicense = false;
		private List<PRestriction> mListUsageData;
		private LayoutInflater mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

		public UsageAdapter(Context context, int textViewResourceId, List<PRestriction> objects) {
			super(context, textViewResourceId, objects);
			mHasProLicense = (Util.hasProLicense(ActivityUsage.this) != null);
			mListUsageData = new ArrayList<PRestriction>();
			mListUsageData.addAll(objects);
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
				List<PRestriction> lstResult = new ArrayList<PRestriction>();
				for (PRestriction usageData : UsageAdapter.this.mListUsageData) {
					if (all ? true : usageData.restricted)
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
					addAll((ArrayList<PRestriction>) results.values);
					notifyDataSetChanged();
				}
			}
		}

		private class ViewHolder {
			private View row;
			private int position;
			public LinearLayout llUsage;
			public TextView tvTime;
			public ImageView imgIcon;
			public ImageView imgRestricted;
			public TextView tvApp;
			public TextView tvRestriction;
			public TextView tvParameter;
			public TextView tvValue;

			public ViewHolder(View theRow, int thePosition) {
				row = theRow;
				position = thePosition;
				llUsage = (LinearLayout) row.findViewById(R.id.llUsage);
				tvTime = (TextView) row.findViewById(R.id.tvTime);
				imgIcon = (ImageView) row.findViewById(R.id.imgIcon);
				imgRestricted = (ImageView) row.findViewById(R.id.imgRestricted);
				tvApp = (TextView) row.findViewById(R.id.tvApp);
				tvRestriction = (TextView) row.findViewById(R.id.tvRestriction);
				tvParameter = (TextView) row.findViewById(R.id.tvParameter);
				tvValue = (TextView) row.findViewById(R.id.tvValue);
			}
		}

		private class HolderTask extends AsyncTask<Object, Object, Object> {
			private int position;
			private ViewHolder holder;
			private PRestriction usageData;
			private Drawable icon = null;
			private boolean system;
			private Hook hook;

			public HolderTask(int thePosition, ViewHolder theHolder, PRestriction theUsageData) {
				position = thePosition;
				holder = theHolder;
				usageData = theUsageData;
			}

			@Override
			protected Object doInBackground(Object... params) {
				if (usageData != null) {
					ApplicationInfoEx appInfo = new ApplicationInfoEx(ActivityUsage.this, usageData.uid);
					icon = appInfo.getIcon(ActivityUsage.this);
					system = appInfo.isSystem();
					hook = PrivacyManager.getHook(usageData.restrictionName, usageData.methodName);
					return holder;
				}
				return null;
			}

			@Override
			protected void onPostExecute(Object result) {
				if (holder.position == position && result != null) {
					if (system || (hook != null && hook.isDangerous()))
						holder.row.setBackgroundColor(getResources().getColor(getThemed(R.attr.color_dangerous)));
					else
						holder.row.setBackgroundColor(Color.TRANSPARENT);
					holder.imgIcon.setImageDrawable(icon);
					holder.imgIcon.setVisibility(View.VISIBLE);

					View.OnClickListener clickListener = new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							PRestriction usageData = mUsageAdapter.getItem(position);
							Intent intent = new Intent(ActivityUsage.this, ActivityApp.class);
							intent.putExtra(ActivityApp.cUid, usageData.uid);
							intent.putExtra(ActivityApp.cRestrictionName, usageData.restrictionName);
							intent.putExtra(ActivityApp.cMethodName, usageData.methodName);
							startActivity(intent);
						}
					};

					View.OnLongClickListener longClickListener = new View.OnLongClickListener() {
						@Override
						public boolean onLongClick(View view) {
							int userId = Util.getUserId(Process.myUid());
							final PRestriction usageData = mUsageAdapter.getItem(position);
							final Hook hook = PrivacyManager.getHook(usageData.restrictionName, usageData.methodName);

							boolean isApp = PrivacyManager.isApplication(usageData.uid);
							boolean odSystem = PrivacyManager.getSettingBool(userId,
									PrivacyManager.cSettingOnDemandSystem, false);
							final boolean wnomod = PrivacyManager.getSettingBool(usageData.uid,
									PrivacyManager.cSettingWhitelistNoModify, false);

							if ((isApp || odSystem) && hook != null && hook.whitelist() != null
									&& usageData.extra != null) {
								if (Util.hasProLicense(ActivityUsage.this) == null)
									Util.viewUri(ActivityUsage.this, ActivityMain.cProUri);
								else {
									AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivityUsage.this);
									alertDialogBuilder.setTitle(R.string.menu_whitelists);
									alertDialogBuilder.setMessage(usageData.restrictionName + "/"
											+ usageData.methodName + "(" + usageData.extra + ")");
									alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
									alertDialogBuilder.setPositiveButton(getString(R.string.title_deny),
											new DialogInterface.OnClickListener() {
												@Override
												public void onClick(DialogInterface dialog, int which) {
													// Deny
													PrivacyManager.setSetting(usageData.uid, hook.whitelist(),
															usageData.extra, Boolean.toString(false));
													if (!wnomod)
														PrivacyManager.updateState(usageData.uid);
												}
											});
									alertDialogBuilder.setNeutralButton(getString(R.string.title_allow),
											new DialogInterface.OnClickListener() {
												@Override
												public void onClick(DialogInterface dialog, int which) {
													// Allow
													PrivacyManager.setSetting(usageData.uid, hook.whitelist(),
															usageData.extra, Boolean.toString(true));
													if (!wnomod)
														PrivacyManager.updateState(usageData.uid);
												}
											});
									alertDialogBuilder.setNegativeButton(getString(android.R.string.cancel),
											new DialogInterface.OnClickListener() {
												@Override
												public void onClick(DialogInterface dialog, int which) {
												}
											});
									AlertDialog alertDialog = alertDialogBuilder.create();
									alertDialog.show();
								}
								return true;
							} else
								return false;
						}
					};

					holder.llUsage.setOnClickListener(clickListener);
					holder.tvRestriction.setOnClickListener(clickListener);
					holder.llUsage.setOnLongClickListener(longClickListener);
					holder.tvRestriction.setOnLongClickListener(longClickListener);
				}
			}
		}

		@Override
		@SuppressLint("InflateParams")
		public View getView(int position, View convertView, ViewGroup parent) {
			ViewHolder holder;
			if (convertView == null) {
				convertView = mInflater.inflate(R.layout.usageentry, null);
				holder = new ViewHolder(convertView, position);
				convertView.setTag(holder);
			} else {
				holder = (ViewHolder) convertView.getTag();
				holder.position = position;
			}

			// Get data
			PRestriction usageData = getItem(position);

			// Build entry
			holder.row.setBackgroundColor(Color.TRANSPARENT);

			Date date = new Date(usageData.time);
			SimpleDateFormat format = new SimpleDateFormat("HH:mm:ss", Locale.ROOT);
			holder.tvTime.setText(format.format(date));

			holder.imgIcon.setVisibility(View.INVISIBLE);
			holder.imgRestricted.setVisibility(usageData.restricted ? View.VISIBLE : View.INVISIBLE);
			holder.tvApp.setText(Integer.toString(usageData.uid));
			holder.tvRestriction.setText(String.format("%s/%s", usageData.restrictionName, usageData.methodName));

			if (!TextUtils.isEmpty(usageData.extra) && mHasProLicense) {
				holder.tvParameter.setText(usageData.extra);
				holder.tvParameter.setVisibility(View.VISIBLE);
			} else
				holder.tvParameter.setVisibility(View.GONE);

			if (usageData.value != null && mHasProLicense) {
				holder.tvValue.setText(getString(R.string.title_original) + ": " + usageData.value);
				holder.tvValue.setVisibility(View.VISIBLE);
			} else
				holder.tvValue.setVisibility(View.GONE);

			// Async update
			new HolderTask(position, holder, usageData).executeOnExecutor(mExecutor, (Object) null);

			return convertView;
		}
	}

	// Helpers

	private void updateTitle() {
		if (mUid == 0) {
			// Get statistics
			long count = 0;
			long restricted = 0;
			double persec = 0;
			try {
				@SuppressWarnings("rawtypes")
				Map statistics = PrivacyService.getClient().getStatistics();
				count = (Long) statistics.get("restriction_count");
				restricted = (Long) statistics.get("restriction_restricted");
				long uptime = (Long) statistics.get("uptime_milliseconds");
				persec = (double) count / (uptime / 1000);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

			// Set sub title
			getSupportActionBar().setSubtitle(String.format("%d/%d %.2f/s", restricted, count, persec));
		} else
			getSupportActionBar().setSubtitle(
					TextUtils.join(", ", new ApplicationInfoEx(this, mUid).getApplicationName()));
	}
}

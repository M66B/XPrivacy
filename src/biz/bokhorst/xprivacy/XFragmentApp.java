package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.SparseArray;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;

public class XFragmentApp extends Fragment {

	private AppListAdapter mAppAdapter;

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		View view = inflater.inflate(R.layout.xfragapp, container, false);

		// Start task to get app list
		AppListTask appListTask = new AppListTask(view);
		appListTask.execute();

		return view;
	}

	private class AppListTask extends AsyncTask<String, Integer, List<XApplicationInfo>> {

		private View mView;

		public AppListTask(View view) {
			mView = view;
		}

		@Override
		protected List<XApplicationInfo> doInBackground(String... params) {
			// Get argument
			boolean expert = XRestriction.getSetting(null, mView.getContext(), XRestriction.cExpertMode);

			// Get app list
			PackageManager pm = mView.getContext().getPackageManager();
			SparseArray<XApplicationInfo> mapApp = new SparseArray<XApplicationInfo>();
			List<XApplicationInfo> listApp = new ArrayList<XApplicationInfo>();
			for (ApplicationInfo appInfo : pm.getInstalledApplications(PackageManager.GET_META_DATA))
				if ((appInfo.uid == XRestriction.cUidAndroid ? expert : true)
						&& !appInfo.packageName.equals(XActivityRestriction.class.getPackage().getName())) {
					XApplicationInfo xAppInfo = mapApp.get(appInfo.uid);
					if (xAppInfo == null) {
						xAppInfo = new XApplicationInfo(appInfo, null, mView.getContext());
						mapApp.put(appInfo.uid, xAppInfo);
						listApp.add(xAppInfo);
					} else
						xAppInfo.AddApplicationName((String) pm.getApplicationLabel(appInfo));
				}
			Collections.sort(listApp);
			return listApp;
		}

		@Override
		protected void onPreExecute() {
			super.onPreExecute();

			// Show indeterminate progress circle
			ProgressBar progressBar = (ProgressBar) mView.findViewById(R.id.pbApp);
			progressBar.setVisibility(View.VISIBLE);
		}

		@Override
		protected void onPostExecute(List<XApplicationInfo> listApp) {
			super.onPostExecute(listApp);

			// Display app list
			mAppAdapter = new AppListAdapter(mView.getContext(), R.layout.xappentry, listApp);
			final ListView lvApp = (ListView) mView.findViewById(R.id.lvApp);
			lvApp.setAdapter(mAppAdapter);

			// Hide indeterminate progress circle
			ProgressBar progressBar = (ProgressBar) mView.findViewById(R.id.pbApp);
			progressBar.setVisibility(View.GONE);
		}
	}

	private class AppListAdapter extends ArrayAdapter<XApplicationInfo> {

		private Context mContext;

		public AppListAdapter(Context context, int resource, List<XApplicationInfo> objects) {
			super(context, resource, objects);
			mContext = context;
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.xappentry, parent, false);
			ImageView imgIcon = (ImageView) row.findViewById(R.id.imgAppEntryIcon);
			ImageView imgInternet = (ImageView) row.findViewById(R.id.imgAppEntryInternet);
			final TextView tvApp = (TextView) row.findViewById(R.id.tvAppEntryName);

			// Get entry
			final XApplicationInfo appEntry = getItem(position);

			// Set icon
			imgIcon.setImageDrawable(appEntry.getDrawable());

			// Check if internet access
			if (!appEntry.hasInternet())
				imgInternet.setVisibility(View.INVISIBLE);

			// Set title
			tvApp.setText(appEntry.toString());

			// Handle click
			tvApp.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					Intent intentSettings = new Intent(view.getContext(), XActivitySingleApp.class);
					intentSettings.putExtra(XActivitySingleApp.cPackageName, appEntry.getPackageName());
					intentSettings.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					view.getContext().startActivity(intentSettings);
				}
			});

			return row;
		}
	}
}
package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import android.content.ContentResolver;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XInstalledAppDetails extends XHook {

	// @formatter:off
	// @formatter:on

	private static Map<String, String[]> cPermissions = new LinkedHashMap<String, String[]>();

	static {
		cPermissions.put("calendar", new String[] { "READ_CALENDAR", "WRITE_CALENDAR" });
		cPermissions.put("contacts", new String[] { "READ_CONTACTS", "WRITE_CONTACTS" });
		cPermissions.put("location", new String[] { "ACCESS_FINE_LOCATION", "ACCESS_COARSE_LOCATION" });
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// TODO: usage statistics

		// Get app entry
		Field fieldAppEntry = findField(param.thisObject.getClass(), "mAppEntry");
		Object appEntry = fieldAppEntry.get(param.thisObject);
		if (appEntry == null) {
			warning("appEntry is null");
			return;
		}

		// Get app info
		Field fieldInfo = findField(appEntry.getClass(), "info");
		final ApplicationInfo appInfo = (ApplicationInfo) fieldInfo.get(appEntry);
		if (appInfo == null) {
			warning("appInfo is null");
			return;
		}

		// Get root view
		Field fieldRootView = findField(param.thisObject.getClass(), "mRootView");
		View rootView = (View) fieldRootView.get(param.thisObject);
		if (rootView == null) {
			warning("rootView is null");
			return;
		}

		// Get first child
		final LinearLayout detailsView = (LinearLayout) ((ViewGroup) rootView).getChildAt(0);
		if (detailsView == null) {
			warning("detailsView is null");
			return;
		}

		// Get package manager
		PackageManager pm = detailsView.getContext().getPackageManager();
		ContentResolver cr = detailsView.getContext().getContentResolver();

		// Get granted permissions
		List<String> listPermission = new ArrayList<String>();
		for (String permissionName : cPermissions.keySet()) {
			boolean permissionGranted = false;
			for (String aPermission : cPermissions.get(permissionName))
				if (pm.checkPermission("android.permission." + aPermission, appInfo.packageName) == PackageManager.PERMISSION_GRANTED) {
					permissionGranted = true;
					break;
				}
			if (permissionGranted)
				listPermission.add(permissionName);
		}

		// Remove existing privacy view
		LinearLayout privacyView = (LinearLayout) detailsView.findViewById(1966);
		if (privacyView != null)
			detailsView.removeView(privacyView);

		// Create privacy view
		privacyView = new LinearLayout(detailsView.getContext());
		privacyView.setId(1966);
		privacyView.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
				LinearLayout.LayoutParams.WRAP_CONTENT));
		privacyView.setOrientation(LinearLayout.VERTICAL);

		// Create privacy title
		TextView privacyTitle = new TextView(privacyView.getContext(), null, android.R.attr.listSeparatorTextViewStyle);
		LinearLayout.LayoutParams llpSeparator = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
				LinearLayout.LayoutParams.WRAP_CONTENT);
		llpSeparator.setMargins(0, 8, 0, 0);
		privacyTitle.setLayoutParams(llpSeparator);
		privacyTitle.setText("Privacy");
		privacyView.addView(privacyTitle);

		// Create internet title
		TextView internetTitle = new TextView(privacyView.getContext());
		internetTitle.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
				LinearLayout.LayoutParams.WRAP_CONTENT));
		boolean internetGranted = (pm.checkPermission("android.permission.INTERNET", appInfo.packageName) == PackageManager.PERMISSION_GRANTED);
		internetTitle.setText("has " + (internetGranted ? "" : "no ") + "internet access");
		privacyView.addView(internetTitle);

		// Create last usage list
		for (String permissionName : cPermissions.keySet()) {
			Cursor cursor = cr.query(XPrivacyProvider.URI_LASTUSE, null, null,
					new String[] { permissionName, Integer.toString(appInfo.uid) }, null);
			if (cursor.moveToNext()) {
				long lastUsage = cursor.getLong(cursor.getColumnIndex(XPrivacyProvider.COL_LASTUSE));
				cursor.close();
				TextView lastUsageTitle = new TextView(privacyView.getContext());
				lastUsageTitle.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
						LinearLayout.LayoutParams.WRAP_CONTENT));
				lastUsageTitle.setText(permissionName + ": " + (lastUsage == 0 ? "-" : new Date(lastUsage).toString()));
				if (!listPermission.contains(permissionName))
					lastUsageTitle.setTextColor(Color.GRAY);
				privacyView.addView(lastUsageTitle);
			}
		}

		// Create privacy list view
		ListView privacyListView = new ListView(privacyView.getContext());
		privacyListView.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
				LinearLayout.LayoutParams.WRAP_CONTENT));
		privacyListView.setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);

		// Create privacy list view adapter
		ArrayAdapter<String> privacyListAdapter = new ArrayAdapter<String>(privacyView.getContext(),
				android.R.layout.simple_list_item_multiple_choice, listPermission);
		privacyListView.setAdapter(privacyListAdapter);
		XUtil.setListViewHeightBasedOnChildren(privacyListView);

		// Set privacy values
		for (int position = 0; position < privacyListView.getAdapter().getCount(); position++) {
			String permissionName = (String) privacyListView.getItemAtPosition(position);
			privacyListView.setItemChecked(position,
					isAllowed(detailsView.getContext(), appInfo.uid, permissionName, false));
		}

		// Listen for privacy changes
		privacyListView.setOnItemClickListener(new OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				ListView privacyListView = (ListView) parent;
				String permissionName = (String) privacyListView.getItemAtPosition(position);
				boolean allowed = privacyListView.isItemChecked(position);
				setAllowed(detailsView.getContext(), appInfo.uid, permissionName, allowed);
			}
		});

		// Add privacy list
		privacyView.addView(privacyListView);

		// Add privacy view
		detailsView.addView(privacyView);
		detailsView.invalidate();
	}
}

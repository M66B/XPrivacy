package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;

import android.content.pm.ApplicationInfo;
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

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
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

		// Create privacy list view
		ListView privacyListView = new ListView(privacyView.getContext());
		privacyListView.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
				LinearLayout.LayoutParams.WRAP_CONTENT));
		privacyListView.setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);

		// Create privacy list view adapter
		ArrayAdapter<String> privacyListAdapter = new ArrayAdapter<String>(privacyView.getContext(),
				android.R.layout.simple_list_item_multiple_choice, XHook.cPermissionNames);
		privacyListView.setAdapter(privacyListAdapter);
		XUtil.setListViewHeightBasedOnChildren(privacyListView);

		// Set privacy values
		for (int position = 0; position < privacyListView.getAdapter().getCount(); position++) {
			String permissionName = (String) privacyListView.getItemAtPosition(position);
			privacyListView.setItemChecked(position, isAllowed(detailsView.getContext(), appInfo.uid, permissionName));
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

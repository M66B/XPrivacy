package biz.bokhorst.xprivacy;

import android.content.Context;
import android.util.Log;
import android.view.View;
import android.view.View.MeasureSpec;
import android.view.ViewGroup;
import android.widget.ListAdapter;
import android.widget.ListView;

public class XUtil {

	public static void log(XHook hook, int priority, String msg) {
		if (priority != Log.DEBUG)
			if (hook == null)
				Log.println(priority, "XPrivacy", msg);
			else
				Log.println(priority, String.format("XPrivacy/%s", hook.getClass().getSimpleName()), msg);
	}

	public static void bug(XHook hook, Throwable ex) {
		log(hook, Log.ERROR, ex.toString());
		ex.printStackTrace();
	}

	public static String getPackageName(Context context, int uid) {
		String[] packages = context.getPackageManager().getPackagesForUid(uid);
		if (packages != null && packages.length == 1)
			return packages[0];
		return Integer.toString(uid);
	}

	public static void setListViewHeightBasedOnChildren(ListView listView) {
		int height = 0;
		int width = MeasureSpec.makeMeasureSpec(listView.getWidth(), MeasureSpec.AT_MOST);
		ListAdapter listAdapter = listView.getAdapter();
		if (listAdapter != null)
			for (int position = 0; position < listAdapter.getCount(); position++) {
				View listItem = listAdapter.getView(position, null, listView);
				listItem.measure(width, MeasureSpec.UNSPECIFIED);
				height += listItem.getMeasuredHeight();
			}

		ViewGroup.LayoutParams params = listView.getLayoutParams();
		params.height = height + (listView.getDividerHeight() * (listAdapter.getCount() - 1));
		listView.setLayoutParams(params);
	}
}

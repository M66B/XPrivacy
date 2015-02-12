package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Spinner;
import android.widget.AdapterView.OnItemSelectedListener;

public class WhitelistTask extends AsyncTask<Object, Object, Object> {
	private int mUid;
	private String mType;
	private ActivityBase mContext;
	private WhitelistAdapter mWhitelistAdapter;
	private Map<String, TreeMap<String, Boolean>> mListWhitelist;

	public WhitelistTask(int uid, String type, ActivityBase context) {
		mUid = uid;
		mType = type;
		mContext = context;
	}

	@Override
	protected Object doInBackground(Object... params) {
		mListWhitelist = PrivacyManager.listWhitelisted(mUid, null);
		mWhitelistAdapter = new WhitelistAdapter(mContext, R.layout.whitelistentry, mUid, mListWhitelist);
		return null;
	}

	@Override
	@SuppressLint({ "DefaultLocale", "InflateParams" })
	protected void onPostExecute(Object result) {
		if (!mContext.isFinishing()) {
			// Build dialog
			AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(mContext);
			alertDialogBuilder.setTitle(R.string.menu_whitelists);
			alertDialogBuilder.setIcon(mContext.getThemed(R.attr.icon_launcher));

			if (mListWhitelist.keySet().size() > 0) {
				LayoutInflater inflater = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
				View llWhitelists = inflater.inflate(R.layout.whitelists, null);

				int index = 0;
				int selected = -1;
				final List<String> localizedType = new ArrayList<String>();
				for (String type : mListWhitelist.keySet()) {
					String name = "whitelist_" + type.toLowerCase().replace("/", "");
					int id = mContext.getResources().getIdentifier(name, "string", mContext.getPackageName());
					if (id == 0)
						localizedType.add(name);
					else
						localizedType.add(mContext.getResources().getString(id));

					if (type.equals(mType))
						selected = index;
					index++;
				}

				Spinner spWhitelistType = (Spinner) llWhitelists.findViewById(R.id.spWhitelistType);
				ArrayAdapter<String> whitelistTypeAdapter = new ArrayAdapter<String>(mContext,
						android.R.layout.simple_spinner_dropdown_item, localizedType);
				spWhitelistType.setAdapter(whitelistTypeAdapter);
				spWhitelistType.setOnItemSelectedListener(new OnItemSelectedListener() {
					@Override
					public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
						mWhitelistAdapter.setType(mListWhitelist.keySet().toArray(new String[0])[position]);
					}

					@Override
					public void onNothingSelected(AdapterView<?> view) {
					}
				});
				if (selected >= 0)
					spWhitelistType.setSelection(selected);

				ListView lvWhitelist = (ListView) llWhitelists.findViewById(R.id.lvWhitelist);
				lvWhitelist.setAdapter(mWhitelistAdapter);
				int position = spWhitelistType.getSelectedItemPosition();
				if (position != AdapterView.INVALID_POSITION)
					mWhitelistAdapter.setType(mListWhitelist.keySet().toArray(new String[0])[position]);

				alertDialogBuilder.setView(llWhitelists);
			}

			alertDialogBuilder.setPositiveButton(mContext.getString(R.string.msg_done),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							// Do nothing
						}
					});

			// Show dialog
			AlertDialog alertDialog = alertDialogBuilder.create();
			alertDialog.show();
		}

		super.onPostExecute(result);
	}
}

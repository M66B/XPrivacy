package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.ArrayAdapter;
import android.widget.CheckedTextView;
import android.widget.ImageView;

@SuppressLint("DefaultLocale")
public class WhitelistAdapter extends ArrayAdapter<String> {
	private String mSelectedType;
	private int mUid;
	private Map<String, TreeMap<String, Boolean>> mMapWhitelists;
	private LayoutInflater mInflater;

	public WhitelistAdapter(Context context, int resource, int uid, Map<String, TreeMap<String, Boolean>> mapWhitelists) {
		super(context, resource, new ArrayList<String>());
		mUid = uid;
		mMapWhitelists = mapWhitelists;
		mInflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
	}

	public void setType(String selectedType) {
		mSelectedType = selectedType;
		this.clear();
		if (mMapWhitelists.containsKey(selectedType))
			this.addAll(mMapWhitelists.get(selectedType).keySet());
	}

	private class ViewHolder {
		private View row;
		public CheckedTextView ctvName;
		public ImageView imgDelete;

		public ViewHolder(View theRow, int thePosition) {
			row = theRow;
			ctvName = (CheckedTextView) row.findViewById(R.id.cbName);
			imgDelete = (ImageView) row.findViewById(R.id.imgDelete);
		}
	}

	@Override
	@SuppressLint("InflateParams")
	public View getView(int position, View convertView, ViewGroup parent) {
		final ViewHolder holder;
		if (convertView == null) {
			convertView = mInflater.inflate(R.layout.whitelistentry, null);
			holder = new ViewHolder(convertView, position);
			convertView.setTag(holder);
		} else
			holder = (ViewHolder) convertView.getTag();

		// Set data
		final String name = this.getItem(position);
		holder.ctvName.setText(name);
		holder.ctvName.setChecked(mMapWhitelists.get(mSelectedType).get(name));

		final boolean wnomod = PrivacyManager.getSettingBool(mUid, PrivacyManager.cSettingWhitelistNoModify, false);

		holder.ctvName.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View view) {
				// Toggle white/black list entry
				holder.ctvName.toggle();
				boolean isChecked = holder.ctvName.isChecked();
				mMapWhitelists.get(mSelectedType).put(name, isChecked);
				PrivacyManager.setSetting(mUid, mSelectedType, name, Boolean.toString(isChecked));
				if (!wnomod)
					PrivacyManager.updateState(mUid);
			}
		});

		holder.imgDelete.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View view) {
				// Delete white/black list entry
				WhitelistAdapter.this.remove(name);
				mMapWhitelists.get(mSelectedType).remove(name);
				PrivacyManager.setSetting(mUid, mSelectedType, name, null);
				if (!wnomod)
					PrivacyManager.updateState(mUid);
			}
		});

		return convertView;
	}
}

package biz.bokhorst.xprivacy;

import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

public class XFragmentRestriction extends Fragment {

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		View view = inflater.inflate(R.layout.xfragrestriction, container, false);

		// Fill restriction list view adapter
		final List<String> listRestriction = XRestriction.getRestrictions(view.getContext());
		final ListView lvRestriction = (ListView) view.findViewById(R.id.lvRestriction);
		RestrictionAdapter restrictionAdapter = new RestrictionAdapter(view.getContext(),
				android.R.layout.simple_list_item_1, listRestriction);
		lvRestriction.setAdapter(restrictionAdapter);

		// Click: batch edit
		lvRestriction.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

				String restrictionName = listRestriction.get(position);
				Intent intentBatch = new Intent(view.getContext(), XActivityRestriction.class);
				intentBatch.putExtra(XActivityRestriction.cRestrictionName, restrictionName);
				intentBatch.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
				startActivity(intentBatch);
			}
		});

		return view;
	}

	private class RestrictionAdapter extends ArrayAdapter<String> {

		public RestrictionAdapter(Context context, int resource, List<String> objects) {
			super(context, resource, objects);
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			View row = convertView;
			if (row == null) {
				LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(
						Context.LAYOUT_INFLATER_SERVICE);
				row = inflater.inflate(android.R.layout.simple_list_item_1, parent, false);
			}
			TextView tvRestriction = (TextView) row.findViewById(android.R.id.text1);

			// Get entry
			String restrictionName = getItem(position);

			// Display localize name
			tvRestriction.setText(XRestriction.getLocalizedName(row.getContext(), restrictionName));

			return row;
		}
	}
}
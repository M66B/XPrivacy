package biz.bokhorst.xprivacy;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.os.Bundle;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

public class XMain extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xmain);

		// Show version
		try {
			PackageInfo pInfo = getBaseContext().getPackageManager().getPackageInfo(getPackageName(), 0);
			TextView tvVersion = (TextView) findViewById(R.id.tvVersion);
			tvVersion.setText(String.format(getString(R.string.app_version), pInfo.versionName, pInfo.versionCode));
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
		}

		// Check Xposed version
		String processVersion = getInstalledAppProcessVersion(null);
		String bridgeVersion = getJarInstalledVersion(null);
		if (processVersion == null || bridgeVersion == null || Integer.parseInt(processVersion) < 35) {
			AlertDialog alertDialog = new AlertDialog.Builder(this).create();
			alertDialog.setTitle(getString(R.string.app_name));
			alertDialog.setMessage(getString(R.string.app_notxposed));
			alertDialog.setIcon(R.drawable.ic_launcher);
			alertDialog.setButton(AlertDialog.BUTTON_POSITIVE, "OK", new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int which) {
				}
			});
			alertDialog.show();
		}

		// Show Xposed version
		TextView tvXVersion = (TextView) findViewById(R.id.tvXVersion);
		tvXVersion.setText(String.format(getString(R.string.app_xversion), processVersion, bridgeVersion));

		// Fill permission list view adapter
		final List<String> listPermission = new ArrayList<String>(XPermissions.cPermissions.keySet());
		final ListView lvPermission = (ListView) findViewById(R.id.lvPermission);
		PermissionAdapter permissionAdapter = new PermissionAdapter(getBaseContext(),
				android.R.layout.simple_list_item_1, listPermission);
		lvPermission.setAdapter(permissionAdapter);

		// Listen for privacy changes
		lvPermission.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

				String permissionName = listPermission.get(position);
				Intent intentBatch = new Intent(getBaseContext(), XBatchEdit.class);
				intentBatch.putExtra(XBatchEdit.cExtraPermissionName, permissionName);
				intentBatch.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
				startActivity(intentBatch);
			}
		});

	}

	private class PermissionAdapter extends ArrayAdapter<String> {

		public PermissionAdapter(Context context, int resource, List<String> objects) {
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
			TextView tvPermission = (TextView) row.findViewById(android.R.id.text1);

			// Display localize name
			String permissionName = getItem(position);
			tvPermission.setText(XPermissions.getLocalizedName(getBaseContext(), permissionName));

			return row;
		}
	}

	private String getInstalledAppProcessVersion(String defaultValue) {
		try {
			return getAppProcessVersion(new FileInputStream("/system/bin/app_process"), defaultValue);
		} catch (IOException e) {
			return defaultValue;
		}
	}

	private static Pattern PATTERN_APP_PROCESS_VERSION = Pattern.compile(".*with Xposed support \\(version (.+)\\).*");

	private String getAppProcessVersion(InputStream is, String defaultValue) throws IOException {

		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		String line;
		while ((line = br.readLine()) != null) {
			if (!line.contains("Xposed"))
				continue;
			Matcher m = PATTERN_APP_PROCESS_VERSION.matcher(line);
			if (m.find()) {
				is.close();
				return m.group(1);
			}
		}
		is.close();
		return defaultValue;
	}

	public static String getJarInstalledVersion(String defaultValue) {
		try {
			if (new File("/data/xposed/XposedBridge.jar.newversion").exists())
				return getJarVersion(new FileInputStream("/data/xposed/XposedBridge.jar.newversion"), defaultValue);
			else
				return getJarVersion(new FileInputStream("/data/xposed/XposedBridge.jar"), defaultValue);
		} catch (IOException e) {
			return defaultValue;
		}
	}

	public static String getJarVersion(InputStream is, String defaultValue) throws IOException {
		JarInputStream jis = new JarInputStream(is);
		JarEntry entry;
		try {
			while ((entry = jis.getNextJarEntry()) != null) {
				if (!entry.getName().equals("assets/VERSION"))
					continue;

				BufferedReader br = new BufferedReader(new InputStreamReader(jis));
				String version = br.readLine();
				is.close();
				br.close();
				return version;
			}
		} finally {
			try {
				jis.close();
			} catch (Exception e) {
			}
		}
		return defaultValue;
	}
}

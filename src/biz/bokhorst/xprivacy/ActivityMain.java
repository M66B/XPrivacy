package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.R;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.net.InterfaceAddress;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.json.JSONArray;
import org.json.JSONObject;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xmlpull.v1.XmlSerializer;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.database.Cursor;
import android.graphics.Color;
import android.graphics.Typeface;
import android.net.Uri;
import android.net.wifi.WifiInfo;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.util.Xml;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CheckedTextView;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;

public class ActivityMain extends Activity implements OnItemSelectedListener {

	private Spinner spRestriction = null;
	private AppListAdapter mAppAdapter = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Set layout
		setContentView(R.layout.xmainlist);

		// Get localized restriction name
		List<String> listRestriction = XRestriction.getRestrictions(this);
		List<String> listLocalizedRestriction = new ArrayList<String>();
		for (String restrictionName : listRestriction)
			listLocalizedRestriction.add(XRestriction.getLocalizedName(this, restrictionName));
		Collections.sort(listLocalizedRestriction);

		// Build spinner adapter
		ArrayAdapter<String> spAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item);
		spAdapter.addAll(listLocalizedRestriction);

		// Setup search
		final EditText etFilter = (EditText) findViewById(R.id.etFilter);
		etFilter.addTextChangedListener(new TextWatcher() {
			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
				if (mAppAdapter != null)
					mAppAdapter.getFilter().filter(etFilter.getText().toString());
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {
			}

			@Override
			public void afterTextChanged(Editable s) {
			}
		});

		// Setup spinner
		spRestriction = (Spinner) findViewById(R.id.spRestriction);
		spRestriction.setAdapter(spAdapter);
		spRestriction.setOnItemSelectedListener(this);
		spRestriction.setEnabled(false);

		// Handle help
		ImageView ivHelp = (ImageView) findViewById(R.id.ivHelp);
		ivHelp.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				Dialog dialog = new Dialog(ActivityMain.this);
				dialog.requestWindowFeature(Window.FEATURE_LEFT_ICON);
				dialog.setTitle(getString(R.string.help_application));
				dialog.setContentView(R.layout.xhelp);
				dialog.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, R.drawable.ic_launcher);
				dialog.setCancelable(true);
				dialog.show();
			}
		});

		// Start task to get app list
		AppListTask appListTask = new AppListTask();
		appListTask.execute((String) spRestriction.getSelectedItem());

		// Check environment
		checkRequirements();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.xmain, menu);
		if (XUtil.isProVersion(this) == null) {
			menu.removeItem(R.id.menu_export);
			menu.removeItem(R.id.menu_import);
		} else
			menu.removeItem(R.id.menu_pro);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		try {
			switch (item.getItemId()) {
			case R.id.menu_settings:
				optionSettings();
				return true;
			case R.id.menu_update:
				optionCheckUpdate();
				return true;
			case R.id.menu_report:
				optionReportIssue();
				return true;
			case R.id.menu_export:
				optionExport();
				return true;
			case R.id.menu_import:
				optionImport();
				return true;
			case R.id.menu_pro:
				optionPro();
				return true;
			case R.id.menu_about:
				optionAbout();
				return true;
			default:
				return super.onOptionsItemSelected(item);
			}
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
			return true;
		}
	}

	@Override
	public void onItemSelected(AdapterView<?> parent, View view, int pos, long id) {
		if (mAppAdapter != null) {
			String restrictionName = XRestriction.getRestrictions(getApplicationContext()).get(pos);
			mAppAdapter.setRestrictionName(restrictionName);
		}
	}

	@Override
	public void onNothingSelected(AdapterView<?> parent) {
		if (mAppAdapter != null)
			mAppAdapter.setRestrictionName(null);
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mAppAdapter != null)
			mAppAdapter.notifyDataSetChanged();
	}

	private void checkRequirements() {
		// Check Android version
		if (Build.VERSION.SDK_INT != Build.VERSION_CODES.JELLY_BEAN
				&& Build.VERSION.SDK_INT != Build.VERSION_CODES.JELLY_BEAN_MR1) {
			AlertDialog alertDialog = new AlertDialog.Builder(this).create();
			alertDialog.setTitle(getString(R.string.app_name));
			alertDialog.setMessage(getString(R.string.app_wrongandroid));
			alertDialog.setIcon(R.drawable.ic_launcher);
			alertDialog.setButton(AlertDialog.BUTTON_POSITIVE, "OK", new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int which) {
				}
			});
			alertDialog.show();
		}

		// Check Xposed version
		int xVersion = XUtil.getXposedVersion();
		if (xVersion < XRestriction.cXposedMinVersion) {
			AlertDialog alertDialog = new AlertDialog.Builder(this).create();
			alertDialog.setTitle(getString(R.string.app_name));
			alertDialog.setMessage(String.format(getString(R.string.app_notxposed), XRestriction.cXposedMinVersion));
			alertDialog.setIcon(R.drawable.ic_launcher);
			alertDialog.setButton(AlertDialog.BUTTON_POSITIVE, "OK", new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int which) {
				}
			});
			alertDialog.show();
		}

		// Check if XPrivacy is enabled
		if (!XUtil.isXposedEnabled()) {
			AlertDialog alertDialog = new AlertDialog.Builder(this).create();
			alertDialog.setTitle(getString(R.string.app_name));
			alertDialog.setMessage(getString(R.string.app_notenabled));
			alertDialog.setIcon(R.drawable.ic_launcher);
			alertDialog.setButton(AlertDialog.BUTTON_POSITIVE, "OK", new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int which) {
				}
			});
			alertDialog.show();
		}

		// Check location manager
		if (!checkField(getSystemService(Context.LOCATION_SERVICE), "mContext", Context.class)) {
			String msg = "Incompatible location manager";
			Toast toast = Toast.makeText(this, msg, Toast.LENGTH_LONG);
			toast.show();
		}

		// Check package manager
		if (!checkField(getPackageManager(), "mContext", Context.class)) {
			String msg = "Incompatible package manager";
			Toast toast = Toast.makeText(this, msg, Toast.LENGTH_LONG);
			toast.show();
		}

		// TODO: PackageManagerService.getPackageUid
		// Unfortunately the PMS class cannot be loaded by the app class loader

		// Check content resolver
		if (!checkField(getContentResolver(), "mContext", Context.class)) {
			String msg = "Incompatible content resolver";
			Toast toast = Toast.makeText(this, msg, Toast.LENGTH_LONG);
			toast.show();
		}

		// Check telephony manager
		if (!checkField(getSystemService(Context.TELEPHONY_SERVICE), "sContext", Context.class)) {
			String msg = "Incompatible telephony manager";
			Toast toast = Toast.makeText(this, msg, Toast.LENGTH_LONG);
			toast.show();
		}

		// WifiInfo
		if (!checkField(WifiInfo.class, "mBSSID") || !checkField(WifiInfo.class, "mIpAddress")
				|| !checkField(WifiInfo.class, "mMacAddress")
				|| !(checkField(WifiInfo.class, "mSSID") || checkField(WifiInfo.class, "mWifiSsid"))) {
			String msg = "Incompatible WifiInfo";
			Toast toast = Toast.makeText(this, msg, Toast.LENGTH_LONG);
			toast.show();
		}

		// InterfaceAddress
		if (!checkField(InterfaceAddress.class, "address") || !checkField(InterfaceAddress.class, "broadcastAddress")
				|| XNetworkInterface.getInetAddressEmpty() == null) {
			String msg = "Incompatible InterfaceAddress";
			Toast toast = Toast.makeText(this, msg, Toast.LENGTH_LONG);
			toast.show();
		}
	}

	private boolean checkField(Object obj, String fieldName, Class<?> expectedClass) {
		try {
			// Find field
			Field field = null;
			Class<?> superClass = (obj == null ? null : obj.getClass());
			while (superClass != null)
				try {
					field = superClass.getDeclaredField(fieldName);
					field.setAccessible(true);
					break;
				} catch (Throwable ex) {
					superClass = superClass.getSuperclass();
				}

			// Check field
			if (field != null) {
				Object value = field.get(obj);
				if (value != null && expectedClass.isAssignableFrom(value.getClass()))
					return true;
			}
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
		}
		return false;
	}

	private boolean checkField(Class<?> clazz, String fieldName) {
		try {
			clazz.getDeclaredField(fieldName);
			return true;
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
			return false;
		}
	}

	private void optionSettings() {
		// Build dialog
		final Dialog dlgSettings = new Dialog(this);
		dlgSettings.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgSettings.setTitle(getString(R.string.app_name));
		dlgSettings.setContentView(R.layout.xsettings);
		dlgSettings.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, R.drawable.ic_launcher);

		// Reference controls
		final CheckBox cbSettings = (CheckBox) dlgSettings.findViewById(R.id.cbExpert);
		final EditText atLat = (EditText) dlgSettings.findViewById(R.id.etLat);
		final EditText atLon = (EditText) dlgSettings.findViewById(R.id.etLon);
		Button btnOk = (Button) dlgSettings.findViewById(R.id.btnOk);

		// Set current values
		String sExpert = XRestriction.getSetting(null, ActivityMain.this, XRestriction.cSettingExpert,
				Boolean.FALSE.toString());
		final boolean expert = Boolean.parseBoolean(sExpert);
		cbSettings.setChecked(expert);
		atLat.setText(XRestriction.getSetting(null, ActivityMain.this, XRestriction.cSettingLatitude, ""));
		atLon.setText(XRestriction.getSetting(null, ActivityMain.this, XRestriction.cSettingLongitude, ""));

		// Wait for OK
		btnOk.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				// Set expert mode
				XRestriction.setSetting(null, ActivityMain.this, XRestriction.cSettingExpert,
						Boolean.toString(cbSettings.isChecked()));
				if (expert != cbSettings.isChecked()) {
					// Start task to get app list
					AppListTask appListTask = new AppListTask();
					appListTask.execute((String) spRestriction.getSelectedItem());
				}

				// Set location
				try {
					float lat = Float.parseFloat(atLat.getText().toString().replace(',', '.'));
					float lon = Float.parseFloat(atLon.getText().toString().replace(',', '.'));
					if (lat < -90 || lat > 90 || lon < -180 || lon > 180)
						throw new InvalidParameterException();

					XRestriction.setSetting(null, ActivityMain.this, XRestriction.cSettingLatitude, Float.toString(lat));
					XRestriction.setSetting(null, ActivityMain.this, XRestriction.cSettingLongitude,
							Float.toString(lon));

				} catch (Throwable ex) {
					XRestriction.setSetting(null, ActivityMain.this, XRestriction.cSettingLatitude, "");
					XRestriction.setSetting(null, ActivityMain.this, XRestriction.cSettingLongitude, "");
				}

				// Done
				dlgSettings.dismiss();
			}
		});

		dlgSettings.setCancelable(true);
		dlgSettings.show();
	}

	private void optionCheckUpdate() {
		new UpdateTask().execute("http://goo.im/json2&path=/devs/M66B/xprivacy");
	}

	private void optionReportIssue() {
		// Report issue
		Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse("https://github.com/M66B/XPrivacy/issues"));
		startActivity(browserIntent);
	}

	private void optionPro() {
		Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse("http://www.faircode.eu/xprivacy/"));
		startActivity(browserIntent);
	}

	private void optionExport() {
		if (Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState()))
			try {
				// Serialize
				FileOutputStream fos = new FileOutputStream(getExportFile());
				XmlSerializer serializer = Xml.newSerializer();
				serializer.setOutput(fos, "UTF-8");
				serializer.startDocument(null, Boolean.valueOf(true));
				serializer.setFeature("http://xmlpull.org/v1/doc/features.html#indent-output", true);
				serializer.startTag(null, "XPrivacy");

				// Process restrictions
				Cursor sCursor = getContentResolver().query(XPrivacyProvider.URI_SETTING, null, null, null, null);
				while (sCursor.moveToNext()) {
					// Get setting
					String setting = sCursor.getString(sCursor.getColumnIndex(XPrivacyProvider.COL_SETTING));
					String value = sCursor.getString(sCursor.getColumnIndex(XPrivacyProvider.COL_VALUE));

					// Serialize setting
					serializer.startTag(null, "Setting");
					serializer.attribute(null, "Name", setting);
					serializer.attribute(null, "Value", value);
					serializer.endTag(null, "Setting");
				}
				sCursor.close();

				// Process restrictions
				Cursor rCursor = getContentResolver().query(XPrivacyProvider.URI_RESTRICTION, null, null,
						new String[] { Integer.toString(0), Boolean.toString(false) }, null);
				while (rCursor.moveToNext()) {
					// Decode uid
					int uid = rCursor.getInt(rCursor.getColumnIndex(XPrivacyProvider.COL_UID));
					boolean restricted = Boolean.parseBoolean(rCursor.getString(rCursor
							.getColumnIndex(XPrivacyProvider.COL_RESTRICTED)));
					if (restricted) {
						String[] packages = getPackageManager().getPackagesForUid(uid);
						if (packages == null)
							XUtil.log(null, Log.WARN, "No packages for uid=" + uid);
						else
							for (String packageName : packages) {
								// Package
								serializer.startTag(null, "Package");

								// Package name
								serializer.attribute(null, "Name", packageName);

								// Restriction name
								String restrictionName = rCursor.getString(rCursor
										.getColumnIndex(XPrivacyProvider.COL_RESTRICTION));
								serializer.attribute(null, "Restriction", restrictionName);

								serializer.endTag(null, "Package");
							}
					}
				}
				rCursor.close();

				// End serialization
				serializer.endTag(null, "XPrivacy");
				serializer.endDocument();
				serializer.flush();
				fos.close();

				// Display message
				String message = String.format("%s %s", getString(R.string.menu_export), getExportFile()
						.getAbsolutePath());
				Toast toast = Toast.makeText(this, message, Toast.LENGTH_LONG);
				toast.show();
			} catch (Throwable ex) {
				XUtil.bug(null, ex);
				Toast toast = Toast.makeText(this, ex.toString(), Toast.LENGTH_LONG);
				toast.show();
			}
	}

	private void optionImport() {
		if (Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState()))
			try {
				// Read XML
				FileInputStream fis = new FileInputStream(getExportFile());
				InputStreamReader isr = new InputStreamReader(fis);
				char[] inputBuffer = new char[fis.available()];
				isr.read(inputBuffer);
				String xml = new String(inputBuffer);
				isr.close();
				fis.close();

				// Prepare XML document
				InputStream is = new ByteArrayInputStream(xml.getBytes("UTF-8"));
				DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
				DocumentBuilder db = dbf.newDocumentBuilder();
				Document dom = db.parse(is);
				dom.getDocumentElement().normalize();

				// Process settings
				NodeList sItems = dom.getElementsByTagName("Setting");
				for (int i = 0; i < sItems.getLength(); i++) {
					// Process package restriction
					Node entry = sItems.item(i);
					NamedNodeMap attrs = entry.getAttributes();
					String setting = attrs.getNamedItem("Name").getNodeValue();
					String value = attrs.getNamedItem("Value").getNodeValue();
					XRestriction.setSetting(null, this, setting, value);
				}

				// Process restrictions
				Map<String, List<String>> mapPackage = new HashMap<String, List<String>>();
				NodeList rItems = dom.getElementsByTagName("Package");
				for (int i = 0; i < rItems.getLength(); i++) {
					// Process package restriction
					Node entry = rItems.item(i);
					NamedNodeMap attrs = entry.getAttributes();
					String packageName = attrs.getNamedItem("Name").getNodeValue();
					String restrictionName = attrs.getNamedItem("Restriction").getNodeValue();

					// Map package restriction
					if (!mapPackage.containsKey(packageName))
						mapPackage.put(packageName, new ArrayList<String>());
					mapPackage.get(packageName).add(restrictionName);
				}

				// Process result
				for (String packageName : mapPackage.keySet()) {
					try {
						// Get uid
						int uid = getPackageManager().getPackageInfo(packageName, 0).applicationInfo.uid;

						// Reset existing restrictions
						for (String restrictionName : XRestriction.getRestrictions(this))
							XRestriction.setRestricted(null, this, uid, restrictionName, null, false);

						// Set imported restrictions
						for (String restrictionName : mapPackage.get(packageName))
							XRestriction.setRestricted(null, this, uid, restrictionName, null, true);
					} catch (NameNotFoundException ex) {
						XUtil.log(null, Log.WARN, "Not found package=" + packageName);
					}
				}

				// Display message
				String message = String.format("%s %s", getString(R.string.menu_import), getExportFile()
						.getAbsolutePath());
				Toast toast = Toast.makeText(this, message, Toast.LENGTH_LONG);
				toast.show();
			} catch (Throwable ex) {
				XUtil.bug(null, ex);
				Toast toast = Toast.makeText(this, ex.toString(), Toast.LENGTH_LONG);
				toast.show();
			}
	}

	private void optionAbout() {
		// About
		Dialog dlgAbout = new Dialog(this);
		dlgAbout.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgAbout.setTitle(getString(R.string.app_name));
		dlgAbout.setContentView(R.layout.xabout);
		dlgAbout.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, R.drawable.ic_launcher);

		// Show version
		try {
			PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
			TextView tvVersion = (TextView) dlgAbout.findViewById(R.id.tvVersion);
			tvVersion.setText(String.format(getString(R.string.app_version), pInfo.versionName, pInfo.versionCode));
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
		}

		// Show Xposed version
		int xVersion = XUtil.getXposedVersion();
		TextView tvXVersion = (TextView) dlgAbout.findViewById(R.id.tvXVersion);
		tvXVersion.setText(String.format(getString(R.string.app_xversion), xVersion));

		// Show license
		String licensed = XUtil.isProVersion(this);
		TextView tvLicensed = (TextView) dlgAbout.findViewById(R.id.tvLicensed);
		if (licensed == null)
			tvLicensed.setVisibility(View.GONE);
		else
			tvLicensed.setText(String.format(getString(R.string.msg_licensed), licensed));

		dlgAbout.setCancelable(true);
		dlgAbout.show();
	}

	private File getExportFile() {
		String folder = Environment.getExternalStorageDirectory().getAbsolutePath();
		String fileName = folder + File.separator + "XPrivacy.xml";
		return new File(fileName);
	}

	private String fetchJson(String... uri) {
		try {
			// Request downloads
			HttpClient httpclient = new DefaultHttpClient();
			HttpResponse response = httpclient.execute(new HttpGet(uri[0]));
			StatusLine statusLine = response.getStatusLine();

			if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
				// Succeeded
				ByteArrayOutputStream out = new ByteArrayOutputStream();
				response.getEntity().writeTo(out);
				out.close();
				return out.toString("ISO-8859-1");
			} else {
				// Failed
				response.getEntity().getContent().close();
				throw new IOException(statusLine.getReasonPhrase());
			}
		} catch (Throwable ex) {
			XUtil.bug(null, ex);
			return ex.toString();
		}
	}

	private void processJson(String json) {
		try {
			// Parse result
			String version = null;
			String url = null;
			if (json != null)
				if (json.startsWith("{")) {
					long newest = 0;
					String prefix = "XPrivacy_";
					JSONObject jRoot = new JSONObject(json);
					JSONArray jArray = jRoot.getJSONArray("list");
					for (int i = 0; jArray != null && i < jArray.length(); i++) {
						// File
						JSONObject jEntry = jArray.getJSONObject(i);
						String filename = jEntry.getString("filename");
						if (filename.startsWith(prefix)) {
							// Check if newer
							long modified = jEntry.getLong("modified");
							if (modified > newest) {
								newest = modified;
								version = filename.substring(prefix.length()).replace(".apk", "");
								url = "http://goo.im" + jEntry.getString("path");
							}
						}
					}
				} else {
					Toast toast = Toast.makeText(ActivityMain.this, json, Toast.LENGTH_LONG);
					toast.show();
				}

			if (url == null || version == null) {
				// Assume no update
				String msg = getString(R.string.msg_noupdate);
				Toast toast = Toast.makeText(ActivityMain.this, msg, Toast.LENGTH_LONG);
				toast.show();
			} else {
				// Compare versions
				PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
				XVersion ourVersion = new XVersion(pInfo.versionName);
				XVersion latestVersion = new XVersion(version);
				if (ourVersion.compareTo(latestVersion) < 0) {
					// Update available
					Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
					startActivity(browserIntent);
				} else {
					// No update available
					String msg = getString(R.string.msg_noupdate);
					Toast toast = Toast.makeText(ActivityMain.this, msg, Toast.LENGTH_LONG);
					toast.show();
				}
			}
		} catch (Throwable ex) {
			Toast toast = Toast.makeText(ActivityMain.this, ex.toString(), Toast.LENGTH_LONG);
			toast.show();
			XUtil.bug(null, ex);
		}
	}

	private class UpdateTask extends AsyncTask<String, String, String> {

		@Override
		protected String doInBackground(String... uri) {
			return fetchJson(uri);
		}

		@Override
		protected void onPostExecute(String json) {
			super.onPostExecute(json);
			if (json != null)
				processJson(json);
		}
	}

	private class AppListTask extends AsyncTask<String, Integer, List<XApplicationInfo>> {

		private String mRestrictionName;

		@Override
		protected List<XApplicationInfo> doInBackground(String... params) {
			mRestrictionName = params[0];
			return XApplicationInfo.getXApplicationList(ActivityMain.this);
		}

		@Override
		protected void onPreExecute() {
			super.onPreExecute();

			// Show indeterminate progress circle
			ListView lvApp = (ListView) findViewById(R.id.lvApp);
			ProgressBar progressBar = (ProgressBar) findViewById(R.id.pbApp);
			progressBar.setVisibility(View.VISIBLE);
			lvApp.setVisibility(View.GONE);
		}

		@Override
		protected void onPostExecute(List<XApplicationInfo> listApp) {
			super.onPostExecute(listApp);

			// Display app list
			mAppAdapter = new AppListAdapter(ActivityMain.this, R.layout.xmainentry, listApp, mRestrictionName);
			ListView lvApp = (ListView) findViewById(R.id.lvApp);
			lvApp.setAdapter(mAppAdapter);

			// Hide indeterminate progress circle
			ProgressBar progressBar = (ProgressBar) findViewById(R.id.pbApp);
			progressBar.setVisibility(View.GONE);
			lvApp.setVisibility(View.VISIBLE);

			// Enable spinner
			Spinner spRestriction = (Spinner) findViewById(R.id.spRestriction);
			spRestriction.setEnabled(true);
		}
	}

	private class AppListAdapter extends ArrayAdapter<XApplicationInfo> {

		private String mRestrictionName;

		public AppListAdapter(Context context, int resource, List<XApplicationInfo> objects,
				String initialRestrictionName) {
			super(context, resource, objects);
			mRestrictionName = initialRestrictionName;
		}

		public void setRestrictionName(String restrictionName) {
			mRestrictionName = restrictionName;
			notifyDataSetChanged();
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			View row = inflater.inflate(R.layout.xmainentry, parent, false);
			ImageView imgIcon = (ImageView) row.findViewById(R.id.imgIcon);
			ImageView imgInternet = (ImageView) row.findViewById(R.id.imgInternet);
			ImageView imgUsed = (ImageView) row.findViewById(R.id.imgUsed);
			final CheckedTextView ctvApp = (CheckedTextView) row.findViewById(R.id.ctvName);

			// Get entry
			final XApplicationInfo xAppInfo = getItem(position);

			// Set background color
			if (xAppInfo.getIsSystem())
				row.setBackgroundColor(Color.parseColor("#FFFDD0"));

			// Set icon
			imgIcon.setImageDrawable(xAppInfo.getDrawable());
			imgIcon.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					Intent intentSettings = new Intent(view.getContext(), ActivityApp.class);
					intentSettings.putExtra(ActivityApp.cPackageName, xAppInfo.getPackageName());
					intentSettings.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					view.getContext().startActivity(intentSettings);
				}
			});

			// Set title
			ctvApp.setText(xAppInfo.toString());

			// Check if internet access
			imgInternet.setVisibility(xAppInfo.hasInternet() ? View.VISIBLE : View.INVISIBLE);

			// Check if used
			boolean used = XRestriction.isUsed(row.getContext(), xAppInfo.getUid(), mRestrictionName, null);
			ctvApp.setTypeface(null, used ? Typeface.BOLD_ITALIC : Typeface.NORMAL);
			imgUsed.setVisibility(used ? View.VISIBLE : View.INVISIBLE);

			// Display restriction
			boolean restricted = XRestriction.getRestricted(null, row.getContext(), xAppInfo.getUid(),
					mRestrictionName, null, false, false);
			ctvApp.setChecked(restricted);

			// Listen for restriction changes
			ctvApp.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					boolean restricted = XRestriction.getRestricted(null, view.getContext(), xAppInfo.getUid(),
							mRestrictionName, null, false, false);
					restricted = !restricted;
					ctvApp.setChecked(restricted);
					XRestriction.setRestricted(null, view.getContext(), xAppInfo.getUid(), mRestrictionName, null,
							restricted);
				}
			});

			row.refreshDrawableState();
			return row;
		}
	}
}

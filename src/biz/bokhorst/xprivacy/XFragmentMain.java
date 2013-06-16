package biz.bokhorst.xprivacy;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

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

import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.database.Cursor;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.util.Xml;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.Window;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.TextView;
import android.widget.Toast;

public class XFragmentMain extends FragmentActivity {

	private final static int cXposedMinVersion = 34;

	private PagerAdapter mPageAdapter;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Set layout
		setContentView(R.layout.xmain);

		// Setup pager
		List<Fragment> listFragment = new ArrayList<Fragment>();
		listFragment.add(new XFragmentApp());
		listFragment.add(new XFragmentRestriction());
		mPageAdapter = new PagerAdapter(getSupportFragmentManager(), listFragment);
		ViewPager viewPager = (ViewPager) findViewById(R.id.viewpager);
		viewPager.setAdapter(mPageAdapter);

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
		if (xVersion < cXposedMinVersion) {
			AlertDialog alertDialog = new AlertDialog.Builder(this).create();
			alertDialog.setTitle(getString(R.string.app_name));
			alertDialog.setMessage(String.format(getString(R.string.app_notxposed), cXposedMinVersion));
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
			alertDialog.setMessage(String.format(getString(R.string.app_notenabled), cXposedMinVersion));
			alertDialog.setIcon(R.drawable.ic_launcher);
			alertDialog.setButton(AlertDialog.BUTTON_POSITIVE, "OK", new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int which) {
				}
			});
			alertDialog.show();
		}
	}

	private class PagerAdapter extends FragmentPagerAdapter {
		private List<Fragment> mListfragment;

		public PagerAdapter(FragmentManager fm, List<Fragment> fragments) {
			super(fm);
			mListfragment = fragments;
		}

		@Override
		public Fragment getItem(int position) {
			return mListfragment.get(position);
		}

		@Override
		public int getCount() {
			return mListfragment.size();
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.xmain, menu);
		if (XUtil.isProVersion(this))
			menu.removeItem(R.id.menu_pro);
		else {
			menu.removeItem(R.id.menu_export);
			menu.removeItem(R.id.menu_import);
		}
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

	private void optionSettings() {
		// Settings
		Dialog dlgSettings = new Dialog(this);
		dlgSettings.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgSettings.setTitle(getString(R.string.app_name));
		dlgSettings.setContentView(R.layout.xsettings);
		dlgSettings.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, R.drawable.ic_launcher);

		// Expert mode
		CheckBox cbSettings = (CheckBox) dlgSettings.findViewById(R.id.cbExpert);
		cbSettings.setChecked(XRestriction.getSetting(null, XFragmentMain.this, XRestriction.cExpertMode));
		cbSettings.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				XRestriction.setSetting(null, XFragmentMain.this, XRestriction.cExpertMode, isChecked);
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
				FileOutputStream fos = new FileOutputStream(getXmlFile());
				XmlSerializer serializer = Xml.newSerializer();
				serializer.setOutput(fos, "UTF-8");
				serializer.startDocument(null, Boolean.valueOf(true));
				serializer.setFeature("http://xmlpull.org/v1/doc/features.html#indent-output", true);
				serializer.startTag(null, "XPrivacy");

				// Process restrictions
				Cursor cursor = getContentResolver().query(XPrivacyProvider.URI_RESTRICTION, null, null,
						new String[] { Integer.toString(0), Boolean.toString(false) }, null);
				while (cursor.moveToNext()) {
					// Decode uid
					int uid = cursor.getInt(cursor.getColumnIndex(XPrivacyProvider.COL_UID));
					boolean restricted = Boolean.parseBoolean(cursor.getString(cursor
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
								String restrictionName = cursor.getString(cursor
										.getColumnIndex(XPrivacyProvider.COL_RESTRICTION));
								serializer.attribute(null, "Restriction", restrictionName);

								serializer.endTag(null, "Package");
							}
					}
				}
				cursor.close();

				// End serialization
				serializer.endTag(null, "XPrivacy");
				serializer.endDocument();
				serializer.flush();
				fos.close();

				// Display message
				String message = String
						.format("%s %s", getString(R.string.menu_export), getXmlFile().getAbsolutePath());
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
				FileInputStream fis = new FileInputStream(getXmlFile());
				InputStreamReader isr = new InputStreamReader(fis);
				char[] inputBuffer = new char[fis.available()];
				isr.read(inputBuffer);
				String xml = new String(inputBuffer);
				isr.close();
				fis.close();

				// Process XML
				InputStream is = new ByteArrayInputStream(xml.getBytes("UTF-8"));
				DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
				DocumentBuilder db = dbf.newDocumentBuilder();
				Document dom = db.parse(is);
				dom.getDocumentElement().normalize();
				NodeList items = dom.getElementsByTagName("Package");
				for (int i = 0; i < items.getLength(); i++) {
					// Process package
					Node entry = items.item(i);
					NamedNodeMap attrs = entry.getAttributes();
					String packageName = attrs.getNamedItem("Name").getNodeValue();
					String restrictionName = attrs.getNamedItem("Restriction").getNodeValue();

					// Resolve uid
					try {
						PackageInfo pInfo = getPackageManager().getPackageInfo(packageName, 0);
						if (pInfo != null) {
							int uid = pInfo.applicationInfo.uid;
							XRestriction.setRestricted(null, this, uid, restrictionName, true);
						}
					} catch (NameNotFoundException ex) {
						XUtil.log(null, Log.WARN, "Not found package=" + packageName);
					}
				}

				// Display message
				String message = String
						.format("%s %s", getString(R.string.menu_import), getXmlFile().getAbsolutePath());
				Toast toast = Toast.makeText(this, message, Toast.LENGTH_LONG);
				toast.show();
			} catch (Throwable ex) {
				XUtil.bug(null, ex);
				Toast toast = Toast.makeText(this, ex.toString(), Toast.LENGTH_LONG);
				toast.show();
			}
	}

	private File getXmlFile() {
		String folder = Environment.getExternalStorageDirectory().getAbsolutePath();
		String fileName = folder + File.separator + "XPrivacy.xml";
		return new File(fileName);
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

		dlgAbout.setCancelable(true);
		dlgAbout.show();
	}

	private class UpdateTask extends AsyncTask<String, String, String> {

		@Override
		protected String doInBackground(String... uri) {
			try {
				HttpClient httpclient = new DefaultHttpClient();
				HttpResponse response = httpclient.execute(new HttpGet(uri[0]));
				String responseString = null;
				StatusLine statusLine = response.getStatusLine();
				if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
					ByteArrayOutputStream out = new ByteArrayOutputStream();
					response.getEntity().writeTo(out);
					out.close();
					responseString = out.toString("ISO-8859-1"); // ISO-8859-1,utf-8
				} else {
					response.getEntity().getContent().close();
					throw new IOException(statusLine.getReasonPhrase());
				}
				return responseString;
			} catch (Throwable ex) {
				Toast toast = Toast.makeText(XFragmentMain.this, ex.toString(), Toast.LENGTH_LONG);
				toast.show();
				XUtil.bug(null, ex);
				return null;
			}
		}

		@Override
		protected void onPostExecute(String result) {
			super.onPostExecute(result);

			if (result != null)
				try {
					// Parse result
					long newest = 0;
					String latest = null;
					String path = null;
					JSONObject jRoot = new JSONObject(result);
					JSONArray jArray = jRoot.getJSONArray("list");
					for (int i = 0; i < jArray.length(); i++) {
						JSONObject jEntry = jArray.getJSONObject(i);
						String filename = jEntry.getString("filename");
						if (filename.startsWith("XPrivacy_")) {
							long modified = jEntry.getLong("modified");
							if (modified > newest) {
								newest = modified;
								latest = filename;
								path = jEntry.getString("path");
							}
						}
					}

					// Start download
					if (path != null) {
						Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse("http://goo.im" + path));
						startActivity(browserIntent);
					}
				} catch (Throwable ex) {
					Toast toast = Toast.makeText(XFragmentMain.this, ex.toString(), Toast.LENGTH_LONG);
					toast.show();
					XUtil.bug(null, ex);
				}
		}
	}
}

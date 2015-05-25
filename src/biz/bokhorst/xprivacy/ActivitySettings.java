package biz.bokhorst.xprivacy;

import java.io.File;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.location.Address;
import android.location.Geocoder;
import android.os.Bundle;
import android.os.Environment;
import android.os.Process;
import android.support.v7.widget.Toolbar;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

public class ActivitySettings extends ActivityBase implements OnCheckedChangeListener, OnClickListener {
	private int userId;
	private int uid;
	private boolean isApp;
	private boolean odSystem;
	private boolean expert;

	private CheckBox cbNotify;
	private CheckBox cbOnDemand;
	private CheckBox cbBlacklist;
	private CheckBox cbUsage;
	private CheckBox cbParameters;
	private CheckBox cbValues;
	private CheckBox cbLog;
	private CheckBox cbSystem;
	private CheckBox cbExperimental;
	private CheckBox cbHttps;
	private CheckBox cbAOSP;
	private EditText etConfidence;
	private EditText etQuirks;
	private Button btnFlush;
	private Button btnClearDb;
	private CheckBox cbRandom;
	private EditText etSerial;
	private EditText etLat;
	private EditText etLon;
	private EditText etAlt;
	private EditText etSearch;
	private EditText etMac;
	private EditText etIP;
	private EditText etImei;
	private EditText etPhone;
	private EditText etId;
	private EditText etGsfId;
	private EditText etAdId;
	private EditText etMcc;
	private EditText etMnc;
	private EditText etCountry;
	private EditText etOperator;
	private EditText etIccId;
	private EditText etCid;
	private EditText etLac;
	private EditText etSubscriber;
	private EditText etSSID;
	private EditText etUa;
	private CheckBox cbSerial;
	private CheckBox cbLat;
	private CheckBox cbLon;
	private CheckBox cbAlt;
	private CheckBox cbMac;
	private CheckBox cbImei;
	private CheckBox cbPhone;
	private CheckBox cbId;
	private CheckBox cbGsfId;
	private CheckBox cbAdId;
	private CheckBox cbCountry;
	private CheckBox cbSubscriber;
	private CheckBox cbSSID;

	public static final String ACTION_SETTINGS = "biz.bokhorst.xprivacy.action.SETTINGS";
	public static final String cUid = "Uid";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.settings);
		setSupportActionBar((Toolbar) findViewById(R.id.widgetToolbar));
		setTitle(R.string.menu_settings);

		userId = Util.getUserId(Process.myUid());

		final Bundle extras = getIntent().getExtras();
		if (extras != null && extras.containsKey(cUid))
			uid = extras.getInt(cUid);
		else
			uid = userId;

		// Reference controls
		TextView tvInfo = (TextView) findViewById(R.id.tvInfo);

		cbNotify = (CheckBox) findViewById(R.id.cbNotify);
		cbOnDemand = (CheckBox) findViewById(R.id.cbOnDemand);
		cbBlacklist = (CheckBox) findViewById(R.id.cbBlacklist);
		cbUsage = (CheckBox) findViewById(R.id.cbUsage);
		cbParameters = (CheckBox) findViewById(R.id.cbParameters);
		cbValues = (CheckBox) findViewById(R.id.cbValues);
		cbLog = (CheckBox) findViewById(R.id.cbLog);

		CheckBox cbExpert = (CheckBox) findViewById(R.id.cbExpert);
		cbSystem = (CheckBox) findViewById(R.id.cbSystem);
		cbExperimental = (CheckBox) findViewById(R.id.cbExperimental);
		cbHttps = (CheckBox) findViewById(R.id.cbHttps);
		cbAOSP = (CheckBox) findViewById(R.id.cbAOSP);
		LinearLayout llConfidence = (LinearLayout) findViewById(R.id.llConfidence);
		etConfidence = (EditText) findViewById(R.id.etConfidence);
		etQuirks = (EditText) findViewById(R.id.etQuirks);
		btnFlush = (Button) findViewById(R.id.btnFlush);
		btnClearDb = (Button) findViewById(R.id.btnClearDb);

		cbRandom = (CheckBox) findViewById(R.id.cbRandom);
		Button btnRandom = (Button) findViewById(R.id.btnRandom);
		Button btnClear = (Button) findViewById(R.id.btnClear);

		etSerial = (EditText) findViewById(R.id.etSerial);
		etLat = (EditText) findViewById(R.id.etLat);
		etLon = (EditText) findViewById(R.id.etLon);
		etAlt = (EditText) findViewById(R.id.etAlt);
		etSearch = (EditText) findViewById(R.id.etSearch);
		Button btnSearch = (Button) findViewById(R.id.btnSearch);
		etMac = (EditText) findViewById(R.id.etMac);
		etIP = (EditText) findViewById(R.id.etIP);
		etImei = (EditText) findViewById(R.id.etImei);
		etPhone = (EditText) findViewById(R.id.etPhone);
		etId = (EditText) findViewById(R.id.etId);
		etGsfId = (EditText) findViewById(R.id.etGsfId);
		etAdId = (EditText) findViewById(R.id.etAdId);
		etMcc = (EditText) findViewById(R.id.etMcc);
		etMnc = (EditText) findViewById(R.id.etMnc);
		etCountry = (EditText) findViewById(R.id.etCountry);
		etOperator = (EditText) findViewById(R.id.etOperator);
		etIccId = (EditText) findViewById(R.id.etIccId);
		etCid = (EditText) findViewById(R.id.etCid);
		etLac = (EditText) findViewById(R.id.etLac);
		etSubscriber = (EditText) findViewById(R.id.etSubscriber);
		etSSID = (EditText) findViewById(R.id.etSSID);
		etUa = (EditText) findViewById(R.id.etUa);

		cbSerial = (CheckBox) findViewById(R.id.cbSerial);
		cbLat = (CheckBox) findViewById(R.id.cbLat);
		cbLon = (CheckBox) findViewById(R.id.cbLon);
		cbAlt = (CheckBox) findViewById(R.id.cbAlt);
		cbMac = (CheckBox) findViewById(R.id.cbMac);
		cbImei = (CheckBox) findViewById(R.id.cbImei);
		cbPhone = (CheckBox) findViewById(R.id.cbPhone);
		cbId = (CheckBox) findViewById(R.id.cbId);
		cbGsfId = (CheckBox) findViewById(R.id.cbGsfId);
		cbAdId = (CheckBox) findViewById(R.id.cbAdId);
		cbCountry = (CheckBox) findViewById(R.id.cbCountry);
		cbSubscriber = (CheckBox) findViewById(R.id.cbSubscriber);
		cbSSID = (CheckBox) findViewById(R.id.cbSSID);

		// Listen for changes
		cbParameters.setOnCheckedChangeListener(this);
		cbValues.setOnCheckedChangeListener(this);
		cbExpert.setOnCheckedChangeListener(this);
		cbSerial.setOnCheckedChangeListener(this);
		cbLat.setOnCheckedChangeListener(this);
		cbLon.setOnCheckedChangeListener(this);
		cbAlt.setOnCheckedChangeListener(this);
		cbMac.setOnCheckedChangeListener(this);
		cbImei.setOnCheckedChangeListener(this);
		cbPhone.setOnCheckedChangeListener(this);
		cbId.setOnCheckedChangeListener(this);
		cbGsfId.setOnCheckedChangeListener(this);
		cbAdId.setOnCheckedChangeListener(this);
		cbCountry.setOnCheckedChangeListener(this);
		cbSubscriber.setOnCheckedChangeListener(this);
		cbSSID.setOnCheckedChangeListener(this);

		// Get current values
		boolean usage = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingUsage, true);
		boolean parameters = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingParameters, false);
		boolean values = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingValues, false);
		boolean log = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingLog, false);

		boolean components = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingSystem, false);
		boolean experimental = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingExperimental, false);
		boolean https = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingHttps, true);
		boolean aosp = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingAOSPMode, false);
		String confidence = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingConfidence, "");

		// Get quirks
		boolean freeze = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingFreeze, false);
		boolean resolve = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingResolve, false);
		boolean noresolve = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingNoResolve, false);
		boolean permman = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingPermMan, false);
		boolean iwall = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingIntentWall, false);
		boolean safemode = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingSafeMode, false);
		boolean test = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingTestVersions, false);
		boolean updates = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingUpdates, false);
		boolean odsystem = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingOnDemandSystem, false);
		boolean wnomod = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingWhitelistNoModify, false);
		boolean nousage = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingNoUsageData, false);
		List<String> listQuirks = new ArrayList<String>();
		if (freeze)
			listQuirks.add("freeze");
		if (resolve)
			listQuirks.add("resolve");
		if (noresolve)
			listQuirks.add("noresolve");
		if (permman)
			listQuirks.add("permman");
		if (iwall)
			listQuirks.add("iwall");
		if (safemode)
			listQuirks.add("safemode");
		if (test)
			listQuirks.add("test");
		if (updates)
			listQuirks.add("updates");
		if (odsystem)
			listQuirks.add("odsystem");
		if (wnomod)
			listQuirks.add("wnomod");
		if (nousage)
			listQuirks.add("nousage");
		Collections.sort(listQuirks);
		String quirks = TextUtils.join(",", listQuirks.toArray());

		expert = (components || experimental || !https || aosp || !"".equals(confidence) || listQuirks.size() > 0);

		// Application specific
		boolean notify = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingNotify, true);
		boolean ondemand = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingOnDemand, uid == userId);
		boolean blacklist = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingBlacklist, false);
		boolean enabled = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingRestricted, true);

		// Common
		boolean random = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingRandom, false);
		String serial = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingSerial, "");
		String lat = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingLatitude, "");
		String lon = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingLongitude, "");
		String alt = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingAltitude, "");
		String mac = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingMac, "");
		String imei = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingImei, "");
		String phone = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingPhone, "");
		String id = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingId, "");
		String gsfid = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingGsfId, "");
		String adid = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingAdId, "");
		String country = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingCountry, "");
		String subscriber = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingSubscriber, "");
		String ssid = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingSSID, "");

		// Set current values
		if (uid == userId) {
			// Global settings
			tvInfo.setVisibility(View.GONE);
			cbUsage.setChecked(usage);
			cbParameters.setChecked(parameters);
			cbValues.setChecked(values);
			if (userId == 0)
				cbLog.setChecked(log);
			else {
				cbLog.setVisibility(View.GONE);
				btnFlush.setVisibility(View.GONE);
				btnClearDb.setVisibility(View.GONE);
			}
			cbExpert.setChecked(expert);

			if (PrivacyManager.cVersion3
					&& (!Util.isSELinuxEnforced() || "true".equals(Util.getXOption("ignoreselinux"))))
				cbAOSP.setVisibility(View.VISIBLE);

			if (expert) {
				cbSystem.setChecked(components);
				cbExperimental.setChecked(experimental);
				cbHttps.setChecked(https);
				cbAOSP.setChecked(aosp);
				etConfidence.setText(confidence);
				etQuirks.setText(quirks);
			} else {
				cbSystem.setEnabled(false);
				cbExperimental.setEnabled(false);
				cbHttps.setEnabled(false);
				cbHttps.setChecked(true);
				cbAOSP.setEnabled(false);
				cbAOSP.setChecked(false);
				etConfidence.setEnabled(false);
				etQuirks.setEnabled(false);
				btnFlush.setEnabled(false);
				btnClearDb.setEnabled(false);
			}
		} else {
			// Display application names
			ApplicationInfoEx appInfo = new ApplicationInfoEx(this, uid);
			getSupportActionBar().setSubtitle(TextUtils.join(",  ", appInfo.getApplicationName()));

			// Disable global settings
			cbUsage.setVisibility(View.GONE);
			cbParameters.setVisibility(View.GONE);
			cbValues.setVisibility(View.GONE);
			cbLog.setVisibility(View.GONE);
			cbSystem.setVisibility(View.GONE);
			cbExperimental.setVisibility(View.GONE);
			cbHttps.setVisibility(View.GONE);
			cbAOSP.setVisibility(View.GONE);
			llConfidence.setVisibility(View.GONE);
			btnFlush.setVisibility(View.GONE);
			btnClearDb.setVisibility(View.GONE);

			cbExpert.setChecked(expert);
			if (expert)
				etQuirks.setText(quirks);
			else
				etQuirks.setEnabled(false);
		}

		boolean gnotify = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingNotify, true);
		if (uid == userId || gnotify)
			cbNotify.setChecked(notify);
		else
			cbNotify.setVisibility(View.GONE);

		isApp = PrivacyManager.isApplication(uid);
		odSystem = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemandSystem, false);
		boolean gondemand = PrivacyManager.getSettingBool(userId, PrivacyManager.cSettingOnDemand, true);
		if (uid == userId || ((isApp || odSystem) && gondemand)) {
			cbOnDemand.setChecked(ondemand);
			cbOnDemand.setEnabled(enabled);
		} else
			cbOnDemand.setVisibility(View.GONE);

		String blFileName = Environment.getExternalStorageDirectory().getPath() + "/.xprivacy/blacklist";
		if (uid == userId || !new File(blFileName).exists())
			cbBlacklist.setVisibility(View.GONE);
		else
			cbBlacklist.setChecked(blacklist);

		// Common
		cbRandom.setChecked(random);

		// Set randomize on access check boxes
		cbSerial.setChecked(serial.equals(PrivacyManager.cValueRandom));
		cbLat.setChecked(lat.equals(PrivacyManager.cValueRandom));
		cbLon.setChecked(lon.equals(PrivacyManager.cValueRandom));
		cbAlt.setChecked(alt.equals(PrivacyManager.cValueRandom));
		cbMac.setChecked(mac.equals(PrivacyManager.cValueRandom));
		cbImei.setChecked(imei.equals(PrivacyManager.cValueRandom));
		cbPhone.setChecked(phone.equals(PrivacyManager.cValueRandom));
		cbId.setChecked(id.equals(PrivacyManager.cValueRandom));
		cbGsfId.setChecked(gsfid.equals(PrivacyManager.cValueRandom));
		cbAdId.setChecked(adid.equals(PrivacyManager.cValueRandom));
		cbCountry.setChecked(country.equals(PrivacyManager.cValueRandom));
		cbSubscriber.setChecked(subscriber.equals(PrivacyManager.cValueRandom));
		cbSSID.setChecked(ssid.equals(PrivacyManager.cValueRandom));

		// Set fake values
		etSerial.setText(cbSerial.isChecked() ? "" : serial);
		etLat.setText(cbLat.isChecked() ? "" : lat);
		etLon.setText(cbLon.isChecked() ? "" : lon);
		etAlt.setText(cbAlt.isChecked() ? "" : alt);
		etMac.setText(cbMac.isChecked() ? "" : mac);
		etImei.setText(cbImei.isChecked() ? "" : imei);
		etPhone.setText(cbPhone.isChecked() ? "" : phone);
		etId.setText(cbId.isChecked() ? "" : id);
		etGsfId.setText(cbGsfId.isChecked() ? "" : gsfid);
		etAdId.setText(cbAdId.isChecked() ? "" : adid);
		etCountry.setText(cbCountry.isChecked() ? "" : country);
		etSubscriber.setText(cbSubscriber.isChecked() ? "" : subscriber);
		etSSID.setText(cbSSID.isChecked() ? "" : ssid);

		etSerial.setEnabled(!cbSerial.isChecked());
		etLat.setEnabled(!cbLat.isChecked());
		etLon.setEnabled(!cbLon.isChecked());
		etAlt.setEnabled(!cbAlt.isChecked());

		etSearch.setEnabled(Geocoder.isPresent());
		btnSearch.setEnabled(Geocoder.isPresent());

		etMac.setEnabled(!cbMac.isChecked());
		etImei.setEnabled(!cbImei.isChecked());
		etPhone.setEnabled(!cbPhone.isChecked());
		etId.setEnabled(!cbId.isChecked());
		etGsfId.setEnabled(!cbGsfId.isChecked());
		etAdId.setEnabled(!cbAdId.isChecked());
		etCountry.setEnabled(!cbCountry.isChecked());
		etSubscriber.setEnabled(!cbSubscriber.isChecked());
		etSSID.setEnabled(!cbSSID.isChecked());

		etIP.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingIP, ""));
		etMcc.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingMcc, ""));
		etMnc.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingMnc, ""));
		etOperator.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingOperator, ""));
		etIccId.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingIccId, ""));
		etCid.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingCid, ""));
		etLac.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingLac, ""));
		etUa.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingUa, ""));

		btnFlush.setOnClickListener(this);
		btnClearDb.setOnClickListener(this);
		btnRandom.setOnClickListener(this);
		btnClear.setOnClickListener(this);
		btnSearch.setOnClickListener(this);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		if (inflater != null && PrivacyService.checkClient()) {
			inflater.inflate(R.menu.settings, menu);
			return true;
		} else
			return false;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_cancel:
			finish();
			return true;
		case R.id.menu_save:
			optionSave();
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
		switch (buttonView.getId()) {
		case R.id.cbParameters:
		case R.id.cbValues:
			if (isChecked && Util.hasProLicense(this) == null) {
				buttonView.setChecked(false);
				Util.viewUri(this, ActivityMain.cProUri);
			}
			break;
		case R.id.cbExpert:
			cbSystem.setEnabled(isChecked);
			cbExperimental.setEnabled(isChecked);
			cbHttps.setEnabled(isChecked);
			cbAOSP.setEnabled(isChecked);
			etConfidence.setEnabled(isChecked);
			etQuirks.setEnabled(isChecked);
			btnFlush.setEnabled(isChecked);
			btnClearDb.setEnabled(isChecked);
			if (isChecked) {
				if (!expert)
					Toast.makeText(this, getString(R.string.msg_expert), Toast.LENGTH_LONG).show();
			} else {
				cbSystem.setChecked(false);
				cbExperimental.setChecked(false);
				cbHttps.setChecked(true);
				cbAOSP.setChecked(false);
				etConfidence.setText("");
				etQuirks.setText("");
			}
			break;
		case R.id.cbSerial:
			etSerial.setEnabled(!isChecked);
			break;
		case R.id.cbLat:
			etLat.setEnabled(!isChecked);
			break;
		case R.id.cbLon:
			etLon.setEnabled(!isChecked);
			break;
		case R.id.cbAlt:
			etAlt.setEnabled(!isChecked);
			break;
		case R.id.cbMac:
			etMac.setEnabled(!isChecked);
			break;
		case R.id.cbImei:
			etImei.setEnabled(!isChecked);
			break;
		case R.id.cbPhone:
			etPhone.setEnabled(!isChecked);
			break;
		case R.id.cbId:
			etId.setEnabled(!isChecked);
			break;
		case R.id.cbGsfId:
			etGsfId.setEnabled(!isChecked);
			break;
		case R.id.cbAdId:
			etAdId.setEnabled(!isChecked);
			break;
		case R.id.cbCountry:
			etCountry.setEnabled(!isChecked);
			break;
		case R.id.cbSubscriber:
			etSubscriber.setEnabled(!isChecked);
			break;
		case R.id.cbSSID:
			etSSID.setEnabled(!isChecked);
			break;
		}
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnFlush:
			flush();
			break;
		case R.id.btnClearDb:
			clearDB();
			break;
		case R.id.btnRandom:
			randomize();
			break;
		case R.id.btnClear:
			clear();
			break;
		case R.id.btnSearch:
			search();
			break;
		}
	}

	private void clearDB() {
		AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(ActivitySettings.this);
		alertDialogBuilder.setTitle(R.string.menu_clear_db);
		alertDialogBuilder.setMessage(R.string.msg_sure);
		alertDialogBuilder.setIcon(getThemed(R.attr.icon_launcher));
		alertDialogBuilder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				PrivacyManager.clear();
				Toast.makeText(ActivitySettings.this, getString(R.string.msg_reboot), Toast.LENGTH_LONG).show();
				finish();

				// Refresh main UI
				Intent intent = new Intent(ActivitySettings.this, ActivityMain.class);
				intent.putExtra(ActivityMain.cAction, ActivityMain.cActionRefresh);
				startActivity(intent);
			}
		});
		alertDialogBuilder.setNegativeButton(getString(android.R.string.cancel), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
			}
		});
		AlertDialog alertDialog = alertDialogBuilder.create();
		alertDialog.show();
	}

	private void randomize() {
		etSerial.setText(PrivacyManager.getRandomProp("SERIAL"));
		etLat.setText(PrivacyManager.getRandomProp("LAT"));
		etLon.setText(PrivacyManager.getRandomProp("LON"));
		etAlt.setText(PrivacyManager.getRandomProp("ALT"));
		etMac.setText(PrivacyManager.getRandomProp("MAC"));
		etImei.setText(PrivacyManager.getRandomProp("IMEI"));
		etPhone.setText(PrivacyManager.getRandomProp("PHONE"));
		etId.setText(PrivacyManager.getRandomProp("ANDROID_ID"));
		etGsfId.setText(PrivacyManager.getRandomProp("GSF_ID"));
		etAdId.setText(PrivacyManager.getRandomProp("AdvertisingId"));
		etCountry.setText(PrivacyManager.getRandomProp("ISO3166"));
		etSubscriber.setText(PrivacyManager.getRandomProp("SubscriberId"));
		etSSID.setText(PrivacyManager.getRandomProp("SSID"));
	}

	private void clear() {
		final EditText[] edits = new EditText[] { etSerial, etLat, etLon, etAlt, etMac, etIP, etImei, etPhone, etId,
				etGsfId, etAdId, etMcc, etMnc, etCountry, etOperator, etIccId, etCid, etLac, etSubscriber, etSSID, etUa };
		final CheckBox[] boxes = new CheckBox[] { cbSerial, cbLat, cbLon, cbAlt, cbMac, cbImei, cbPhone, cbId, cbGsfId,
				cbAdId, cbCountry, cbSubscriber, cbSSID };

		for (EditText edit : edits)
			edit.setText("");
		etSearch.setText("");

		for (CheckBox box : boxes)
			box.setChecked(false);
	}

	private void search() {
		try {
			String search = etSearch.getText().toString();
			final List<Address> listAddress = new Geocoder(ActivitySettings.this).getFromLocationName(search, 1);
			if (listAddress.size() > 0) {
				Address address = listAddress.get(0);

				// Get coordinates
				if (address.hasLatitude()) {
					cbLat.setChecked(false);
					etLat.setText(Double.toString(address.getLatitude()));
				}
				if (address.hasLongitude()) {
					cbLon.setChecked(false);
					etLon.setText(Double.toString(address.getLongitude()));
				}

				// Get address
				StringBuilder sb = new StringBuilder();
				for (int i = 0; i <= address.getMaxAddressLineIndex(); i++) {
					if (i != 0)
						sb.append(", ");
					sb.append(address.getAddressLine(i));
				}
				etSearch.setText(sb.toString());
			}
		} catch (Throwable ex) {
			Toast.makeText(ActivitySettings.this, ex.getMessage(), Toast.LENGTH_LONG).show();
		}
	}

	private void flush() {
		Intent flushIntent = new Intent(UpdateService.cFlush);
		flushIntent.setPackage(getPackageName());
		startService(flushIntent);
		Toast.makeText(ActivitySettings.this, getString(R.string.msg_done), Toast.LENGTH_LONG).show();
	}

	@SuppressLint("DefaultLocale")
	private void optionSave() {
		if (uid == userId) {
			// Global settings
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingUsage, Boolean.toString(cbUsage.isChecked()));
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingParameters,
					Boolean.toString(cbParameters.isChecked()));
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingValues, Boolean.toString(cbValues.isChecked()));
			if (userId == 0)
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingLog, Boolean.toString(cbLog.isChecked()));
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingSystem, Boolean.toString(cbSystem.isChecked()));
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingExperimental,
					Boolean.toString(cbExperimental.isChecked()));
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingHttps, Boolean.toString(cbHttps.isChecked()));
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingAOSPMode, Boolean.toString(cbAOSP.isChecked()));
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingConfidence, etConfidence.getText().toString());
		}

		// Quirks
		List<String> listQuirks = Arrays
				.asList(etQuirks.getText().toString().toLowerCase().replace(" ", "").split(","));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingFreeze, Boolean.toString(listQuirks.contains("freeze")));
		PrivacyManager
				.setSetting(uid, PrivacyManager.cSettingResolve, Boolean.toString(listQuirks.contains("resolve")));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingNoResolve,
				Boolean.toString(listQuirks.contains("noresolve")));
		PrivacyManager
				.setSetting(uid, PrivacyManager.cSettingPermMan, Boolean.toString(listQuirks.contains("permman")));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingIntentWall,
				Boolean.toString(listQuirks.contains("iwall")));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingSafeMode,
				Boolean.toString(listQuirks.contains("safemode")));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingTestVersions,
				Boolean.toString(listQuirks.contains("test")));
		PrivacyManager
				.setSetting(uid, PrivacyManager.cSettingUpdates, Boolean.toString(listQuirks.contains("updates")));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingOnDemandSystem,
				Boolean.toString(listQuirks.contains("odsystem")));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingWhitelistNoModify,
				Boolean.toString(listQuirks.contains("wnomod")));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingNoUsageData,
				Boolean.toString(listQuirks.contains("nousage")));

		// Notifications
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingNotify, Boolean.toString(cbNotify.isChecked()));

		// On demand restricting
		if (uid == userId || (isApp || odSystem))
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingOnDemand, Boolean.toString(cbOnDemand.isChecked()));

		if (uid != userId)
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingBlacklist, Boolean.toString(cbBlacklist.isChecked()));

		// Random at boot
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingRandom, cbRandom.isChecked() ? Boolean.toString(true)
				: null);

		// Serial#
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingSerial, getValue(cbSerial, etSerial));

		// Set latitude
		if (cbLat.isChecked())
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingLatitude, PrivacyManager.cValueRandom);
		else
			try {
				float lat = Float.parseFloat(etLat.getText().toString().replace(',', '.'));
				if (lat < -90 || lat > 90)
					throw new InvalidParameterException();

				PrivacyManager.setSetting(uid, PrivacyManager.cSettingLatitude, Float.toString(lat));
			} catch (Throwable ignored) {
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingLatitude, null);
			}

		// Set longitude
		if (cbLon.isChecked())
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingLongitude, PrivacyManager.cValueRandom);
		else
			try {
				float lon = Float.parseFloat(etLon.getText().toString().replace(',', '.'));
				if (lon < -180 || lon > 180)
					throw new InvalidParameterException();
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingLongitude, Float.toString(lon));
			} catch (Throwable ignored) {
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingLongitude, null);
			}

		// Set altitude
		if (cbAlt.isChecked())
			PrivacyManager.setSetting(uid, PrivacyManager.cSettingAltitude, PrivacyManager.cValueRandom);
		else
			try {
				float alt = Float.parseFloat(etAlt.getText().toString().replace(',', '.'));
				if (alt < -10000 || alt > 10000)
					throw new InvalidParameterException();
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingAltitude, Float.toString(alt));
			} catch (Throwable ignored) {
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingAltitude, null);
			}

		// Check Gsf ID
		try {
			String value = etGsfId.getText().toString();
			if (!"".equals(value))
				Long.parseLong(value.toLowerCase(), 16);
		} catch (NumberFormatException ignored) {
			etGsfId.setText("");
		}

		// Other settings
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingMac, getValue(cbMac, etMac));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingIP, getValue(null, etIP));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingImei, getValue(cbImei, etImei));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingPhone, getValue(cbPhone, etPhone));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingId, getValue(cbId, etId));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingGsfId, getValue(cbGsfId, etGsfId));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingAdId, getValue(cbAdId, etAdId));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingMcc, getValue(null, etMcc));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingMnc, getValue(null, etMnc));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingCountry, getValue(cbCountry, etCountry));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingOperator, getValue(null, etOperator));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingIccId, getValue(null, etIccId));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingCid, getValue(null, etCid));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingLac, getValue(null, etLac));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingSubscriber, getValue(cbSubscriber, etSubscriber));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingSSID, getValue(cbSSID, etSSID));
		PrivacyManager.setSetting(uid, PrivacyManager.cSettingUa, getValue(null, etUa));

		finish();

		// Refresh view
		if (uid == userId) {
			Intent intent = new Intent(ActivitySettings.this, ActivityMain.class);
			startActivity(intent);
		} else {
			Intent intent = new Intent(ActivitySettings.this, ActivityApp.class);
			intent.putExtra(ActivityApp.cUid, uid);
			intent.putExtra(ActivityApp.cAction, ActivityApp.cActionRefresh);
			startActivity(intent);
		}
	}

	private static String getValue(CheckBox check, EditText edit) {
		if (check != null && check.isChecked())
			return PrivacyManager.cValueRandom;
		String value = edit.getText().toString().trim();
		return ("".equals(value) ? null : value);
	}
}

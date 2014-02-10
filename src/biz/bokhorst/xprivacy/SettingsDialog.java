package biz.bokhorst.xprivacy;

import java.security.InvalidParameterException;
import java.util.List;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.content.Intent;
import android.location.Address;
import android.location.Geocoder;
import android.text.TextUtils;
import android.view.View;
import android.view.Window;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

public class SettingsDialog {

	public static void edit(final ActivityBase context, ApplicationInfoEx appInfo) {
		final int uid = (appInfo == null ? 0 : appInfo.getUid());

		// Build dialog
		String themeName = PrivacyManager.getSetting(0, PrivacyManager.cSettingTheme, "", false);
		int themeId = (themeName.equals("Dark") ? R.style.CustomTheme_Dialog : R.style.CustomTheme_Light_Dialog);
		final Dialog dlgSettings = new Dialog(context, themeId);
		dlgSettings.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgSettings.setTitle(R.string.menu_settings);
		dlgSettings.setContentView(R.layout.settings);
		dlgSettings.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, context.getThemed(R.attr.icon_launcher));

		// Reference controls
		TextView tvAppName = (TextView) dlgSettings.findViewById(R.id.tvAppName);
		View vwAppNameBorder = (View) dlgSettings.findViewById(R.id.vwAppNameBorder);

		final CheckBox cbNotify = (CheckBox) dlgSettings.findViewById(R.id.cbNotify);
		final CheckBox cbOnDemand = (CheckBox) dlgSettings.findViewById(R.id.cbOnDemand);

		final CheckBox cbUsage = (CheckBox) dlgSettings.findViewById(R.id.cbUsage);
		final CheckBox cbLog = (CheckBox) dlgSettings.findViewById(R.id.cbLog);
		final CheckBox cbExpert = (CheckBox) dlgSettings.findViewById(R.id.cbExpert);
		final CheckBox cbSystem = (CheckBox) dlgSettings.findViewById(R.id.cbSystem);
		final CheckBox cbDangerous = (CheckBox) dlgSettings.findViewById(R.id.cbDangerous);
		final CheckBox cbExperimental = (CheckBox) dlgSettings.findViewById(R.id.cbExperimental);
		final CheckBox cbHttps = (CheckBox) dlgSettings.findViewById(R.id.cbHttps);
		final LinearLayout llConfidence = (LinearLayout) dlgSettings.findViewById(R.id.llConfidence);
		final EditText etConfidence = (EditText) dlgSettings.findViewById(R.id.etConfidence);

		final Button btnClear = (Button) dlgSettings.findViewById(R.id.btnClear);
		final CheckBox cbRandom = (CheckBox) dlgSettings.findViewById(R.id.cbRandom);
		final Button btnRandom = (Button) dlgSettings.findViewById(R.id.btnRandom);

		final EditText etSerial = (EditText) dlgSettings.findViewById(R.id.etSerial);
		final EditText etLat = (EditText) dlgSettings.findViewById(R.id.etLat);
		final EditText etLon = (EditText) dlgSettings.findViewById(R.id.etLon);
		final EditText etAlt = (EditText) dlgSettings.findViewById(R.id.etAlt);
		final EditText etSearch = (EditText) dlgSettings.findViewById(R.id.etSearch);
		final Button btnSearch = (Button) dlgSettings.findViewById(R.id.btnSearch);
		final EditText etMac = (EditText) dlgSettings.findViewById(R.id.etMac);
		final EditText etIP = (EditText) dlgSettings.findViewById(R.id.etIP);
		final EditText etImei = (EditText) dlgSettings.findViewById(R.id.etImei);
		final EditText etPhone = (EditText) dlgSettings.findViewById(R.id.etPhone);
		final EditText etId = (EditText) dlgSettings.findViewById(R.id.etId);
		final EditText etGsfId = (EditText) dlgSettings.findViewById(R.id.etGsfId);
		final EditText etAdId = (EditText) dlgSettings.findViewById(R.id.etAdId);
		final EditText etMcc = (EditText) dlgSettings.findViewById(R.id.etMcc);
		final EditText etMnc = (EditText) dlgSettings.findViewById(R.id.etMnc);
		final EditText etCountry = (EditText) dlgSettings.findViewById(R.id.etCountry);
		final EditText etOperator = (EditText) dlgSettings.findViewById(R.id.etOperator);
		final EditText etIccId = (EditText) dlgSettings.findViewById(R.id.etIccId);
		final EditText etSubscriber = (EditText) dlgSettings.findViewById(R.id.etSubscriber);
		final EditText etSSID = (EditText) dlgSettings.findViewById(R.id.etSSID);
		final EditText etUa = (EditText) dlgSettings.findViewById(R.id.etUa);

		final CheckBox cbSerial = (CheckBox) dlgSettings.findViewById(R.id.cbSerial);
		final CheckBox cbLat = (CheckBox) dlgSettings.findViewById(R.id.cbLat);
		final CheckBox cbLon = (CheckBox) dlgSettings.findViewById(R.id.cbLon);
		final CheckBox cbAlt = (CheckBox) dlgSettings.findViewById(R.id.cbAlt);
		final CheckBox cbMac = (CheckBox) dlgSettings.findViewById(R.id.cbMac);
		final CheckBox cbImei = (CheckBox) dlgSettings.findViewById(R.id.cbImei);
		final CheckBox cbPhone = (CheckBox) dlgSettings.findViewById(R.id.cbPhone);
		final CheckBox cbId = (CheckBox) dlgSettings.findViewById(R.id.cbId);
		final CheckBox cbGsfId = (CheckBox) dlgSettings.findViewById(R.id.cbGsfId);
		final CheckBox cbAdId = (CheckBox) dlgSettings.findViewById(R.id.cbAdId);
		final CheckBox cbCountry = (CheckBox) dlgSettings.findViewById(R.id.cbCountry);
		final CheckBox cbSubscriber = (CheckBox) dlgSettings.findViewById(R.id.cbSubscriber);
		final CheckBox cbSSID = (CheckBox) dlgSettings.findViewById(R.id.cbSSID);

		Button btnOk = (Button) dlgSettings.findViewById(R.id.btnOk);
		Button btnCancel = (Button) dlgSettings.findViewById(R.id.btnCancel);

		final EditText[] edits = new EditText[] { etSerial, etLat, etLon, etAlt, etMac, etIP, etImei, etPhone, etId,
				etGsfId, etAdId, etMcc, etMnc, etCountry, etOperator, etIccId, etSubscriber, etSSID, etUa };

		final CheckBox[] boxes = new CheckBox[] { cbSerial, cbLat, cbLon, cbAlt, cbMac, cbImei, cbPhone, cbId, cbGsfId,
				cbAdId, cbCountry, cbSubscriber, cbSSID };

		// Listen for changes
		cbExpert.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				cbSystem.setEnabled(isChecked);
				cbDangerous.setEnabled(isChecked);
				cbExperimental.setEnabled(isChecked);
				cbHttps.setEnabled(isChecked);
				etConfidence.setEnabled(isChecked);
				if (!isChecked) {
					cbSystem.setChecked(false);
					cbDangerous.setChecked(false);
					cbExperimental.setChecked(false);
					cbHttps.setChecked(true);
					etConfidence.setText("");
				}
			}
		});

		cbSerial.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etSerial.setEnabled(!isChecked);
			}
		});

		cbLat.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etLat.setEnabled(!isChecked);
			}
		});

		cbLon.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etLon.setEnabled(!isChecked);
			}
		});

		cbAlt.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etAlt.setEnabled(!isChecked);
			}
		});

		cbMac.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etMac.setEnabled(!isChecked);
			}
		});

		cbImei.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etImei.setEnabled(!isChecked);
			}
		});

		cbPhone.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etPhone.setEnabled(!isChecked);
			}
		});

		cbId.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etId.setEnabled(!isChecked);
			}
		});

		cbGsfId.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etGsfId.setEnabled(!isChecked);
			}
		});

		cbAdId.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etAdId.setEnabled(!isChecked);
			}
		});

		cbCountry.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etCountry.setEnabled(!isChecked);
			}
		});

		cbSubscriber.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etSubscriber.setEnabled(!isChecked);
			}
		});

		cbSSID.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				etSSID.setEnabled(!isChecked);
			}
		});

		// Display app name
		if (appInfo == null) {
			tvAppName.setVisibility(View.GONE);
			vwAppNameBorder.setVisibility(View.GONE);
		} else
			tvAppName.setText(TextUtils.join(", ", appInfo.getApplicationName()));

		// Get current values
		boolean usage = PrivacyManager.getSettingBool(uid, PrivacyManager.cSettingUsage, true, false);
		boolean log = PrivacyManager.getSettingBool(uid, PrivacyManager.cSettingLog, false, false);
		boolean components = PrivacyManager.getSettingBool(uid, PrivacyManager.cSettingSystem, false, false);
		boolean dangerous = PrivacyManager.getSettingBool(uid, PrivacyManager.cSettingDangerous, false, false);
		boolean experimental = PrivacyManager.getSettingBool(uid, PrivacyManager.cSettingExperimental, false, false);
		boolean https = PrivacyManager.getSettingBool(uid, PrivacyManager.cSettingHttps, true, false);
		String confidence = PrivacyManager.getSetting(uid, PrivacyManager.cSettingConfidence, "", false);
		final boolean expert = (components || dangerous || experimental || !https || !"".equals(confidence));

		// Common
		boolean random = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingRandom, false, false);

		// Application specific
		boolean notify = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingNotify, true, false);
		final boolean ondemand = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingOnDemand, uid == 0, false);

		String serial = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingSerial, "", false);
		String lat = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingLatitude, "", false);
		String lon = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingLongitude, "", false);
		String alt = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingAltitude, "", false);
		String mac = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingMac, "", false);
		String imei = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingImei, "", false);
		String phone = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingPhone, "", false);
		String id = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingId, "", false);
		String gsfid = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingGsfId, "", false);
		String adid = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingAdId, "", false);
		String country = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingCountry, "", false);
		String subscriber = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingSubscriber, "", false);
		String ssid = PrivacyManager.getSetting(-uid, PrivacyManager.cSettingSSID, "", false);

		// Set current values
		if (uid == 0) {
			// Disable app settings
			cbNotify.setVisibility(View.GONE);

			// Global settings
			cbUsage.setChecked(usage);
			cbLog.setChecked(log);
			cbExpert.setChecked(expert);
			if (expert) {
				cbSystem.setChecked(components);
				cbDangerous.setChecked(dangerous);
				cbExperimental.setChecked(experimental);
				cbHttps.setChecked(https);
				etConfidence.setText(confidence);
			} else {
				cbSystem.setEnabled(false);
				cbDangerous.setEnabled(false);
				cbExperimental.setEnabled(false);
				cbHttps.setEnabled(false);
				cbHttps.setChecked(true);
				etConfidence.setEnabled(false);
			}
		} else {
			// Disable global settings
			cbUsage.setVisibility(View.GONE);
			cbLog.setVisibility(View.GONE);
			cbExpert.setVisibility(View.GONE);
			cbSystem.setVisibility(View.GONE);
			cbDangerous.setVisibility(View.GONE);
			cbExperimental.setVisibility(View.GONE);
			cbHttps.setVisibility(View.GONE);
			llConfidence.setVisibility(View.GONE);

			// Application specific settings
			cbNotify.setChecked(notify);
		}

		if (uid == 0 || PrivacyManager.isApplication(uid))
			cbOnDemand.setChecked(ondemand);
		else
			cbOnDemand.setVisibility(View.GONE);

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

		etIP.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingIP, "", false));
		etMcc.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingMcc, "", false));
		etMnc.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingMnc, "", false));
		etOperator.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingOperator, "", false));
		etIccId.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingIccId, "", false));
		etUa.setText(PrivacyManager.getSetting(-uid, PrivacyManager.cSettingUa, "", false));

		// Handle search
		btnSearch.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				try {
					String search = etSearch.getText().toString();
					final List<Address> listAddress = new Geocoder(context).getFromLocationName(search, 1);
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
					Toast.makeText(context, ex.getMessage(), Toast.LENGTH_LONG).show();
					Util.bug(null, ex);
				}
			}
		});

		// Handle clear
		btnClear.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				for (EditText edit : edits)
					edit.setText("");
				etSearch.setText("");

				for (CheckBox box : boxes)
					box.setChecked(false);
			}
		});

		// Handle manual randomize
		btnRandom.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
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
		});

		// Handle OK
		btnOk.setOnClickListener(new View.OnClickListener() {
			@Override
			@SuppressLint("DefaultLocale")
			public void onClick(View view) {
				if (uid == 0) {
					// Global settings
					PrivacyManager.setSetting(uid, PrivacyManager.cSettingUsage, Boolean.toString(cbUsage.isChecked()));
					PrivacyManager.setSetting(uid, PrivacyManager.cSettingLog, Boolean.toString(cbLog.isChecked()));
					PrivacyManager.setSetting(uid, PrivacyManager.cSettingSystem,
							Boolean.toString(cbSystem.isChecked()));
					PrivacyManager.setSetting(uid, PrivacyManager.cSettingDangerous,
							Boolean.toString(cbDangerous.isChecked()));
					PrivacyManager.setSetting(uid, PrivacyManager.cSettingExperimental,
							Boolean.toString(cbExperimental.isChecked()));
					PrivacyManager.setSetting(uid, PrivacyManager.cSettingHttps, Boolean.toString(cbHttps.isChecked()));
					PrivacyManager
							.setSetting(uid, PrivacyManager.cSettingConfidence, etConfidence.getText().toString());
				} else {
					// App specific settings
					PrivacyManager.setSetting(uid, PrivacyManager.cSettingNotify,
							Boolean.toString(cbNotify.isChecked()));
				}

				if (uid == 0 || PrivacyManager.isApplication(uid))
					PrivacyManager.setSetting(uid, PrivacyManager.cSettingOnDemand,
							Boolean.toString(cbOnDemand.isChecked()));

				// Random at boot
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingRandom,
						cbRandom.isChecked() ? Boolean.toString(true) : null);

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
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingSubscriber, getValue(cbSubscriber, etSubscriber));
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingSSID, getValue(cbSSID, etSSID));
				PrivacyManager.setSetting(uid, PrivacyManager.cSettingUa, getValue(null, etUa));

				dlgSettings.dismiss();

				// Refresh view
				if (uid == 0) {
					Intent intent = new Intent(context, ActivityMain.class);
					context.startActivity(intent);
				} else {
					Intent intent = new Intent(context, ActivityApp.class);
					intent.putExtra(ActivityApp.cUid, uid);
					intent.putExtra(ActivityApp.cAction, ActivityApp.cActionRefresh);
					context.startActivity(intent);
				}
			}
		});

		// Handle Cancel
		btnCancel.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				dlgSettings.dismiss();
			}
		});

		// Show dialog
		dlgSettings.setCancelable(true);
		dlgSettings.show();
	}

	private static String getValue(CheckBox check, EditText edit) {
		if (check != null && check.isChecked())
			return PrivacyManager.cValueRandom;
		String value = edit.getText().toString().trim();
		return ("".equals(value) ? null : value);
	}
}

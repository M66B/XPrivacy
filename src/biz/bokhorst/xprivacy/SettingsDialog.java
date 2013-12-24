package biz.bokhorst.xprivacy;

import java.security.InvalidParameterException;
import java.util.List;

import android.app.Dialog;
import android.content.Context;
import android.location.Address;
import android.location.Geocoder;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

public class SettingsDialog {

	public static void edit(final Context context, ApplicationInfoEx appInfo) {
		final int uid = (appInfo == null ? 0 : appInfo.getUid());

		// Build dialog
		String themeName = PrivacyManager.getSetting(null, context, 0, PrivacyManager.cSettingTheme, "", false);
		int themeId = (themeName.equals("Dark") ? R.style.CustomTheme_Dialog : R.style.CustomTheme_Light_Dialog);
		final Dialog dlgSettings = new Dialog(context, themeId);
		dlgSettings.requestWindowFeature(Window.FEATURE_LEFT_ICON);
		dlgSettings.setTitle(context.getString(R.string.menu_settings));
		dlgSettings.setContentView(R.layout.settings);
		dlgSettings.setFeatureDrawableResource(Window.FEATURE_LEFT_ICON, Util.getThemed(context, R.attr.icon_launcher));

		// Reference controls
		TextView tvAppName = (TextView) dlgSettings.findViewById(R.id.tvAppName);
		View vwAppNameBorder = (View) dlgSettings.findViewById(R.id.vwAppNameBorder);

		final CheckBox cbNotify = (CheckBox) dlgSettings.findViewById(R.id.cbNotify);
		final CheckBox cbLog = (CheckBox) dlgSettings.findViewById(R.id.cbLog);
		final CheckBox cbExpert = (CheckBox) dlgSettings.findViewById(R.id.cbExpert);
		final CheckBox cbDangerous = (CheckBox) dlgSettings.findViewById(R.id.cbDangerous);
		final CheckBox cbUsage = (CheckBox) dlgSettings.findViewById(R.id.cbUsage);
		final CheckBox cbExperimental = (CheckBox) dlgSettings.findViewById(R.id.cbExperimental);
		final LinearLayout llConfidence = (LinearLayout) dlgSettings.findViewById(R.id.llConfidence);
		final EditText etConfidence = (EditText) dlgSettings.findViewById(R.id.etConfidence);
		final CheckBox cbGlobal = (CheckBox) dlgSettings.findViewById(R.id.cbGlobal);
		final Button btnClear = (Button) dlgSettings.findViewById(R.id.btnClear);
		final CheckBox cbRandom = (CheckBox) dlgSettings.findViewById(R.id.cbRandom);
		final Button btnRandom = (Button) dlgSettings.findViewById(R.id.btnRandom);

		final EditText etSerial = (EditText) dlgSettings.findViewById(R.id.etSerial);
		final EditText etLat = (EditText) dlgSettings.findViewById(R.id.etLat);
		final EditText etLon = (EditText) dlgSettings.findViewById(R.id.etLon);
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

		// Listen for changes
		cbExpert.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				cbDangerous.setEnabled(isChecked);
				cbUsage.setEnabled(isChecked);
				cbExperimental.setEnabled(isChecked);
				etConfidence.setEnabled(isChecked);
				if (!isChecked) {
					cbDangerous.setChecked(false);
					cbUsage.setChecked(false);
					cbExperimental.setChecked(false);
					etConfidence.setText("");
				}
			}
		});

		cbGlobal.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				cbRandom.setEnabled(!isChecked);
				btnClear.setEnabled(!isChecked);
				btnRandom.setEnabled(!isChecked);

				etSerial.setEnabled(!isChecked);
				etLat.setEnabled(!isChecked);
				etLon.setEnabled(!isChecked);
				etSearch.setEnabled(!isChecked);
				etMac.setEnabled(!isChecked);
				etIP.setEnabled(!isChecked);
				etImei.setEnabled(!isChecked);
				etPhone.setEnabled(!isChecked);
				etId.setEnabled(!isChecked);
				etGsfId.setEnabled(!isChecked);
				etAdId.setEnabled(!isChecked);
				etMcc.setEnabled(!isChecked);
				etMnc.setEnabled(!isChecked);
				etCountry.setEnabled(!isChecked);
				etOperator.setEnabled(!isChecked);
				etIccId.setEnabled(!isChecked);
				etSubscriber.setEnabled(!isChecked);
				etSSID.setEnabled(!isChecked);
				etUa.setEnabled(!isChecked);

				cbSerial.setEnabled(!isChecked);
				cbLat.setEnabled(!isChecked);
				cbLon.setEnabled(!isChecked);
				btnSearch.setEnabled(!isChecked);
				cbMac.setEnabled(!isChecked);
				cbImei.setEnabled(!isChecked);
				cbPhone.setEnabled(!isChecked);
				cbId.setEnabled(!isChecked);
				cbGsfId.setEnabled(!isChecked);
				cbAdId.setEnabled(!isChecked);
				cbCountry.setEnabled(!isChecked);
				cbSubscriber.setEnabled(!isChecked);
				cbSSID.setEnabled(!isChecked);
			}
		});

		cbRandom.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				cbSerial.setEnabled(!isChecked);
				cbLat.setEnabled(!isChecked);
				cbLon.setEnabled(!isChecked);
				cbMac.setEnabled(!isChecked);
				cbImei.setEnabled(!isChecked);
				cbPhone.setEnabled(!isChecked);
				cbId.setEnabled(!isChecked);
				cbGsfId.setEnabled(!isChecked);
				cbAdId.setEnabled(!isChecked);
				cbCountry.setEnabled(!isChecked);
				cbSubscriber.setEnabled(!isChecked);
				cbSSID.setEnabled(!isChecked);

				etSearch.setEnabled(!isChecked && Geocoder.isPresent());
				btnSearch.setEnabled(!isChecked && Geocoder.isPresent());

				etSerial.setEnabled(!isChecked);
				etLat.setEnabled(!isChecked);
				etLon.setEnabled(!isChecked);
				etMac.setEnabled(!isChecked);
				etImei.setEnabled(!isChecked);
				etPhone.setEnabled(!isChecked);
				etId.setEnabled(!isChecked);
				etGsfId.setEnabled(!isChecked);
				etAdId.setEnabled(!isChecked);
				etCountry.setEnabled(!isChecked);
				etSubscriber.setEnabled(!isChecked);
				etSSID.setEnabled(!isChecked);
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
		boolean notify = PrivacyManager.getSettingBool(null, context, uid, PrivacyManager.cSettingNotify, true, false);
		boolean log = PrivacyManager.getSettingBool(null, context, uid, PrivacyManager.cSettingLog, false, false);
		boolean dangerous = PrivacyManager.getSettingBool(null, context, uid, PrivacyManager.cSettingDangerous, false,
				false);
		boolean usage = PrivacyManager.getSettingBool(null, context, uid, PrivacyManager.cSettingAndroidUsage, false,
				false);
		boolean experimental = PrivacyManager.getSettingBool(null, context, uid, PrivacyManager.cSettingExperimental,
				PrivacyManager.cTestVersion, false);
		String confidence = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingConfidence, "", false);
		final boolean expert = (dangerous || usage || experimental || !"".equals(confidence));
		boolean global = (PrivacyManager.getAppSetting(null, context, uid, PrivacyManager.cSettingSerial, null, false) == null);
		boolean random = PrivacyManager.getSettingBool(null, context, uid, PrivacyManager.cSettingRandom, false, false);

		String serial = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingSerial, "", false);
		String lat = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingLatitude, "", false);
		String lon = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingLongitude, "", false);
		String mac = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingMac, "", false);
		String imei = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingImei, "", false);
		String phone = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingPhone, "", false);
		String id = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingId, "", false);
		String gsfid = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingGsfId, "", false);
		String adid = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingAdId, "", false);
		String country = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingCountry, "", false);
		String subscriber = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingSubscriber, "", false);
		String ssid = PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingSSID, "", false);

		// Set current values
		if (uid == 0) {
			// Disable app settings
			cbNotify.setVisibility(View.GONE);
			cbGlobal.setVisibility(View.GONE);

			// Global settings
			cbLog.setChecked(log);
			cbExpert.setChecked(expert);
			if (expert) {
				cbDangerous.setChecked(dangerous);
				cbUsage.setChecked(usage);
				cbExperimental.setChecked(experimental);
				etConfidence.setText(confidence);
			} else {
				cbDangerous.setEnabled(false);
				cbUsage.setEnabled(false);
				cbExperimental.setEnabled(false);
				etConfidence.setEnabled(false);
			}
		} else {
			// Disable global settings
			cbLog.setVisibility(View.GONE);
			cbExpert.setVisibility(View.GONE);
			cbDangerous.setVisibility(View.GONE);
			cbUsage.setVisibility(View.GONE);
			cbExperimental.setVisibility(View.GONE);
			llConfidence.setVisibility(View.GONE);

			// Application specific settings
			cbNotify.setChecked(notify);
			cbGlobal.setChecked(global);
		}

		cbSerial.setChecked(serial.equals(PrivacyManager.cValueRandom));
		cbLat.setChecked(lat.equals(PrivacyManager.cValueRandom));
		cbLon.setChecked(lon.equals(PrivacyManager.cValueRandom));
		cbMac.setChecked(mac.equals(PrivacyManager.cValueRandom));
		cbImei.setChecked(imei.equals(PrivacyManager.cValueRandom));
		cbPhone.setChecked(phone.equals(PrivacyManager.cValueRandom));
		cbId.setChecked(id.equals(PrivacyManager.cValueRandom));
		cbGsfId.setChecked(gsfid.equals(PrivacyManager.cValueRandom));
		cbAdId.setChecked(adid.equals(PrivacyManager.cValueRandom));
		cbCountry.setChecked(country.equals(PrivacyManager.cValueRandom));
		cbSubscriber.setChecked(subscriber.equals(PrivacyManager.cValueRandom));
		cbSSID.setChecked(ssid.equals(PrivacyManager.cValueRandom));

		etSerial.setText(cbSerial.isChecked() ? "" : serial);
		etLat.setText(cbLat.isChecked() ? "" : lat);
		etLon.setText(cbLon.isChecked() ? "" : lon);
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

		etSearch.setEnabled(!random && Geocoder.isPresent());
		btnSearch.setEnabled(!random && Geocoder.isPresent());

		etMac.setEnabled(!cbMac.isChecked());
		etImei.setEnabled(!cbImei.isChecked());
		etPhone.setEnabled(!cbPhone.isChecked());
		etId.setEnabled(!cbId.isChecked());
		etGsfId.setEnabled(!cbGsfId.isChecked());
		etAdId.setEnabled(!cbAdId.isChecked());
		etCountry.setEnabled(!cbCountry.isChecked());
		etSubscriber.setEnabled(!cbSubscriber.isChecked());
		etSSID.setEnabled(!cbSSID.isChecked());

		etIP.setText(PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingIP, "", false));
		etMcc.setText(PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingMcc, "", false));
		etMnc.setText(PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingMnc, "", false));
		etOperator.setText(PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingOperator, "", false));
		etIccId.setText(PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingIccId, "", false));
		etUa.setText(PrivacyManager.getSetting(null, context, uid, PrivacyManager.cSettingUa, "", false));

		cbRandom.setChecked(random);

		// Handle search
		btnSearch.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				try {
					etLat.setText("");
					etLon.setText("");
					String search = etSearch.getText().toString();
					final List<Address> listAddress = new Geocoder(context).getFromLocationName(search, 1);
					if (listAddress.size() > 0) {
						Address address = listAddress.get(0);

						// Get coordinates
						if (address.hasLatitude())
							etLat.setText(Double.toString(address.getLatitude()));
						if (address.hasLongitude())
							etLon.setText(Double.toString(address.getLongitude()));

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
					Util.bug(null, ex);
				}
			}
		});

		// Handle clear
		btnClear.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				cbRandom.setChecked(false);

				etSerial.setText("");
				etLat.setText("");
				etLon.setText("");
				etSearch.setText("");
				etMac.setText("");
				etIP.setText("");
				etImei.setText("");
				etPhone.setText("");
				etId.setText("");
				etGsfId.setText("");
				etAdId.setText("");
				etMcc.setText("");
				etMnc.setText("");
				etCountry.setText("");
				etOperator.setText("");
				etIccId.setText("");
				etSubscriber.setText("");
				etSSID.setText("");
				etUa.setText("");

				cbSerial.setChecked(false);
				cbLat.setChecked(false);
				cbLon.setChecked(false);
				cbMac.setChecked(false);
				cbImei.setChecked(false);
				cbPhone.setChecked(false);
				cbId.setChecked(false);
				cbGsfId.setChecked(false);
				cbAdId.setChecked(false);
				cbCountry.setChecked(false);
				cbSubscriber.setChecked(false);
				cbSSID.setChecked(false);
			}
		});

		// Handle manual randomize
		btnRandom.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View view) {
				etSerial.setText(PrivacyManager.getRandomProp("SERIAL"));
				etLat.setText(PrivacyManager.getRandomProp("LAT"));
				etLon.setText(PrivacyManager.getRandomProp("LON"));
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
			public void onClick(View view) {
				if (uid == 0) {
					// Global settings
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingLog,
							Boolean.toString(cbLog.isChecked()));
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingDangerous,
							Boolean.toString(cbDangerous.isChecked()));
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingAndroidUsage,
							Boolean.toString(cbUsage.isChecked()));
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingExperimental,
							Boolean.toString(cbExperimental.isChecked()));
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingConfidence, etConfidence
							.getText().toString());
				} else {
					// App specific settings
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingNotify,
							Boolean.toString(cbNotify.isChecked()));
				}

				if (cbGlobal.isChecked()) {
					// Clear all settings
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingRandom, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingSerial, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingLatitude, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingLongitude, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingMac, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingIP, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingImei, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingPhone, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingId, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingGsfId, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingAdId, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingMcc, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingMnc, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingCountry, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingOperator, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingIccId, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingSubscriber, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingSSID, null);
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingUa, null);
					Util.log(null, Log.WARN, "Cleared all settings uid=" + uid);
				} else {
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingRandom,
							Boolean.toString(cbRandom.isChecked()));

					// Serial#
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingSerial,
							cbSerial.isChecked() ? PrivacyManager.cValueRandom : etSerial.getText().toString());

					// Set latitude
					try {
						float lat = Float.parseFloat(etLat.getText().toString().replace(',', '.'));
						if (lat < -90 || lat > 90)
							throw new InvalidParameterException();
						PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingLatitude,
								cbLat.isChecked() ? PrivacyManager.cValueRandom : Float.toString(lat));
					} catch (Throwable ex) {
						PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingLatitude,
								cbLat.isChecked() ? PrivacyManager.cValueRandom : "");
					}

					// Set longitude
					try {
						float lon = Float.parseFloat(etLon.getText().toString().replace(',', '.'));
						if (lon < -180 || lon > 180)
							throw new InvalidParameterException();
						PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingLongitude,
								cbLon.isChecked() ? PrivacyManager.cValueRandom : Float.toString(lon));
					} catch (Throwable ex) {
						PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingLongitude,
								cbLon.isChecked() ? PrivacyManager.cValueRandom : "");
					}

					// Other settings
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingMac,
							cbMac.isChecked() ? PrivacyManager.cValueRandom : etMac.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingIP, etIP.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingImei,
							cbImei.isChecked() ? PrivacyManager.cValueRandom : etImei.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingPhone,
							cbPhone.isChecked() ? PrivacyManager.cValueRandom : etPhone.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingId,
							cbId.isChecked() ? PrivacyManager.cValueRandom : etId.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingGsfId,
							cbGsfId.isChecked() ? PrivacyManager.cValueRandom : etGsfId.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingAdId,
							cbAdId.isChecked() ? PrivacyManager.cValueRandom : etAdId.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingMcc, etMcc.getText()
							.toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingMnc, etMnc.getText()
							.toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingCountry,
							cbCountry.isChecked() ? PrivacyManager.cValueRandom : etCountry.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingOperator, etOperator.getText()
							.toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingIccId, etIccId.getText()
							.toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingSubscriber,
							cbSubscriber.isChecked() ? PrivacyManager.cValueRandom : etSubscriber.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingSSID,
							cbSSID.isChecked() ? PrivacyManager.cValueRandom : etSSID.getText().toString());
					PrivacyManager.setSetting(null, context, uid, PrivacyManager.cSettingUa, etUa.getText().toString());
				}

				dlgSettings.dismiss();
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
}

package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.ParcelableRestriction;
import biz.bokhorst.xprivacy.ParcelableSetting;

interface IPrivacyService {
	int getVersion();
	void migrated();
	List /* String */ check();

	void setRestriction(int uid, String restrictionName, String methodName, boolean restricted);
	void setRestrictionList(in List<ParcelableRestriction> listRestriction);
	boolean getRestriction(int uid, String restrictionName, String methodName, boolean usage);
	List /* Boolean */ getRestrictionList(int uid, String restrictionName);
	void deleteRestrictions(int uid);

	long getUsage(int uid, in List<String> listRestrictionName, String methodName);
	List<ParcelableRestriction> getUsageList(int uid);
	void deleteUsage(int uid);

	void setSetting(int uid, String name, String value);
	void setSettingList(in List<ParcelableSetting> listSetting);
	String getSetting(int uid, String name, String defaultValue);
	Map /* String, String */ getSettingMap(int uid);
	void deleteSettings(int uid);
}

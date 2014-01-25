package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.ParcelableRestriction;
import biz.bokhorst.xprivacy.ParcelableSetting;

interface IPrivacyService {
	int getVersion();
	void migrated();
	List /* String */ check();

	void setRestriction(in ParcelableRestriction restriction);
	void setRestrictionList(in List<ParcelableRestriction> listRestriction);
	boolean getRestriction(in ParcelableRestriction restriction, boolean usage);
	List<ParcelableRestriction> getRestrictionList(int uid, String restrictionName);
	void deleteRestrictions(int uid);

	long getUsage(int uid, in List<ParcelableRestriction> restriction);
	List<ParcelableRestriction> getUsageList(int uid);
	void deleteUsage(int uid);

	void setSetting(in ParcelableSetting setting);
	void setSettingList(in List<ParcelableSetting> listSetting);
	ParcelableSetting getSetting(in ParcelableSetting setting);
	List<ParcelableSetting> getSettingList(int uid);
	void deleteSettings(int uid);
}

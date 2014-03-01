package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.PRestriction;
import biz.bokhorst.xprivacy.PSetting;

interface IPrivacyService {
	int getVersion();
	List /* String */ check();
	void reportError(String message);

	void setRestriction(in PRestriction restriction);
	void setRestrictionList(in List<PRestriction> listRestriction);
	PRestriction getRestriction(in PRestriction restriction, boolean usage, String secret);
	List<PRestriction> getRestrictionList(in PRestriction selector);
	void deleteRestrictions(int uid, String restrictionName);

	long getUsage(in List<PRestriction> restriction);
	List<PRestriction> getUsageList(int uid, String restrictionName);
	void deleteUsage(int uid);

	void setSetting(in PSetting setting);
	void setSettingList(in List<PSetting> listSetting);
	PSetting getSetting(in PSetting setting);
	List<PSetting> getSettingList(int uid);
	void deleteSettings(int uid);

	void clear();
	void reboot();
	void dump(int uid);
}

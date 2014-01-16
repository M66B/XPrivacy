package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.ParcelableUsageData;

interface IPrivacyService {
	void setRestriction(int uid, String restrictionName, String methodName, boolean restricted);
	boolean getRestriction(int uid, String restrictionName, String methodName, boolean usage);
	void deleteRestrictions(int uid);

	long getUsage(int uid, String restrictionName, String methodName);
	List<ParcelableUsageData> getAllUsage(int uid);
	void deleteUsage(int uid);

	void setSetting(int uid, String name, String value);
	String getSetting(int uid, String name, String defaultValue);
	Map getSettings(int uid);
	void deleteSettings(int uid);
}

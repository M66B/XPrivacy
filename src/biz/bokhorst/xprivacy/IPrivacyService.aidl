package biz.bokhorst.xprivacy;

import biz.bokhorst.xprivacy.ParcelableUsageData;

interface IPrivacyService {
	String ping(String pong);
	int getVersion();

	void setRestriction(int uid, String restrictionName, String methodName, boolean restricted);
	boolean getRestriction(int uid, String restrictionName, String methodName, boolean usage);
	List getRestrictionList(int uid, String restrictionName);
	void deleteRestrictions(int uid);

	long getUsage(int uid, String restrictionName, String methodName);
	List<ParcelableUsageData> getUsageList(int uid);
	void deleteUsage(int uid);

	void setSetting(int uid, String name, String value);
	String getSetting(int uid, String name, String defaultValue);
	Map getSettings(int uid);
	void deleteSettings(int uid);
}

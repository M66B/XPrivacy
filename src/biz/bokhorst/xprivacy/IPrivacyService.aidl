package biz.bokhorst.xprivacy;

interface IPrivacyService {

	void setRestricted(
		in String hookName,
		in int uid,
		in String restrictionName,
		in String methodName,
		in boolean restricted);

	boolean getRestricted(
		in String hookName,
		in int uid,
		in String restrictionName,
		in String methodName,
		boolean usage,
		in boolean useCache);

	void setSetting(
		in String hookName,
		in int uid,
		in String settingName,
		in String value);

	String getSetting(
		in String hookName,
		in int uid,
		in String name,
		in String defaultValue,
		in boolean useCache);
}

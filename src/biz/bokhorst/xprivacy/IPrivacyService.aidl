package biz.bokhorst.xprivacy;

interface IPrivacyService {
	boolean getRestricted(in int uid, in String restrictionName, in String methodName);
	String getSetting(in String name, in String defaultValue);
}

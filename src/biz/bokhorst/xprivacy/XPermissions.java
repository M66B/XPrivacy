package biz.bokhorst.xprivacy;

import java.util.LinkedHashMap;
import java.util.Map;

public class XPermissions {

	// This should correspond with perm_<name> in strings.xml
	public static final String cBrowser = "browser";
	public static final String cCalendar = "calendar";
	public static final String cCalllog = "calllog";
	public static final String cContacts = "contacts";
	public static final String cIdentification = "identification";
	public static final String cLocation = "location";
	public static final String cMessages = "messages";
	public static final String cVoicemail = "voicemail";

	public static Map<String, String[]> cPermissions = new LinkedHashMap<String, String[]>();

	static {
		cPermissions.put(cBrowser, new String[] { "READ_HISTORY_BOOKMARKS", "GLOBAL_SEARCH" });
		cPermissions.put(cCalendar, new String[] { "READ_CALENDAR" });
		cPermissions.put(cCalllog, new String[] { "READ_CALL_LOG" });
		cPermissions.put(cContacts, new String[] { "READ_CONTACTS" });
		cPermissions.put(cIdentification, new String[] {}); // READ_PHONE_STATE
		cPermissions.put(cLocation, new String[] { "ACCESS_FINE_LOCATION", "ACCESS_COARSE_LOCATION" });
		cPermissions.put(cMessages, new String[] { "READ_SMS" });
		cPermissions.put(cVoicemail, new String[] { "READ_WRITE_ALL_VOICEMAIL" });
	}
}

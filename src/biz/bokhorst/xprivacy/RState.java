package biz.bokhorst.xprivacy;

public class RState {
	public boolean restricted;
	public boolean asked;
	public boolean partial = false;

	public static RState get(int uid, String restrictionName, String methodName) {
		RState state = new RState();

		// Get if on demand
		boolean onDemand = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingOnDemand, true, false);
		if (onDemand)
			onDemand = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingOnDemand, false, false);

		boolean crestricted = false;
		boolean allRestricted = true;
		boolean someRestricted = false;
		boolean someToAsk = false;

		if (methodName != null) {
			// Examine the method state
			PRestriction query = PrivacyManager.getRestrictionEx(uid, restrictionName, methodName);
			allRestricted = query.restricted;
			someToAsk = !query.asked;
		} else {
			// Examine the category/method states
			for (PRestriction restriction : PrivacyManager.getRestrictionList(uid, restrictionName)) {
				boolean asked = (restriction.asked || !onDemand);
				allRestricted = (allRestricted && (restriction.restricted && asked));
				someRestricted = (someRestricted || (restriction.restricted && asked));
				someToAsk = (someToAsk || !restriction.asked);
			}
			// Get the category state, if relevant
			if (restrictionName != null) {
				PRestriction query = PrivacyManager.getRestrictionEx(uid, restrictionName, null);
				crestricted = query.restricted;
			}
		}

		state.restricted = crestricted || allRestricted || someRestricted;
		state.partial = (state.restricted && !allRestricted);
		state.asked = !(onDemand && someToAsk);

		return state;
	}
}

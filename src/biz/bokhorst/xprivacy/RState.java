package biz.bokhorst.xprivacy;

public class RState {
	public int uid;
	public String restrictionName;
	public String methodName;
	public boolean restricted;
	public boolean asked;
	public boolean partial = false;

	public static RState get(int uid, String restrictionName, String methodName) {
		RState state = new RState();
		state.uid = uid;
		state.restrictionName = restrictionName;
		state.methodName = methodName;

		// Get if on demand
		boolean onDemand = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingOnDemand, true, false);
		if (onDemand)
			onDemand = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingOnDemand, false, false);

		boolean allRestricted = true;
		boolean someRestricted = false;
		boolean asked = false;

		if (methodName == null) {
			if (restrictionName == null) {
				// Examine the category state
				asked = true;
				for (String rRestrictionName : PrivacyManager.getRestrictions()) {
					PRestriction query = PrivacyManager.getRestrictionEx(uid, rRestrictionName, null);
					allRestricted = (allRestricted && query.restricted);
					someRestricted = (someRestricted || query.restricted);
					if (!query.asked)
						asked = false;
				}
			} else {
				// Examine the category/method states
				PRestriction query = PrivacyManager.getRestrictionEx(uid, restrictionName, null);
				someRestricted = query.restricted;
				for (PRestriction restriction : PrivacyManager.getRestrictionList(uid, restrictionName)) {
					allRestricted = (allRestricted && restriction.restricted);
					someRestricted = (someRestricted || restriction.restricted);
				}
				asked = query.asked;
			}
		} else {
			// Examine the method state
			PRestriction query = PrivacyManager.getRestrictionEx(uid, restrictionName, methodName);
			allRestricted = query.restricted;
			someRestricted = false;
			asked = query.asked;
		}

		state.restricted = (allRestricted || someRestricted);
		state.partial = (!allRestricted && someRestricted);
		state.asked = (!onDemand || asked);
		return state;
	}

	public RState next() {
		RState next = new RState();
		next.restricted = !this.restricted;
		next.asked = true;
		return next;
	}
}

package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

public class RState {
	public int mUid;
	public String mRestrictionName;
	public String mMethodName;
	public boolean crestricted;
	public boolean restricted;
	public boolean asked;
	public boolean partial = false;
	public boolean proposeRestricted;
	public boolean proposeAsked;

	public RState(int uid, String restrictionName, String methodName) {
		mUid = uid;
		mRestrictionName = restrictionName;
		mMethodName = methodName;

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
				crestricted = query.restricted;
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

		restricted = (allRestricted || someRestricted);
		partial = (!allRestricted && someRestricted);
		asked = (!onDemand || asked);

		proposeRestricted = !this.restricted;
		proposeAsked = true;
	}

	public void apply() {

		// Apply changes
		if (mMethodName == null) {

			// Get restrictions to change
			List<String> listRestriction;
			if (mRestrictionName == null)
				listRestriction = PrivacyManager.getRestrictions();
			else {
				listRestriction = new ArrayList<String>();
				listRestriction.add(mRestrictionName);
			}

			if (proposeRestricted)
				for (String restrictionName : listRestriction)
					PrivacyManager.setRestriction(mUid, restrictionName, null, proposeRestricted, proposeAsked);
			else
				PrivacyManager.deleteRestrictions(mUid, mRestrictionName);

		} else {

			PrivacyManager.setRestriction(mUid, mRestrictionName, mMethodName, proposeRestricted, proposeAsked);

			if (!crestricted) {
				// We should restrict the category too
			}
		}
	}
}

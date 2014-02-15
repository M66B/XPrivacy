package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class RState {
	private int mUid;
	private String mRestrictionName;
	private String mMethodName;
	public boolean restricted;
	public boolean asked;
	public boolean partial = false;

	public static RState get(int uid, String restrictionName, String methodName) {
		RState state = new RState();
		state.mUid = uid;
		state.mRestrictionName = restrictionName;
		state.mMethodName = methodName;

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

	public RState apply(RState next, String from) {
		RState newState = this.next();

		// Apply changes
		if (from.equals("Main")) {

			// Get restrictions to change
			List<String> listRestriction;
			if (mRestrictionName == null)
				listRestriction = PrivacyManager.getRestrictions();
			else {
				listRestriction = new ArrayList<String>();
				listRestriction.add(mRestrictionName);
			}

			if (next.restricted)
				for (String restrictionName : listRestriction)
					PrivacyManager.setRestriction(mUid, restrictionName, null, next.restricted, next.asked);
			else
				PrivacyManager.deleteRestrictions(mUid, mRestrictionName);

		} else if (from.equals("AppCategory")) {

			if (next.restricted)
				PrivacyManager.setRestriction(mUid, mRestrictionName, null, next.restricted, next.asked);
			else
				PrivacyManager.deleteRestrictions(mUid, mRestrictionName);

		} else if (from.equals("AppMethod")) {

			PrivacyManager.setRestriction(mUid, mRestrictionName, mMethodName, next.restricted, next.asked);

		} else
			Util.log(null, Log.ERROR, "Change request of unknown provenance");

		return newState;
	}
}

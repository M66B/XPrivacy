package biz.bokhorst.xprivacy;

import android.os.Parcel;
import android.os.Parcelable;

public class ParcelableRestriction implements Parcelable {
	public int uid;
	public String restrictionName;
	public String methodName;
	public boolean restricted;
	public long time;

	public ParcelableRestriction() {
	}

	public static final Parcelable.Creator<ParcelableRestriction> CREATOR = new Parcelable.Creator<ParcelableRestriction>() {
		public ParcelableRestriction createFromParcel(Parcel in) {
			return new ParcelableRestriction(in);
		}

		public ParcelableRestriction[] newArray(int size) {
			return new ParcelableRestriction[size];
		}
	};

	private ParcelableRestriction(Parcel in) {
		readFromParcel(in);
	}

	@Override
	public void writeToParcel(Parcel out, int flags) {
		out.writeInt(uid);
		out.writeString(restrictionName);
		out.writeString(methodName);
		out.writeInt(restricted ? 1 : 0);
		out.writeLong(time);
	}

	public void readFromParcel(Parcel in) {
		uid = in.readInt();
		restrictionName = in.readString();
		methodName = in.readString();
		restricted = (in.readInt() > 0 ? true : false);
		time = in.readLong();
	}

	@Override
	public int describeContents() {
		return 0;
	}
}

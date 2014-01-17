package biz.bokhorst.xprivacy;

import android.os.Parcel;
import android.os.Parcelable;

public class ParcelableUsageData implements Parcelable {
	public int uid;
	public String restrictionName;
	public String methodName;
	public boolean restricted;
	public long time;

	public ParcelableUsageData() {
	}

	public static final Parcelable.Creator<ParcelableUsageData> CREATOR = new Parcelable.Creator<ParcelableUsageData>() {
		public ParcelableUsageData createFromParcel(Parcel in) {
			return new ParcelableUsageData(in);
		}

		public ParcelableUsageData[] newArray(int size) {
			return new ParcelableUsageData[size];
		}
	};

	private ParcelableUsageData(Parcel in) {
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

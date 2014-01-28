package biz.bokhorst.xprivacy;

import android.os.Parcel;
import android.os.Parcelable;

public class ParcelableSetting implements Parcelable {
	public int uid;
	public String name;
	public String value;

	public ParcelableSetting() {
	}

	public ParcelableSetting(int _uid, String _name, String _value) {
		uid = _uid;
		name = _name;
		value = _value;
	}

	public static final Parcelable.Creator<ParcelableSetting> CREATOR = new Parcelable.Creator<ParcelableSetting>() {
		public ParcelableSetting createFromParcel(Parcel in) {
			return new ParcelableSetting(in);
		}

		public ParcelableSetting[] newArray(int size) {
			return new ParcelableSetting[size];
		}
	};

	private ParcelableSetting(Parcel in) {
		readFromParcel(in);
	}

	@Override
	public void writeToParcel(Parcel out, int flags) {
		out.writeInt(uid);
		out.writeString(name);
		out.writeInt(value == null ? 1 : 0);
		if (value != null)
			out.writeString(value);
	}

	public void readFromParcel(Parcel in) {
		uid = in.readInt();
		name = in.readString();
		if (in.readInt() > 0)
			value = null;
		else
			value = in.readString();
	}

	@Override
	public int describeContents() {
		return 0;
	}
}

package biz.bokhorst.xprivacy;

import android.os.Parcel;
import android.os.Parcelable;
import android.util.Log;

public class PSetting implements Parcelable {
	public int uid;
	public String type;
	public String name;
	public String value;

	public PSetting() {
	}

	public PSetting(PSetting other) {
		uid = other.uid;
		type = other.type;
		name = other.name;
		value = other.value;
	}

	public PSetting(int _uid, String _type, String _name, String _value) {
		uid = _uid;
		type = _type;
		name = _name;
		value = _value;
		if (type == null) {
			Util.log(null, Log.WARN, "PSetting null");
			Util.logStack(null, Log.WARN);
		}
	}

	public static final Parcelable.Creator<PSetting> CREATOR = new Parcelable.Creator<PSetting>() {
		public PSetting createFromParcel(Parcel in) {
			return new PSetting(in);
		}

		public PSetting[] newArray(int size) {
			return new PSetting[size];
		}
	};

	private PSetting(Parcel in) {
		readFromParcel(in);
	}

	@Override
	public void writeToParcel(Parcel out, int flags) {
		out.writeInt(uid);
		out.writeString(type);
		out.writeString(name);
		out.writeInt(value == null ? 1 : 0);
		if (value != null)
			out.writeString(value);
	}

	public void readFromParcel(Parcel in) {
		uid = in.readInt();
		type = in.readString();
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

	@Override
	public String toString() {
		return "uid=" + uid + " " + type + "/" + name + "=" + (value == null ? "null" : value);
	}
}

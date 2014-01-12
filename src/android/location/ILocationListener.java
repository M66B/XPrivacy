package android.location;

import android.os.Bundle;

public interface ILocationListener extends android.os.IInterface {
	void onLocationChanged(Location location);

	void onStatusChanged(String provider, int status, Bundle extras);

	void onProviderEnabled(String provider);

	void onProviderDisabled(String provider);
}

package com.android.internal.telephony;

import java.util.List;

import android.os.Bundle;
import android.telephony.CellInfo;
import android.telephony.ServiceState;
import android.telephony.SignalStrength;

public interface IPhoneStateListener extends android.os.IInterface {
	void onServiceStateChanged(ServiceState serviceState);

	void onSignalStrengthChanged(int asu);

	void onMessageWaitingIndicatorChanged(boolean mwi);

	void onCallForwardingIndicatorChanged(boolean cfi);

	// we use bundle here instead of CellLocation so it can get the right
	// subclass
	void onCellLocationChanged(Bundle location);

	void onCallStateChanged(int state, String incomingNumber);

	void onDataConnectionStateChanged(int state, int networkType);

	void onDataActivity(int direction);

	void onSignalStrengthsChanged(SignalStrength signalStrength);

	void onOtaspChanged(int otaspMode);

	void onCellInfoChanged(List<CellInfo> cellInfo);
}

package biz.bokhorst.xprivacy;

import android.content.ContentResolver;
import android.database.CharArrayBuffer;
import android.database.ContentObserver;
import android.database.Cursor;
import android.database.DataSetObserver;
import android.net.Uri;
import android.os.Bundle;

public class XCursor implements Cursor {

	private Cursor mCursor;

	public XCursor(Cursor cursor) {
		mCursor = cursor;
	}

	@Override
	public void close() {
		mCursor.close();
	}

	@Override
	public void copyStringToBuffer(int arg0, CharArrayBuffer arg1) {
		mCursor.copyStringToBuffer(arg0, arg1);
	}

	@Override
	@Deprecated
	public void deactivate() {
		mCursor.deactivate();
	}

	@Override
	public byte[] getBlob(int arg0) {
		return mCursor.getBlob(arg0);
	}

	@Override
	public int getColumnCount() {
		return mCursor.getColumnCount();
	}

	@Override
	public int getColumnIndex(String arg0) {
		return mCursor.getColumnIndex(arg0);
	}

	@Override
	public int getColumnIndexOrThrow(String arg0) throws IllegalArgumentException {
		return mCursor.getColumnIndexOrThrow(arg0);
	}

	@Override
	public String getColumnName(int arg0) {
		return mCursor.getColumnName(arg0);
	}

	@Override
	public String[] getColumnNames() {
		return mCursor.getColumnNames();
	}

	@Override
	public int getCount() {
		return 0;
	}

	@Override
	public double getDouble(int arg0) {
		return mCursor.getDouble(arg0);
	}

	@Override
	public Bundle getExtras() {
		return mCursor.getExtras();
	}

	@Override
	public float getFloat(int arg0) {
		return mCursor.getFloat(arg0);
	}

	@Override
	public int getInt(int arg0) {
		return mCursor.getInt(0);
	}

	@Override
	public long getLong(int arg0) {
		return mCursor.getLong(arg0);
	}

	@Override
	public int getPosition() {
		return mCursor.getPosition();
	}

	@Override
	public short getShort(int arg0) {
		return mCursor.getShort(arg0);
	}

	@Override
	public String getString(int arg0) {
		return mCursor.getString(arg0);
	}

	@Override
	public int getType(int arg0) {
		return mCursor.getType(arg0);
	}

	@Override
	public boolean getWantsAllOnMoveCalls() {
		return mCursor.getWantsAllOnMoveCalls();
	}

	@Override
	public boolean isAfterLast() {
		return mCursor.isAfterLast();
	}

	@Override
	public boolean isBeforeFirst() {
		return mCursor.isBeforeFirst();
	}

	@Override
	public boolean isClosed() {
		return mCursor.isClosed();
	}

	@Override
	public boolean isFirst() {
		return mCursor.isFirst();
	}

	@Override
	public boolean isLast() {
		return mCursor.isLast();
	}

	@Override
	public boolean isNull(int arg0) {
		return mCursor.isNull(arg0);
	}

	@Override
	public boolean move(int arg0) {
		return false;
	}

	@Override
	public boolean moveToFirst() {
		return false;
	}

	@Override
	public boolean moveToLast() {
		return false;
	}

	@Override
	public boolean moveToNext() {
		return false;
	}

	@Override
	public boolean moveToPosition(int arg0) {
		return false;
	}

	@Override
	public boolean moveToPrevious() {
		return false;
	}

	@Override
	public void registerContentObserver(ContentObserver arg0) {
	}

	@Override
	public void registerDataSetObserver(DataSetObserver arg0) {
	}

	@Override
	@Deprecated
	public boolean requery() {
		return mCursor.requery();
	}

	@Override
	public Bundle respond(Bundle arg0) {
		return Bundle.EMPTY;
	}

	@Override
	public void setNotificationUri(ContentResolver arg0, Uri arg1) {
	}

	@Override
	public void unregisterContentObserver(ContentObserver arg0) {
	}

	@Override
	public void unregisterDataSetObserver(DataSetObserver arg0) {
	}
}

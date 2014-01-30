package biz.bokhorst.xprivacy;

// Based on:
// https://github.com/rovo89/XposedBridge/blob/master/src/de/robv/android/xposed/XSharedPreferences.java

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import android.content.SharedPreferences;
import android.os.Process;
import android.util.Log;

import com.android.internal.util.XmlUtils;

/**
 * This class is basically the same as SharedPreferencesImpl from AOSP, but
 * read-only and without listeners support. Instead, it is made to be compatible
 * with all ROMs.
 */
public final class SharedPreferencesEx implements SharedPreferences {
	private final File mFile;
	private final File mBackupFile;
	private Map<String, Object> mMap;
	private boolean mLoaded = false;
	private long mLastModified;
	private long mFileSize;

	private static int cTryMaxCount = 10;
	private static int cTryWaitMs = 50;

	public SharedPreferencesEx(File prefFile) {
		mFile = prefFile;
		mBackupFile = makeBackupFile(prefFile);
		startLoadFromDisk();
	}

	public SharedPreferencesEx(String packageName, String prefFileName) {
		mFile = new File(Util.getUserDataDirectory(Process.myUid()) + File.pathSeparator + "shared_prefs"
				+ File.pathSeparator + prefFileName + ".xml");
		mBackupFile = makeBackupFile(mFile);
		startLoadFromDisk();
	}

	private void startLoadFromDisk() {
		synchronized (this) {
			mLoaded = false;
		}
		new Thread("SharedPreferencesEx-load") {
			@Override
			public void run() {
				synchronized (SharedPreferencesEx.this) {
					loadFromDiskLocked();
				}
			}
		}.start();
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void loadFromDiskLocked() {
		int tries = 0;
		while (++tries <= cTryMaxCount && !mLoaded && (mFile.exists() || mBackupFile.exists())) {
			// Log retry
			if (tries > 1)
				Util.log(null, Log.WARN, "Load " + mFile + " try=" + tries + " exists=" + mFile.exists() + " readable="
						+ mFile.canRead() + " backup=" + mBackupFile.exists());

			// Read file if possible
			if (mFile.exists() && mFile.canRead() && !mBackupFile.exists()) {
				Map map = null;
				long lastModified = mFile.lastModified();
				long fileSize = mFile.length();
				BufferedInputStream str = null;
				try {
					str = new BufferedInputStream(new FileInputStream(mFile), 16 * 1024);
					map = XmlUtils.readMapXml(str);
				} catch (Throwable ex) {
					Util.log(null, Log.WARN, "Error reading " + mFile + ": " + ex);
				} finally {
					if (str != null) {
						try {
							str.close();
						} catch (RuntimeException rethrown) {
							throw rethrown;
						} catch (Throwable ex) {
							Util.log(null, Log.WARN, "Error closing " + mFile + ": " + ex);
						}
					}
				}
				if (map != null) {
					mLoaded = true;
					mMap = map;
					mLastModified = lastModified;
					mFileSize = fileSize;
					notifyAll();
				}
			}

			// Wait for next try
			if (!mLoaded && tries < cTryMaxCount)
				try {
					Thread.sleep(cTryWaitMs);
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}
		}

		// File not read
		if (!mLoaded) {
			if (tries >= cTryMaxCount)
				// Not loaded: try to load again on next access
				Util.log(null, Log.ERROR, "Not loaded " + mFile);
			else
				mLoaded = true;
			mMap = new HashMap<String, Object>();
			notifyAll();
		}
	}

	private static File makeBackupFile(File prefsFile) {
		return new File(prefsFile.getPath() + ".bak");
	}

	/**
	 * Reload the settings from file if they have changed.
	 */
	public void reload() {
		synchronized (this) {
			if (hasFileChanged())
				startLoadFromDisk();
		}
	}

	private boolean hasFileChanged() {
		// canRead returns false for non existing files
		if (!mFile.canRead() || mBackupFile.exists())
			return true;

		long lastModified = mFile.lastModified();
		long fileSize = mFile.length();
		synchronized (this) {
			return (mLastModified != lastModified || mFileSize != fileSize);
		}
	}

	private void awaitLoadedLocked() {
		while (!mLoaded)
			try {
				wait();
			} catch (InterruptedException unused) {
			}
	}

	@Override
	public Map<String, ?> getAll() {
		synchronized (this) {
			awaitLoadedLocked();
			return new HashMap<String, Object>(mMap);
		}
	}

	@Override
	public String getString(String key, String defValue) {
		synchronized (this) {
			awaitLoadedLocked();
			String v = (String) mMap.get(key);
			return v != null ? v : defValue;
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	public Set<String> getStringSet(String key, Set<String> defValues) {
		synchronized (this) {
			awaitLoadedLocked();
			Set<String> v = (Set<String>) mMap.get(key);
			return v != null ? v : defValues;
		}
	}

	@Override
	public int getInt(String key, int defValue) {
		synchronized (this) {
			awaitLoadedLocked();
			Integer v = (Integer) mMap.get(key);
			return v != null ? v : defValue;
		}
	}

	@Override
	public long getLong(String key, long defValue) {
		synchronized (this) {
			awaitLoadedLocked();
			Long v = (Long) mMap.get(key);
			return v != null ? v : defValue;
		}
	}

	@Override
	public float getFloat(String key, float defValue) {
		synchronized (this) {
			awaitLoadedLocked();
			Float v = (Float) mMap.get(key);
			return v != null ? v : defValue;
		}
	}

	@Override
	public boolean getBoolean(String key, boolean defValue) {
		synchronized (this) {
			awaitLoadedLocked();
			Boolean v = (Boolean) mMap.get(key);
			return v != null ? v : defValue;
		}
	}

	@Override
	public boolean contains(String key) {
		synchronized (this) {
			awaitLoadedLocked();
			return mMap.containsKey(key);
		}
	}

	@Override
	public Editor edit() {
		throw new UnsupportedOperationException("read-only implementation");
	}

	@Override
	public void registerOnSharedPreferenceChangeListener(OnSharedPreferenceChangeListener listener) {
		throw new UnsupportedOperationException("listeners are not supported in this implementation");
	}

	@Override
	public void unregisterOnSharedPreferenceChangeListener(OnSharedPreferenceChangeListener listener) {
		throw new UnsupportedOperationException("listeners are not supported in this implementation");
	}
}

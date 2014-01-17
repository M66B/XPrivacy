package biz.bokhorst.xprivacy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.TimeUnit;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.AccountManagerFuture;
import android.accounts.AuthenticatorException;
import android.accounts.OnAccountsUpdateListener;
import android.accounts.OperationCanceledException;
import android.os.Binder;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XAccountManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final Map<OnAccountsUpdateListener, XOnAccountsUpdateListener> mListener = new WeakHashMap<OnAccountsUpdateListener, XOnAccountsUpdateListener>();

	private XAccountManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	private XAccountManager(Methods method, String restrictionName, String className, int sdk) {
		super(restrictionName, method.name(), null, sdk);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public void addOnAccountsUpdatedListener(final OnAccountsUpdateListener listener, Handler handler, boolean updateImmediately)
	// public String blockingGetAuthToken(Account account, String authTokenType, boolean notifyAuthFailure)
	// public Account[] getAccounts()
	// public Account[] getAccountsByType(String type)
	// public Account[] getAccountsByTypeForPackage(String type, String packageName)
	// public AccountManagerFuture<Account[]> getAccountsByTypeAndFeatures(final String type, final String[] features, AccountManagerCallback<Account[]> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final Bundle options, final Activity activity, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final boolean notifyAuthFailure, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final Bundle options, final boolean notifyAuthFailure, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthTokenByFeatures(final String accountType, final String authTokenType, final String[] features, final Activity activity, final Bundle addAccountOptions, final Bundle getAuthTokenOptions, final AccountManagerCallback<Bundle> callback, final Handler handler)
	// public AccountManagerFuture<Boolean> hasFeatures(final Account account, final String[] features, AccountManagerCallback<Boolean> callback, Handler handler)
	// public void removeOnAccountsUpdatedListener(OnAccountsUpdateListener listener)
	// frameworks/base/core/java/android/accounts/AccountManager.java
	// http://developer.android.com/reference/android/accounts/AccountManager.html

	// @formatter:on

	private enum Methods {
		addOnAccountsUpdatedListener, blockingGetAuthToken, getAccounts, getAccountsByType, getAccountsByTypeForPackage, getAccountsByTypeAndFeatures, getAuthToken, getAuthTokenByFeatures, hasFeatures, removeOnAccountsUpdatedListener
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XAccountManager(Methods.addOnAccountsUpdatedListener, PrivacyManager.cAccounts, className));
		listHook.add(new XAccountManager(Methods.blockingGetAuthToken, PrivacyManager.cAccounts, className));
		listHook.add(new XAccountManager(Methods.getAccounts, PrivacyManager.cAccounts, className));
		listHook.add(new XAccountManager(Methods.getAccountsByType, PrivacyManager.cAccounts, className));
		listHook.add(new XAccountManager(Methods.getAccountsByTypeForPackage, PrivacyManager.cAccounts, className,
				Build.VERSION_CODES.JELLY_BEAN_MR2));
		listHook.add(new XAccountManager(Methods.getAccountsByTypeAndFeatures, PrivacyManager.cAccounts, className));
		listHook.add(new XAccountManager(Methods.getAuthToken, PrivacyManager.cAccounts, className));
		listHook.add(new XAccountManager(Methods.getAuthTokenByFeatures, PrivacyManager.cAccounts, className));
		listHook.add(new XAccountManager(Methods.hasFeatures, PrivacyManager.cAccounts, className));
		listHook.add(new XAccountManager(Methods.removeOnAccountsUpdatedListener, PrivacyManager.cAccounts, className));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.addOnAccountsUpdatedListener) {
			if (param.args.length > 0 && param.args[0] != null)
				if (isRestricted(param)) {
					OnAccountsUpdateListener listener = (OnAccountsUpdateListener) param.args[0];
					XOnAccountsUpdateListener xlistener = new XOnAccountsUpdateListener(listener,
							Binder.getCallingUid());
					synchronized (mListener) {
						mListener.put(listener, xlistener);
						Util.log(this, Log.INFO, "Added count=" + mListener.size());
					}
					param.args[0] = xlistener;
				}
		} else if (mMethod == Methods.removeOnAccountsUpdatedListener) {
			if (param.args.length > 0 && param.args[0] != null)
				if (isRestricted(param)) {
					synchronized (mListener) {
						OnAccountsUpdateListener listener = (OnAccountsUpdateListener) param.args[0];
						XOnAccountsUpdateListener xlistener = mListener.get(listener);
						if (xlistener == null)
							Util.log(this, Log.WARN, "Not found count=" + mListener.size());
						else {
							param.args[0] = xlistener;
							mListener.remove(listener);
						}
					}
				}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod != Methods.addOnAccountsUpdatedListener && mMethod != Methods.removeOnAccountsUpdatedListener) {
			int uid = Binder.getCallingUid();
			if (mMethod == Methods.blockingGetAuthToken) {
				if (param.getResult() != null && isRestricted(param))
					if (param.args.length > 0 && param.args[0] != null) {
						Account account = (Account) param.args[0];
						if (!isAccountAllowed(account, uid))
							param.setResult(null);
					}
			} else if (mMethod == Methods.getAccounts || mMethod == Methods.getAccountsByType
					|| mMethod == Methods.getAccountsByTypeForPackage) {
				if (param.getResult() != null && isRestricted(param)) {
					Account[] accounts = (Account[]) param.getResult();
					param.setResult(filterAccounts(accounts, uid));
				}
			} else if (mMethod == Methods.getAccountsByTypeAndFeatures) {
				if (param.getResult() != null && isRestricted(param)) {
					AccountManagerFuture<Account[]> future = (AccountManagerFuture<Account[]>) param.getResult();
					param.setResult(new XFutureAccount(future, uid));
				}
			} else if (mMethod == Methods.getAuthToken || mMethod == Methods.getAuthTokenByFeatures) {
				if (param.getResult() != null && isRestricted(param)) {
					AccountManagerFuture<Bundle> future = (AccountManagerFuture<Bundle>) param.getResult();
					param.setResult(new XFutureBundle(future, uid));
				}
			} else if (mMethod == Methods.hasFeatures) {
				if (param.getResult() != null && isRestricted(param))
					if (param.args.length > 0 && param.args[0] != null) {
						Account account = (Account) param.args[0];
						if (!isAccountAllowed(account, uid))
							param.setResult(new XFutureBoolean());
					}
			} else
				Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
		}
	}

	private Account[] filterAccounts(Account[] original, int uid) {
		List<Account> listAccount = new ArrayList<Account>();
		for (Account account : original)
			if (isAccountAllowed(account, uid))
				listAccount.add(account);
		return listAccount.toArray(new Account[0]);
	}

	private boolean isAccountAllowed(Account account, int uid) {
		return isAccountAllowed(account.name, account.type, uid);
	}

	private boolean isAccountAllowed(String accountName, String accountType, int uid) {
		try {
			String sha1 = Util.sha1(accountName + accountType);
			if (PrivacyManager.getSettingBool(this, uid, PrivacyManager.cSettingAccount + sha1, false, true))
				return true;
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
		return false;
	}

	private class XFutureAccount implements AccountManagerFuture<Account[]> {
		private AccountManagerFuture<Account[]> mFuture;
		private int mUid;

		public XFutureAccount(AccountManagerFuture<Account[]> future, int uid) {
			mFuture = future;
			mUid = uid;
		}

		@Override
		public boolean cancel(boolean mayInterruptIfRunning) {
			return mFuture.cancel(mayInterruptIfRunning);
		}

		@Override
		public Account[] getResult() throws OperationCanceledException, IOException, AuthenticatorException {
			return XAccountManager.this.filterAccounts(mFuture.getResult(), mUid);
		}

		@Override
		public Account[] getResult(long timeout, TimeUnit unit) throws OperationCanceledException, IOException,
				AuthenticatorException {
			return XAccountManager.this.filterAccounts(mFuture.getResult(timeout, unit), mUid);
		}

		@Override
		public boolean isCancelled() {
			return mFuture.isCancelled();
		}

		@Override
		public boolean isDone() {
			return mFuture.isDone();
		}
	}

	private class XFutureBoolean implements AccountManagerFuture<Boolean> {

		@Override
		public boolean cancel(boolean mayInterruptIfRunning) {
			return false;
		}

		@Override
		public Boolean getResult() throws OperationCanceledException, IOException, AuthenticatorException {
			return false;
		}

		@Override
		public Boolean getResult(long timeout, TimeUnit unit) throws OperationCanceledException, IOException,
				AuthenticatorException {
			return false;
		}

		@Override
		public boolean isCancelled() {
			return false;
		}

		@Override
		public boolean isDone() {
			return true;
		}
	}

	private class XFutureBundle implements AccountManagerFuture<Bundle> {

		private AccountManagerFuture<Bundle> mFuture;
		private int mUid;

		public XFutureBundle(AccountManagerFuture<Bundle> future, int uid) {
			mFuture = future;
			mUid = uid;
		}

		@Override
		public boolean cancel(boolean mayInterruptIfRunning) {
			return mFuture.cancel(mayInterruptIfRunning);
		}

		@Override
		public Bundle getResult() throws OperationCanceledException, IOException, AuthenticatorException {
			Bundle bundle = mFuture.getResult();
			String accountName = bundle.getString(AccountManager.KEY_ACCOUNT_NAME);
			String accountType = bundle.getString(AccountManager.KEY_ACCOUNT_TYPE);
			if (XAccountManager.this.isAccountAllowed(accountName, accountType, mUid))
				return bundle;
			else
				throw new OperationCanceledException();
		}

		@Override
		public Bundle getResult(long timeout, TimeUnit unit) throws OperationCanceledException, IOException,
				AuthenticatorException {
			Bundle bundle = mFuture.getResult(timeout, unit);
			String accountName = bundle.getString(AccountManager.KEY_ACCOUNT_NAME);
			String accountType = bundle.getString(AccountManager.KEY_ACCOUNT_TYPE);
			if (XAccountManager.this.isAccountAllowed(accountName, accountType, mUid))
				return bundle;
			else
				throw new OperationCanceledException();
		}

		@Override
		public boolean isCancelled() {
			return mFuture.isCancelled();
		}

		@Override
		public boolean isDone() {
			return mFuture.isDone();
		}
	}

	private class XOnAccountsUpdateListener implements OnAccountsUpdateListener {
		private OnAccountsUpdateListener mListener;
		private int mUid;

		public XOnAccountsUpdateListener(OnAccountsUpdateListener listener, int uid) {
			mListener = listener;
			mUid = uid;
		}

		@Override
		public void onAccountsUpdated(Account[] accounts) {
			mListener.onAccountsUpdated(XAccountManager.this.filterAccounts(accounts, mUid));
		}
	}
}

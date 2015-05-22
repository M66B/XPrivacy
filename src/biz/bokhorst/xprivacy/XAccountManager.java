package biz.bokhorst.xprivacy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.TimeUnit;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.AccountManagerCallback;
import android.accounts.AccountManagerFuture;
import android.accounts.AuthenticatorDescription;
import android.accounts.AuthenticatorException;
import android.accounts.OnAccountsUpdateListener;
import android.accounts.OperationCanceledException;
import android.os.Binder;
import android.os.Bundle;
import android.util.Log;

public class XAccountManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.accounts.AccountManager";
	private static final Map<OnAccountsUpdateListener, XOnAccountsUpdateListener> mListener = new WeakHashMap<OnAccountsUpdateListener, XOnAccountsUpdateListener>();

	private XAccountManager(Methods method, String restrictionName) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
		mClassName = "com.android.server.accounts.AccountManagerService";
	}

	private XAccountManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
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
	// public AuthenticatorDescription[] getAuthenticatorTypes()
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final Bundle options, final Activity activity, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final boolean notifyAuthFailure, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final Bundle options, final boolean notifyAuthFailure, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthTokenByFeatures(final String accountType, final String authTokenType, final String[] features, final Activity activity, final Bundle addAccountOptions, final Bundle getAuthTokenOptions, final AccountManagerCallback<Bundle> callback, final Handler handler)
	// public AccountManagerFuture<Boolean> hasFeatures(final Account account, final String[] features, AccountManagerCallback<Boolean> callback, Handler handler)
	// public void removeOnAccountsUpdatedListener(OnAccountsUpdateListener listener)
	// frameworks/base/core/java/android/accounts/AccountManager.java
	// http://developer.android.com/reference/android/accounts/AccountManager.html

	// @formatter:on

	// @formatter:off

	// public android.accounts.Account[] getAccounts(java.lang.String accountType) throws android.os.RemoteException;
	// public android.accounts.Account[] getAccountsAsUser(java.lang.String accountType, int userId) throws android.os.RemoteException;
	// public void getAccountsByFeatures(android.accounts.IAccountManagerResponse response, java.lang.String accountType, java.lang.String[] features) throws android.os.RemoteException;
	// public android.accounts.Account[] getAccountsForPackage(java.lang.String packageName, int uid)
	// public android.accounts.Account[] getSharedAccountsAsUser(int userId) throws android.os.RemoteException;
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/server/accounts/AccountManagerService.java
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/android/accounts/IAccountManager.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		addOnAccountsUpdatedListener,
		blockingGetAuthToken,
		getAccounts, getAccountsByType, getAccountsByTypeForPackage, getAccountsByTypeAndFeatures, getAuthenticatorTypes,
		getAuthToken, getAuthTokenByFeatures,
		hasFeatures,
		removeOnAccountsUpdatedListener,

		Srv_getAccounts,
		Srv_getAccountsAsUser,
		Srv_getAccountsByFeatures,
		Srv_getAccountsForPackage,
		Srv_getSharedAccountsAsUser
	};
	// @formatter:on

	public static List<XHook> getInstances(String className, boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			if (server) {
				listHook.add(new XAccountManager(Methods.Srv_getAccounts, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAccountsAsUser, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAccountsByFeatures, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAccountsForPackage, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getSharedAccountsAsUser, PrivacyManager.cAccounts));
			} else {
				listHook.add(new XAccountManager(Methods.addOnAccountsUpdatedListener, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.blockingGetAuthToken, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.getAccounts, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.getAccountsByType, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.getAccountsByTypeForPackage, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.getAccountsByTypeAndFeatures, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.getAuthenticatorTypes, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.getAuthToken, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.getAuthTokenByFeatures, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.hasFeatures, PrivacyManager.cAccounts, className));
				listHook.add(new XAccountManager(Methods.removeOnAccountsUpdatedListener, null, className));
			}
		}
		return listHook;
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case addOnAccountsUpdatedListener:
			if (param.args.length > 0 && param.args[0] != null)
				if (isRestricted(param)) {
					int uid = Binder.getCallingUid();
					OnAccountsUpdateListener listener = (OnAccountsUpdateListener) param.args[0];
					XOnAccountsUpdateListener xListener;
					synchronized (mListener) {
						xListener = mListener.get(listener);
						if (xListener == null) {
							xListener = new XOnAccountsUpdateListener(listener, uid);
							mListener.put(listener, xListener);
							Util.log(this, Log.WARN, "Added count=" + mListener.size() + " uid=" + uid);
						}
					}
					param.args[0] = xListener;
				}
			break;

		case blockingGetAuthToken:
		case getAccounts:
		case getAccountsByType:
		case getAccountsByTypeForPackage:
			// Do nothing
			break;

		case getAccountsByTypeAndFeatures:
			if (param.args.length > 2 && param.args[2] != null)
				if (isRestrictedExtra(param, (String) param.args[0])) {
					AccountManagerCallback<Account[]> callback = (AccountManagerCallback<Account[]>) param.args[2];
					param.args[2] = new XAccountManagerCallbackAccount(callback, Binder.getCallingUid());
				}
			break;

		case getAuthenticatorTypes:
			// Do nothing
			break;

		case getAuthToken:
			if (param.args.length > 0) {
				Account account = (Account) param.args[0];
				for (int i = 1; i < param.args.length; i++)
					if (param.args[i] instanceof AccountManagerCallback<?>)
						if (isRestrictedExtra(param, account == null ? null : account.name)) {
							AccountManagerCallback<Bundle> callback = (AccountManagerCallback<Bundle>) param.args[i];
							param.args[i] = new XAccountManagerCallbackBundle(callback, Binder.getCallingUid());
						}
			}
			break;

		case getAuthTokenByFeatures:
			if (param.args.length > 0)
				for (int i = 1; i < param.args.length; i++)
					if (param.args[i] instanceof AccountManagerCallback<?>)
						if (isRestrictedExtra(param, (String) param.args[0])) {
							AccountManagerCallback<Bundle> callback = (AccountManagerCallback<Bundle>) param.args[i];
							param.args[i] = new XAccountManagerCallbackBundle(callback, Binder.getCallingUid());
						}
			break;

		case hasFeatures:
			if (param.args.length > 0) {
				Account account = (Account) param.args[0];
				for (int i = 1; i < param.args.length; i++)
					if (param.args[i] instanceof AccountManagerCallback<?>)
						if (isRestrictedExtra(param, account == null ? null : account.name)) {
							AccountManagerCallback<Boolean> callback = (AccountManagerCallback<Boolean>) param.args[i];
							param.args[i] = new XAccountManagerCallbackBoolean(callback);
						}
			}
			break;

		case removeOnAccountsUpdatedListener:
			if (param.args.length > 0 && param.args[0] != null)
				synchronized (mListener) {
					OnAccountsUpdateListener listener = (OnAccountsUpdateListener) param.args[0];
					XOnAccountsUpdateListener xListener = mListener.get(listener);
					if (xListener != null) {
						param.args[0] = xListener;
						Util.log(this, Log.WARN, "Removed count=" + mListener.size() + " uid=" + Binder.getCallingUid());
					}
				}
			break;

		case Srv_getAccounts:
		case Srv_getAccountsAsUser:
		case Srv_getAccountsForPackage:
			// Do nothing
			break;

		case Srv_getAccountsByFeatures:
			if (param.args.length > 1 && (param.args[1] == null || param.args[1] instanceof String)) {
				if (isRestrictedExtra(param, (String) param.args[1]))
					param.setResult(null);
			} else {
				if (isRestricted(param))
					param.setResult(null);
			}
			break;

		case Srv_getSharedAccountsAsUser:
			// Do nothing
			break;
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void after(XParam param) throws Throwable {
		int uid = Binder.getCallingUid();

		switch (mMethod) {
		case addOnAccountsUpdatedListener:
			// Do nothing
			break;

		case blockingGetAuthToken:
			if (param.getResult() != null && param.args.length > 0 && param.args[0] != null) {
				Account account = (Account) param.args[0];
				if (isRestrictedExtra(param, account == null ? null : account.name))
					if (!isAccountAllowed(account, uid))
						param.setResult(null);
			}
			break;

		case getAccounts:
			if (param.getResult() != null && isRestricted(param)) {
				Account[] accounts = (Account[]) param.getResult();
				param.setResult(filterAccounts(accounts, uid));
			}
			break;

		case getAccountsByType:
		case getAccountsByTypeForPackage:
			if (param.getResult() != null && param.args.length > 0)
				if (isRestrictedExtra(param, (String) param.args[0])) {
					Account[] accounts = (Account[]) param.getResult();
					param.setResult(filterAccounts(accounts, uid));
				}
			break;

		case getAccountsByTypeAndFeatures:
			if (param.getResult() != null && param.args.length > 0)
				if (isRestrictedExtra(param, (String) param.args[0])) {
					AccountManagerFuture<Account[]> future = (AccountManagerFuture<Account[]>) param.getResult();
					param.setResult(new XFutureAccount(future, uid));
				}
			break;

		case getAuthenticatorTypes:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new AuthenticatorDescription[0]);
			break;

		case getAuthToken:
			if (param.getResult() != null && param.args.length > 0) {
				Account account = (Account) param.args[0];
				if (isRestrictedExtra(param, account == null ? null : account.name)) {
					AccountManagerFuture<Bundle> future = (AccountManagerFuture<Bundle>) param.getResult();
					param.setResult(new XFutureBundle(future, uid));
				}
			}
			break;

		case getAuthTokenByFeatures:
			if (param.getResult() != null)
				if (isRestrictedExtra(param, (String) param.args[0])) {
					AccountManagerFuture<Bundle> future = (AccountManagerFuture<Bundle>) param.getResult();
					param.setResult(new XFutureBundle(future, uid));
				}
			break;

		case hasFeatures:
			if (param.getResult() != null && param.args.length > 0 && param.args[0] != null) {
				Account account = (Account) param.args[0];
				if (isRestrictedExtra(param, account == null ? null : account.name))
					if (!isAccountAllowed(account, uid))
						param.setResult(new XFutureBoolean());
			}
			break;

		case removeOnAccountsUpdatedListener:
			// Do nothing
			break;

		case Srv_getAccounts:
		case Srv_getAccountsAsUser:
		case Srv_getAccountsForPackage:
		case Srv_getSharedAccountsAsUser:
			// Filter account list
			String extra = null;
			if (mMethod == Methods.Srv_getAccounts || mMethod == Methods.Srv_getAccountsAsUser
					|| mMethod == Methods.Srv_getAccountsForPackage)
				if (param.args.length > 0 && param.args[0] instanceof String)
					extra = (String) param.args[0];

			if (param.getResult() instanceof Account[])
				if (isRestrictedExtra(param, extra)) {
					Account[] accounts = (Account[]) param.getResult();
					param.setResult(filterAccounts(accounts, uid));
				}
			break;

		case Srv_getAccountsByFeatures:
			// Do nothing
			break;
		}
	}

	private Account[] filterAccounts(Account[] original, int uid) {
		List<Account> listAccount = new ArrayList<Account>();
		for (Account account : original)
			if (isAccountAllowed(account, uid))
				listAccount.add(account);
		return listAccount.toArray(new Account[0]);
	}

	public static boolean isAccountAllowed(Account account, int uid) {
		return isAccountAllowed(account.name, account.type, uid);
	}

	public static boolean isAccountAllowed(String accountName, String accountType, int uid) {
		try {
			boolean allowed = PrivacyManager.getSettingBool(-uid, Meta.cTypeAccountHash, accountName + accountType,
					false);
			boolean blacklist = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingBlacklist, false);
			if (blacklist)
				allowed = !allowed;
			return allowed;
		} catch (Throwable ex) {
			Util.bug(null, ex);
			return false;
		}
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
			Account[] account = mFuture.getResult();
			return XAccountManager.this.filterAccounts(account, mUid);
		}

		@Override
		public Account[] getResult(long timeout, TimeUnit unit) throws OperationCanceledException, IOException,
				AuthenticatorException {
			Account[] account = mFuture.getResult(timeout, unit);
			return XAccountManager.this.filterAccounts(account, mUid);
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
			if (isAccountAllowed(accountName, accountType, mUid))
				return bundle;
			else
				throw new OperationCanceledException("XPrivacy");
		}

		@Override
		public Bundle getResult(long timeout, TimeUnit unit) throws OperationCanceledException, IOException,
				AuthenticatorException {
			Bundle bundle = mFuture.getResult(timeout, unit);
			String accountName = bundle.getString(AccountManager.KEY_ACCOUNT_NAME);
			String accountType = bundle.getString(AccountManager.KEY_ACCOUNT_TYPE);
			if (isAccountAllowed(accountName, accountType, mUid))
				return bundle;
			else
				throw new OperationCanceledException("XPrivacy");
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

	private class XAccountManagerCallbackAccount implements AccountManagerCallback<Account[]> {
		private AccountManagerCallback<Account[]> mCallback;
		private int mUid;

		public XAccountManagerCallbackAccount(AccountManagerCallback<Account[]> callback, int uid) {
			mCallback = callback;
			mUid = uid;
		}

		@Override
		public void run(AccountManagerFuture<Account[]> future) {
			mCallback.run(new XAccountManager.XFutureAccount(future, mUid));
		}
	}

	private class XAccountManagerCallbackBundle implements AccountManagerCallback<Bundle> {
		private AccountManagerCallback<Bundle> mCallback;
		private int mUid;

		public XAccountManagerCallbackBundle(AccountManagerCallback<Bundle> callback, int uid) {
			mCallback = callback;
			mUid = uid;
		}

		@Override
		public void run(AccountManagerFuture<Bundle> future) {
			mCallback.run(new XAccountManager.XFutureBundle(future, mUid));
		}
	}

	private class XAccountManagerCallbackBoolean implements AccountManagerCallback<Boolean> {
		private AccountManagerCallback<Boolean> mCallback;

		public XAccountManagerCallbackBoolean(AccountManagerCallback<Boolean> callback) {
			mCallback = callback;
		}

		@Override
		public void run(AccountManagerFuture<Boolean> future) {
			mCallback.run(new XAccountManager.XFutureBoolean());
		}
	}
}

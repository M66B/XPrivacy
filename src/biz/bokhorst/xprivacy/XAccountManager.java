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

	// public java.lang.String getPassword(android.accounts.Account account) throws android.os.RemoteException;
	// public java.lang.String getUserData(android.accounts.Account account, java.lang.String key) throws android.os.RemoteException;
	// public android.accounts.AuthenticatorDescription[] getAuthenticatorTypes() throws android.os.RemoteException;
	// public android.accounts.Account[] getAccounts(java.lang.String accountType) throws android.os.RemoteException;
	// public android.accounts.Account[] getAccountsForPackage(java.lang.String packageName, int uid) throws android.os.RemoteException;
	// public android.accounts.Account[] getAccountsByTypeForPackage(java.lang.String type, java.lang.String packageName) throws android.os.RemoteException;
	// public android.accounts.Account[] getAccountsAsUser(java.lang.String accountType, int userId) throws android.os.RemoteException;
	// public void hasFeatures(android.accounts.IAccountManagerResponse response, android.accounts.Account account, java.lang.String[] features) throws android.os.RemoteException;
	// public void getAccountsByFeatures(android.accounts.IAccountManagerResponse response, java.lang.String accountType, java.lang.String[] features) throws android.os.RemoteException;
	// public boolean addAccountExplicitly(android.accounts.Account account, java.lang.String password, android.os.Bundle extras) throws android.os.RemoteException;
	// public void removeAccount(android.accounts.IAccountManagerResponse response, android.accounts.Account account) throws android.os.RemoteException;
	// public void invalidateAuthToken(java.lang.String accountType, java.lang.String authToken) throws android.os.RemoteException;
	// public java.lang.String peekAuthToken(android.accounts.Account account, java.lang.String authTokenType) throws android.os.RemoteException;
	// public void setAuthToken(android.accounts.Account account, java.lang.String authTokenType, java.lang.String authToken) throws android.os.RemoteException;
	// public void setPassword(android.accounts.Account account, java.lang.String password) throws android.os.RemoteException;
	// public void clearPassword(android.accounts.Account account) throws android.os.RemoteException;
	// public void setUserData(android.accounts.Account account, java.lang.String key, java.lang.String value) throws android.os.RemoteException;
	// public void updateAppPermission(android.accounts.Account account, java.lang.String authTokenType, int uid, boolean value) throws android.os.RemoteException;
	// public void getAuthToken(android.accounts.IAccountManagerResponse response, android.accounts.Account account, java.lang.String authTokenType, boolean notifyOnAuthFailure, boolean expectActivityLaunch, android.os.Bundle options) throws android.os.RemoteException;
	// public void addAccount(android.accounts.IAccountManagerResponse response, java.lang.String accountType, java.lang.String authTokenType, java.lang.String[] requiredFeatures, boolean expectActivityLaunch, android.os.Bundle options) throws android.os.RemoteException;
	// public void updateCredentials(android.accounts.IAccountManagerResponse response, android.accounts.Account account, java.lang.String authTokenType, boolean expectActivityLaunch, android.os.Bundle options) throws android.os.RemoteException;
	// public void editProperties(android.accounts.IAccountManagerResponse response, java.lang.String accountType, boolean expectActivityLaunch) throws android.os.RemoteException;
	// public void confirmCredentialsAsUser(android.accounts.IAccountManagerResponse response, android.accounts.Account account, android.os.Bundle options, boolean expectActivityLaunch, int userId) throws android.os.RemoteException;
	// public void getAuthTokenLabel(android.accounts.IAccountManagerResponse response, java.lang.String accountType, java.lang.String authTokenType) throws android.os.RemoteException;
	// public boolean addSharedAccountAsUser(android.accounts.Account account, int userId) throws android.os.RemoteException;
	// public android.accounts.Account[] getSharedAccountsAsUser(int userId) throws android.os.RemoteException;
	// public boolean removeSharedAccountAsUser(android.accounts.Account account, int userId) throws android.os.RemoteException;
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/server/accounts/AccountManagerService.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		addOnAccountsUpdatedListener,
		blockingGetAuthToken,
		getAccounts, getAccountsByType, getAccountsByTypeForPackage, getAccountsByTypeAndFeatures, getAuthenticatorTypes,
		getAuthToken, getAuthTokenByFeatures,
		hasFeatures,
		removeOnAccountsUpdatedListener,

		Srv_getAuthenticatorTypes,
		Srv_getAccounts, Srv_getAccountsForPackage, Srv_getAccountsByTypeForPackage, Srv_getAccountsAsUser,
		Srv_getAccountsByFeatures,
		Srv_peekAuthToken, Srv_getAuthToken, Srv_getAuthTokenLabel,
		Srv_getSharedAccountsAsUser
	};
	// @formatter:on

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			if (isAOSPKitKat()) {
				listHook.add(new XAccountManager(Methods.Srv_getAuthenticatorTypes, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAccounts, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAccountsForPackage, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAccountsByTypeForPackage, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAccountsAsUser, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAccountsByFeatures, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_peekAuthToken, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAuthToken, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getAuthTokenLabel, PrivacyManager.cAccounts));
				listHook.add(new XAccountManager(Methods.Srv_getSharedAccountsAsUser, PrivacyManager.cAccounts));
			}

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
			listHook.add(new XAccountManager(Methods.removeOnAccountsUpdatedListener, null, className, 5));
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

		case getAccountsByTypeAndFeatures:
			if (param.args.length > 2 && param.args[2] != null)
				if (isRestrictedExtra(param, (String) param.args[0])) {
					AccountManagerCallback<Account[]> callback = (AccountManagerCallback<Account[]>) param.args[2];
					param.args[2] = new XAccountManagerCallbackAccount(callback, Binder.getCallingUid());
				}
			break;

		case getAuthToken:
			if (param.args.length > 0) {
				Account account = (Account) param.args[0];
				for (int i = 0; i < param.args.length; i++)
					if (param.args[i] instanceof AccountManagerCallback<?>)
						if (isRestrictedExtra(param, account == null ? null : account.name)) {
							AccountManagerCallback<Bundle> callback = (AccountManagerCallback<Bundle>) param.args[i];
							param.args[i] = new XAccountManagerCallbackBundle(callback, Binder.getCallingUid());
						}
			}
			break;

		case getAuthTokenByFeatures:
			if (param.args.length > 0)
				for (int i = 0; i < param.args.length; i++)
					if (param.args[i] instanceof AccountManagerCallback<?>)
						if (isRestrictedExtra(param, (String) param.args[0])) {
							AccountManagerCallback<Bundle> callback = (AccountManagerCallback<Bundle>) param.args[i];
							param.args[i] = new XAccountManagerCallbackBundle(callback, Binder.getCallingUid());
						}
			break;

		case hasFeatures:
			if (param.args.length > 0) {
				Account account = (Account) param.args[0];
				for (int i = 0; i < param.args.length; i++)
					if (param.args[i] instanceof AccountManagerCallback<?>)
						if (isRestrictedExtra(param, account == null ? null : account.name)) {
							AccountManagerCallback<Boolean> callback = (AccountManagerCallback<Boolean>) param.args[i];
							param.args[i] = new XAccountManagerCallbackBoolean(callback);
						}
			}
			break;

		default:
			break;
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void after(XParam param) throws Throwable {
		if (mMethod != Methods.addOnAccountsUpdatedListener && mMethod != Methods.removeOnAccountsUpdatedListener) {
			int uid = Binder.getCallingUid();

			if (mMethod == Methods.Srv_getAuthenticatorTypes) {
				// TODO: Srv_getAuthenticatorTypes

			} else if (mMethod == Methods.Srv_getAccounts || mMethod == Methods.Srv_getAccountsForPackage
					|| mMethod == Methods.Srv_getAccountsByTypeForPackage || mMethod == Methods.Srv_getAccountsAsUser
					|| mMethod == Methods.Srv_getSharedAccountsAsUser) {
				if (param.getResult() != null && isRestricted(param)) {
					Account[] accounts = (Account[]) param.getResult();
					param.setResult(filterAccounts(accounts, uid));
				}

			} else if (mMethod == Methods.Srv_getAccountsByFeatures) {
				// TODO: Srv_getAccountsByFeatures

			} else if (mMethod == Methods.Srv_peekAuthToken) {
				// TODO: Srv_peekAuthToken

			} else if (mMethod == Methods.Srv_getAuthToken || mMethod == Methods.Srv_getAuthTokenLabel) {
				// TODO: Srv_getAuthToken
				// TODO: Srv_getAuthTokenLabel

			} else if (mMethod == Methods.blockingGetAuthToken) {
				if (param.args.length > 0 && param.args[0] != null) {
					Account account = (Account) param.args[0];
					if (param.getResult() != null && isRestrictedExtra(param, account == null ? null : account.name))
						if (!isAccountAllowed(account, uid))
							param.setResult(null);
				}

			} else if (mMethod == Methods.getAccounts) {
				if (param.getResult() != null && isRestricted(param)) {
					Account[] accounts = (Account[]) param.getResult();
					param.setResult(filterAccounts(accounts, uid));
				}

			} else if (mMethod == Methods.getAccountsByType || mMethod == Methods.getAccountsByTypeForPackage) {
				if (param.args.length > 0)
					if (param.getResult() != null && isRestrictedExtra(param, (String) param.args[0])) {
						Account[] accounts = (Account[]) param.getResult();
						param.setResult(filterAccounts(accounts, uid));
					}

			} else if (mMethod == Methods.getAccountsByTypeAndFeatures) {
				if (param.args.length > 0)
					if (param.getResult() != null && isRestrictedExtra(param, (String) param.args[0])) {
						AccountManagerFuture<Account[]> future = (AccountManagerFuture<Account[]>) param.getResult();
						param.setResult(new XFutureAccount(future, uid));
					}

			} else if (mMethod == Methods.getAuthenticatorTypes) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(new AuthenticatorDescription[0]);

			} else if (mMethod == Methods.getAuthToken) {
				if (param.args.length > 0) {
					Account account = (Account) param.args[0];
					if (param.getResult() != null && isRestrictedExtra(param, account == null ? null : account.name)) {
						AccountManagerFuture<Bundle> future = (AccountManagerFuture<Bundle>) param.getResult();
						param.setResult(new XFutureBundle(future, uid));
					}
				}

			} else if (mMethod == Methods.getAuthTokenByFeatures) {
				if (param.getResult() != null && isRestrictedExtra(param, (String) param.args[0])) {
					AccountManagerFuture<Bundle> future = (AccountManagerFuture<Bundle>) param.getResult();
					param.setResult(new XFutureBundle(future, uid));
				}

			} else if (mMethod == Methods.hasFeatures) {
				if (param.args.length > 0 && param.args[0] != null) {
					Account account = (Account) param.args[0];
					if (param.getResult() != null && isRestrictedExtra(param, account == null ? null : account.name))
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
			boolean allowed = PrivacyManager.getSettingBool(-uid, Meta.cTypeAccountHash, accountName + accountType,
					false);
			boolean blacklist = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingBlacklist, false);
			if (blacklist)
				allowed = !allowed;
			return allowed;
		} catch (Throwable ex) {
			Util.bug(this, ex);
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
			if (XAccountManager.this.isAccountAllowed(accountName, accountType, mUid))
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
			if (XAccountManager.this.isAccountAllowed(accountName, accountType, mUid))
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

package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.io.IOException;
import java.lang.reflect.Field;
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
import android.content.Context;
import android.os.Binder;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XAccountManager extends XHook {
	private static final Map<OnAccountsUpdateListener, XOnAccountsUpdateListener> mListener = new WeakHashMap<OnAccountsUpdateListener, XOnAccountsUpdateListener>();

	private XAccountManager(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	private XAccountManager(String methodName, String restrictionName, int sdk) {
		super(restrictionName, methodName, null, sdk);
	}

	public String getClassName() {
		return "android.accounts.AccountManager";
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

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XAccountManager("addOnAccountsUpdatedListener", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("blockingGetAuthToken", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("getAccounts", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("getAccountsByType", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("getAccountsByTypeAndFeatures", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("getAuthToken", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("getAuthTokenByFeatures", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("hasFeatures", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("removeOnAccountsUpdatedListener", PrivacyManager.cAccounts));
		listHook.add(new XAccountManager("getAccountsByTypeForPackage", PrivacyManager.cAccounts,
				Build.VERSION_CODES.JELLY_BEAN_MR2));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("addOnAccountsUpdatedListener")) {
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
		} else if (methodName.equals("removeOnAccountsUpdatedListener")) {
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
		String methodName = param.method.getName();
		if (!methodName.equals("addOnAccountsUpdatedListener") && !methodName.equals("removeOnAccountsUpdatedListener")) {
			int uid = Binder.getCallingUid();
			if (methodName.equals("blockingGetAuthToken")) {
				if (param.getResult() != null && isRestricted(param))
					if (param.args.length > 0 && param.args[0] != null) {
						Account account = (Account) param.args[0];
						if (!isAccountAllowed(account, uid))
							param.setResult(null);
					}
			} else if (methodName.equals("getAccounts") || methodName.equals("getAccountsByType")
					|| methodName.equals("getAccountsByTypeForPackage")) {
				if (param.getResult() != null && isRestricted(param)) {
					Account[] accounts = (Account[]) param.getResult();
					param.setResult(filterAccounts(accounts, uid));
				}
			} else if (methodName.equals("getAccountsByTypeAndFeatures")) {
				if (param.getResult() != null && isRestricted(param)) {
					AccountManagerFuture<Account[]> future = (AccountManagerFuture<Account[]>) param.getResult();
					param.setResult(new XFutureAccount(future, uid));
				}
			} else if (methodName.equals("getAuthToken") || methodName.equals("getAuthTokenByFeatures")) {
				if (param.getResult() != null && isRestricted(param)) {
					AccountManagerFuture<Bundle> future = (AccountManagerFuture<Bundle>) param.getResult();
					param.setResult(new XFutureBundle(future, uid));
				}
			} else if (methodName.equals("hasFeatures")) {
				if (param.getResult() != null && isRestricted(param))
					if (param.args.length > 0 && param.args[0] != null) {
						Account account = (Account) param.args[0];
						if (!isAccountAllowed(account, uid))
							param.setResult(new XFutureBoolean());
					}
			} else
				Util.log(this, Log.WARN, "Unknown method=" + methodName);
		}
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = getContext(param);
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	private Context getContext(MethodHookParam param) {
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			return (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			Util.bug(this, ex);
			return null;
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
			if (PrivacyManager.getSettingBool(this, null, String.format("Account.%d.%s", uid, sha1), false, true))
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

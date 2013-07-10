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
import android.os.Bundle;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XAccountManager extends XHook {
	private static final Map<OnAccountsUpdateListener, XOnAccountsUpdateListener> mListener = new WeakHashMap<OnAccountsUpdateListener, XOnAccountsUpdateListener>();

	public XAccountManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// @formatter:off

	// public void addOnAccountsUpdatedListener(final OnAccountsUpdateListener listener, Handler handler, boolean updateImmediately)
	// public String blockingGetAuthToken(Account account, String authTokenType, boolean notifyAuthFailure)
	// public Account[] getAccounts()
	// public Account[] getAccountsByType(String type)
	// public AccountManagerFuture<Account[]> getAccountsByTypeAndFeatures(final String type, final String[] features, AccountManagerCallback<Account[]> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final Bundle options, final Activity activity, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final boolean notifyAuthFailure, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final Bundle options, final boolean notifyAuthFailure, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthTokenByFeatures(final String accountType, final String authTokenType, final String[] features, final Activity activity, final Bundle addAccountOptions, final Bundle getAuthTokenOptions, final AccountManagerCallback<Bundle> callback, final Handler handler)
	// public AccountManagerFuture<Boolean> hasFeatures(final Account account, final String[] features, AccountManagerCallback<Boolean> callback, Handler handler)
	// public void removeOnAccountsUpdatedListener(OnAccountsUpdateListener listener)
	// frameworks/base/core/java/android/accounts/AccountManager.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("addOnAccountsUpdatedListener")) {
			if (param.args[0] != null)
				if (isRestricted(param)) {
					OnAccountsUpdateListener listener = (OnAccountsUpdateListener) param.args[0];
					XOnAccountsUpdateListener xlistener = new XOnAccountsUpdateListener(listener, getContext(param));
					synchronized (mListener) {
						mListener.put(listener, xlistener);
						Util.log(this, Log.INFO, "Added count=" + mListener.size());
					}
					param.args[0] = xlistener;
				}
		} else if (methodName.equals("removeOnAccountsUpdatedListener")) {

			if (param.args[0] != null)
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
		if (!methodName.equals("addOnAccountsUpdatedListener") && !methodName.equals("removeOnAccountsUpdatedListener"))
			if (param.getResult() != null)
				if (isRestricted(param))
					if (methodName.equals("blockingGetAuthToken")) {
						Account account = (Account) param.args[0];
						if (!isAccountAllowed(account, getContext(param)))
							param.setResult(null);
					} else if (methodName.equals("getAccounts") || methodName.equals("getAccountsByType")) {
						Account[] accounts = (Account[]) param.getResult();
						param.setResult(filterAccounts(accounts, getContext(param)));
					} else if (methodName.equals("getAccountsByTypeAndFeatures")) {
						AccountManagerFuture<Account[]> future = (AccountManagerFuture<Account[]>) param.getResult();
						param.setResult(new XFutureAccount(future, getContext(param)));
					} else if (methodName.equals("getAuthToken") || methodName.equals("getAuthTokenByFeatures")) {
						AccountManagerFuture<Bundle> future = (AccountManagerFuture<Bundle>) param.getResult();
						param.setResult(new XFutureBundle(future, getContext(param)));
					} else if (methodName.equals("hasFeatures")) {
						Account account = (Account) param.args[0];
						if (!isAccountAllowed(account, getContext(param)))
							param.setResult(new XFutureBoolean());
					} else
						Util.log(this, Log.WARN, "Unknown method=" + methodName);
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

	private Account[] filterAccounts(Account[] original, Context context) {
		List<Account> listAccount = new ArrayList<Account>();
		for (Account account : original)
			if (isAccountAllowed(account, context))
				listAccount.add(account);
		return listAccount.toArray(new Account[0]);
	}

	private boolean isAccountAllowed(Account account, Context context) {
		return isAccountAllowed(account.name, account.type, context);
	}

	private boolean isAccountAllowed(String accountName, String accountType, Context context) {
		if (context == null)
			return false;
		try {
			String sha1 = Util.sha1(accountName + accountType);
			if (PrivacyManager.getSettingBool(this, context, String.format("%s.%s", context.getPackageName(), sha1),
					false, true))
				return true;
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
		return false;
	}

	private class XFutureAccount implements AccountManagerFuture<Account[]> {
		private AccountManagerFuture<Account[]> mFuture;
		private Context mContext;

		public XFutureAccount(AccountManagerFuture<Account[]> future, Context context) {
			mFuture = future;
			mContext = context;
		}

		@Override
		public boolean cancel(boolean mayInterruptIfRunning) {
			return mFuture.cancel(mayInterruptIfRunning);
		}

		@Override
		public Account[] getResult() throws OperationCanceledException, IOException, AuthenticatorException {
			return XAccountManager.this.filterAccounts(mFuture.getResult(), mContext);
		}

		@Override
		public Account[] getResult(long timeout, TimeUnit unit) throws OperationCanceledException, IOException,
				AuthenticatorException {
			return XAccountManager.this.filterAccounts(mFuture.getResult(timeout, unit), mContext);
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
		private Context mContext;

		public XFutureBundle(AccountManagerFuture<Bundle> future, Context context) {
			mFuture = future;
			mContext = context;
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
			if (XAccountManager.this.isAccountAllowed(accountName, accountType, mContext))
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
			if (XAccountManager.this.isAccountAllowed(accountName, accountType, mContext))
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
		private Context mContext;

		public XOnAccountsUpdateListener(OnAccountsUpdateListener listener, Context context) {
			mListener = listener;
			mContext = context;
		}

		@Override
		public void onAccountsUpdated(Account[] accounts) {
			mListener.onAccountsUpdated(XAccountManager.this.filterAccounts(accounts, mContext));
		}
	}
}

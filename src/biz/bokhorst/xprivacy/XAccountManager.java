package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.TimeUnit;

import android.accounts.Account;
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
			if (isRestricted(param))
				if (param.args[0] != null) {
					OnAccountsUpdateListener listener = (OnAccountsUpdateListener) param.args[0];
					XOnAccountsUpdateListener xlistener = new XOnAccountsUpdateListener(listener);
					synchronized (mListener) {
						mListener.put(listener, xlistener);
						XUtil.log(this, Log.INFO, "Added count=" + mListener.size());
					}
					param.args[0] = xlistener;
				}
		} else if (methodName.equals("removeOnAccountsUpdatedListener")) {
			if (isRestricted(param))
				if (param.args[0] != null) {
					synchronized (mListener) {
						OnAccountsUpdateListener listener = (OnAccountsUpdateListener) param.args[0];
						XOnAccountsUpdateListener xlistener = mListener.get(listener);
						if (xlistener == null)
							param.args[0] = xlistener;
						else
							XUtil.log(this, Log.INFO, "Removed count=" + mListener.size());
					}
				}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (!methodName.equals("addOnAccountsUpdatedListener") && !methodName.equals("removeOnAccountsUpdatedListener"))
			if (param.getResult() != null)
				if (isRestricted(param))
					if (methodName.equals("blockingGetAuthToken"))
						param.setResult(null);
					else if (methodName.equals("getAccounts") || methodName.equals("getAccountsByType"))
						param.setResult(new Account[0]);
					else if (methodName.equals("getAccountsByTypeAndFeatures"))
						param.setResult(new XFutureAccount());
					else if (methodName.equals("getAuthToken") || methodName.equals("getAuthTokenByFeatures"))
						param.setResult(new XFutureBundle());
					else if (methodName.equals("hasFeatures"))
						param.setResult(new XFutureBoolean());
					else
						XUtil.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			XUtil.bug(this, ex);
		}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	private class XFutureAccount implements AccountManagerFuture<Account[]> {
		@Override
		public boolean cancel(boolean arg0) {
			return false;
		}

		@Override
		public Account[] getResult() throws OperationCanceledException, IOException, AuthenticatorException {
			return new Account[0];
		}

		@Override
		public Account[] getResult(long arg0, TimeUnit arg1) throws OperationCanceledException, IOException,
				AuthenticatorException {
			return new Account[0];
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

		@Override
		public boolean cancel(boolean mayInterruptIfRunning) {
			return false;
		}

		@Override
		public Bundle getResult() throws OperationCanceledException, IOException, AuthenticatorException {
			return new Bundle();
		}

		@Override
		public Bundle getResult(long timeout, TimeUnit unit) throws OperationCanceledException, IOException,
				AuthenticatorException {
			return new Bundle();
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

	private class XOnAccountsUpdateListener implements OnAccountsUpdateListener {
		private OnAccountsUpdateListener mListener;

		public XOnAccountsUpdateListener(OnAccountsUpdateListener listener) {
			mListener = listener;
		}

		@Override
		public void onAccountsUpdated(Account[] accounts) {
			mListener.onAccountsUpdated(new Account[0]);
		}
	}
}

package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.concurrent.TimeUnit;

import android.accounts.Account;
import android.accounts.AccountManagerFuture;
import android.accounts.AuthenticatorException;
import android.accounts.OperationCanceledException;
import android.content.Context;
import android.os.Binder;
import android.os.Bundle;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XAccountManager extends XHook {

	public XAccountManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// @formatter:off

	// public Account[] getAccounts()
	// public Account[] getAccountsByType(String type)
	// public AccountManagerFuture<Account[]> getAccountsByTypeAndFeatures(final String type, final String[] features, AccountManagerCallback<Account[]> callback, Handler handler)
	// public AccountManagerFuture<Boolean> hasFeatures(final Account account, final String[] features, AccountManagerCallback<Boolean> callback, Handler handler)
	// public void addOnAccountsUpdatedListener(final OnAccountsUpdateListener listener, Handler handler, boolean updateImmediately)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final Bundle options, final Activity activity, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final boolean notifyAuthFailure, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthToken(final Account account, final String authTokenType, final Bundle options, final boolean notifyAuthFailure, AccountManagerCallback<Bundle> callback, Handler handler)
	// public AccountManagerFuture<Bundle> getAuthTokenByFeatures(final String accountType, final String authTokenType, final String[] features, final Activity activity, final Bundle addAccountOptions, final Bundle getAuthTokenOptions, final AccountManagerCallback<Bundle> callback, final Handler handler)
	// public String blockingGetAuthToken(Account account, String authTokenType, boolean notifyAuthFailure)
	// frameworks/base/core/java/android/accounts/AccountManager.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("addOnAccountsUpdatedListener") || param.getResultOrThrowable() != null)
			if (isRestricted(param))
				if (methodName.equals("getAccounts") || methodName.equals("getAccountsByType"))
					param.setResult(new Account[0]);
				else if (methodName.equals("getAccountsByTypeAndFeatures"))
					param.setResult(new XFutureAccount[0]);
				else if (methodName.equals("hasFeatures"))
					param.setResult(new XFutureBoolean[0]);
				else if (methodName.equals("addOnAccountsUpdatedListener"))
					param.setResult(null);
				else if (methodName.equals("getAuthToken") || methodName.equals("getAuthTokenByFeatures"))
					param.setResult(new XFutureBundle[0]);
				else if (methodName.equals("blockingGetAuthToken"))
					param.setResult(null);
				else
					XUtil.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		// CM10/CM10.1
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
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
}

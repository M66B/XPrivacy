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
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XAccountManager extends XHook {

	public XAccountManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// @formatter:off

	// public Account[] getAccounts()
	// public Account[] getAccountsByType(String type)
	// public AccountManagerFuture<Account[]> getAccountsByTypeAndFeatures(final String type, final String[] features, AccountManagerCallback<Account[]> callback, Handler handler)
	// frameworks/base/core/java/android/accounts/AccountManager.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			String methodName = param.method.getName();
			if (methodName.equals("getAccountsByTypeAndFeatures"))
				param.setResult(new XAccountManagerFutureAccount[0]);
			else
				param.setResult(new Account[0]);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	private class XAccountManagerFutureAccount implements AccountManagerFuture<Account[]> {
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
}

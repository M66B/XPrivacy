package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import android.app.usage.ConfigurationStats;
import android.app.usage.UsageStats;

import biz.bokhorst.xprivacy.XHook;

public class XUsageStatsManager extends XHook {
	private Methods mMethod;

	private XUsageStatsManager(Methods method, String restrictionName) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
	}

	@Override
	public boolean isVisible() {
		return !mMethod.name().startsWith("Srv_");
	}

	public String getClassName() {
		if (mMethod.name().startsWith("Srv_"))
			return "com.android.server.usage.UserUsageStatsService";
		else
			return "android.app.usage.UsageStatsManager";
	}

	// @formatter:off

	// public Map<String, UsageStats> queryAndAggregateUsageStats(long beginTime, long endTime)
	// public List<ConfigurationStats> queryConfigurations(int intervalType, long beginTime, long endTime)
	// public UsageEvents queryEvents(long beginTime, long endTime)
	// public List<UsageStats> queryUsageStats(int intervalType, long beginTime, long endTime)
	// https://developer.android.com/reference/android/app/usage/UsageStatsManager.html

	// List<ConfigurationStats> queryConfigurationStats(int userId, int bucketType, long beginTime, long endTime)
	// UsageEvents queryEvents(int userId, long beginTime, long endTime)
	// List<UsageStats> queryUsageStats(int userId, int bucketType, long beginTime, long endTime)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/com/android/server/usage/UsageStatsService.java

	private enum Methods {
		queryAndAggregateUsageStats, queryConfigurations, queryEvents, queryUsageStats,
		Srv_queryConfigurationStats, Srv_queryEvents, Srv_queryUsageStats
	};

	// @formatter:on

	public static List<XHook> getInstances(boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (server) {
			listHook.add(new XUsageStatsManager(Methods.Srv_queryConfigurationStats, PrivacyManager.cSystem));
			listHook.add(new XUsageStatsManager(Methods.Srv_queryEvents, PrivacyManager.cSystem));
			listHook.add(new XUsageStatsManager(Methods.Srv_queryUsageStats, PrivacyManager.cSystem));
		} else {
			listHook.add(new XUsageStatsManager(Methods.queryAndAggregateUsageStats, PrivacyManager.cSystem));
			listHook.add(new XUsageStatsManager(Methods.queryConfigurations, PrivacyManager.cSystem));
			listHook.add(new XUsageStatsManager(Methods.queryEvents, PrivacyManager.cSystem));
			listHook.add(new XUsageStatsManager(Methods.queryUsageStats, PrivacyManager.cSystem));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case queryAndAggregateUsageStats:
		case queryConfigurations:
		case queryUsageStats:
		case Srv_queryConfigurationStats:
		case Srv_queryUsageStats:
			// Do nothing
			break;
		case queryEvents:
			if (isRestricted(param))
				if (param.args.length > 1) {
					param.args[0] = 0;
					param.args[1] = 0;
				}
			break;

		case Srv_queryEvents:
			if (isRestricted(param))
				if (param.args.length > 2) {
					param.args[1] = 0;
					param.args[2] = 0;
				}
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case queryAndAggregateUsageStats:
			if (isRestricted(param))
				param.setResult(new HashMap<String, UsageStats>());
			break;
		case queryConfigurations:
		case Srv_queryConfigurationStats:
			if (isRestricted(param))
				param.setResult(new ArrayList<ConfigurationStats>());
			break;
		case queryEvents:
		case Srv_queryEvents:
			// Do nothing
			break;
		case queryUsageStats:
		case Srv_queryUsageStats:
			if (isRestricted(param))
				param.setResult(new ArrayList<UsageStats>());
			break;
		}
	}
}

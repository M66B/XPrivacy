Changelog
=========

**Release types**

* EXPERIMENTAL: only install if you know how to fix things
* TEST: new or updated features with a higher risk for bugs
* BETA: new or updated features with a lower risk for bugs
* STABLE: all known bugs are fixed; low risk for bugs

**Downloads**

* [Xposed module repository](http://repo.xposed.info/module/biz.bokhorst.xprivacy)
* [GitHub releases](https://github.com/M66B/XPrivacy/releases)
* Main menu *Check for updates*, available with a [pro license](http://www.xprivacy.eu/) only

**Important**

* Xposed version 2.6+ is required
* Updating from a version prior to 2.0.35 the main template will be reset to default
* Updating from a version prior to 2.1.5 on demand restricting will be disabled until the update is completed
* Updating from a version prior to 2.1.8 you will need to setup the IPC restrictions again
* Updating from a version prior to 2.1.21 you will need to setup the IPC restrictions again, except for *Reflection*
* From version 2.0.29 dangerous functions need to be restricted manually (manually includes the template, if defined to do so)
* From version 2.1.20 batch operations (toggle restrictions for multiple applications) require a pro license
	* I have put about 2000 hours into developing and supporting XPrivacy so far. The reason for [accepting donations](http://www.xprivacy.eu/) is to keep myself motivated to keep doing this. Unfortunately the number of donations is quite low and thus not very motivating. So, don't start complaining about this change, but instead think about supporting this huge project. You can still fully protect your privacy with all the free features of XPrivacy. I have made a promise that you will always be able to do so and I intent to keep this promise.
* From version 2.1.21-5 *I don't know* will allow dangerous functions once (other functions are still denied once)
* **Please send the support info when XPrivacy asks for it**

<a name="xprivacy3"></a>
XPrivacy 3
----------

XPrivacy 2.99.x are XPrivacy 3 test/beta versions.
XPrivacy 2 will not be maintained anymore after XPrivacy 3 has been released.
See for some more information about XPrivacy 3 [here](https://github.com/M66B/XPrivacy#FAQ68).

You can install XPrivacy 2.99.x by following these steps:

* Make a full backup, preferable a [NANDroid](http://forum.xda-developers.com/wiki/NANDroid)
* Update to the latest 2.2.x version
* Enter the quirk *test* (all lower case) into the main settings
* Use the main menu *Check for updates*
* Make sure notifications are not suppressed by another Xposed module or an application manager

Version 2.99.x and version 3.x will be available with a [pro license](http://www.xprivacy.eu/) only for the time being.

**Next release**

* Clear will also clear usage data and application specific settings
* Changed application specific fake values icon to a star ([issue](/../../issues/1831))
* Showing changelog if new version installed
* Updated Arabic translation
* Updated Italian translation
* Updated Japanese translation

[Open issues](https://github.com/M66B/XPrivacy/issues?state=open)

**Version 2.99.36 BETA**

* Displaying AOSP mode setting for KitKat and later only
* Added help texts
* Added option to merge template to reset functions (not categories)
* Updated German translation
* Updated Japanese translation
* Updated Lithuanian translation
* Updated Norwegian translation
* Updated simplified Chinese translation
* Updated traditional Chinese translation

**Version 2.99.35 BETA**

* Added support for [Mahdi ROM](https://plus.google.com/u/0/communities/116540622179206449806)
* Added option to enable/disable AOSP mode
* Updated in application documentation
* Updated Dutch translation
* Updated French translation
* Updated German translation
* Updated traditional Chinese translation

**Version 2.99.34 BETA**

* Displaying if an application has specific fake values ([issue](/../../issues/1831))
* Allow own package name for *Srv_getPackageInfo* and *Srv_getApplicationInfo*
* Updated Dutch translation

**Version 2.99.33 BETA**

* Fixed crash caused by legacy restrictions ([issue](/../../issues/1893))
* Updated Slovak translation

**Version 2.99.32 BETA**

* Added parameter package name to *Srv_getPackageInfo* and *Srv_getApplicationInfo*
* Added support for [Android Revolution HD](http://forum.xda-developers.com/showthread.php?t=1925402)
* Fixed all problems reported through the debug info
* Updated French translation
* Updated German translation

**Version 2.99.31 BETA**

* Flush will clear the asked once cache too
* Display if update service is busy in reboot layout ([issue](/../../issues/1887))
* Fixed asking again in some cases ([issue](/../../issues/1885))
* Performance improvement (caching category restrictions)
* Updated Dutch translation
* Updated French translation
* Updated Slovenian translation

**Version 2.99.30 BETA**

* Added help items to template ([issue](/../../issues/1827))
* Added restrictions *Srv_getPackageInfo* and *Srv_getApplicationInfo*  ([issue](/../../issues/1834))

**Version 2.99.29 BETA**

* Added application specific quirks ([issue](/../../issues/1844))
* Added intent for update check ([issue](/../../issues/1867))
* Changed the default to *noresolve* and added quirk *resolve*
* Fixed all problems reported through the debug info
* Updated Simplified Chinese translation

**Version 2.99.27 BETA**

* Added restriction *registerListener* to the *Sensors* category, which will limit the rate of the gyroscope to 100 Hz to prevent eavesdropping ([issue](/../../issues/1878))
* Added restriction [GMS5.view](https://developer.android.com/reference/com/google/android/gms/appindexing/AppIndexApi.html) ([issue](/../../issues/1778))
* Updated French translation
* Updated Slovenian translation

**Version 2.99.26 BETA**

* Added restriction *GMS5.getLastLocation* and *GMS5.requestLocationUpdates* ([issue](/../../issues/1774))
* Added restriction *GMS.requestActivityUpdates* ([issue](/../../issues/1774))

**Version 2.99.25 BETA**

* Performance optimizations

**Version 2.99.24 BETA**

* Fixed asking again for allow/deny once longer than 15 seconds ([issue](/../../issues/1873))
* Force drop down mode for on demand duration

**Version 2.99.23 BETA**

* Fixed a problem reported through the debug info
	* *Unknown method=Camera.stopPreview*
* Updated Dutch translation
* Updated Japanese translation

**Version 2.99.22 BETA**

* Option to select duration to allow/deny once ([issue](/../../issues/1873))

**Version 2.99.21 BETA**

* Restored restriction *Camera.setPreviewCallback*
* Added restrictions *Camera.setPreviewCallbackWithBuffer*, *Camera.setPreviewDisplay*, *Camera.setPreviewTexture* and *Camera.setOneShotPreviewCallback*
* Handling *Camera.stopPreview*
* Handling *MediaRecorder.prepare* and *MediaRecorder.stop*
* Handling *Audio.stop*

**Version 2.99.20 BETA**

* Restored restriction *MediaRecorder.setOutputFile* ([issue](/../../issues/1874))

**Version 2.99.19 BETA**

* Local transient values only

**Version 2.99.18 BETA**

* Fixed hooking contacts and telephony providers

**Version 2.99.17 BETA**

* Fixed support info warning (transient values)

**Version 2.99.16 BETA**

* Better browser provider compatibility

**Version 2.99.15 BETA**

* Added support for [Dirty Unicorns](http://www.teamdirt.me/)
* Added support for [Liquid Smooth](http://liquidsmooth.net/)
* Added support for some CyanogenMod based ROMs, like Spirit ROM
* Moved *getAllByName*, *getByAddress* and *getByName* to *internet*
	* Since these functions were moved recently, it is not possible to automatically update them
* Performance optimizations (introduced transient values)
* Fixed all problems reported through the support data
* Allow own uid for *getPackagesForUid* and *queryContentProviders* ([issue](/../../issues/1871))
* Updated Dutch translation
* Updated Italian translation

**Version 2.99.14 BETA**

* Fixed restriction *WiFi.Srv_getConnectionInfo*
* Fixed restriction *WiFi.Srv_getDhcpInfo*
* Fixed restriction *BrowserProvider2*

**Version 2.99.13 BETA**

* Added support for [Carbon ROM](https://carbonrom.org/)
* Updated Dutch translation

**Version 2.99.12 BETA**

* Removed restrictions *MapV1.getLatitudeE6* and *MapV1.getLongitudeE6*, since these are not needed and bad for performance ([issue](/../../issues/1862))
* Handling *MapV1.disableMyLocation* when *MapV1.enableMyLocation* is restricted

**Version 2.99.11 TEST**

* Added support for [SlimKat](http://www.slimroms.net/)

**Version 2.99.10 TEST** (only available for testers)

* Added icon for unsafe restrictions
* Added system property restrictions *Srv_Default_DNS* and *Srv_WiFi_Country*
* Added restriction *Bluetooth.Srv_getName*
* Added restriction *Srv_getCompleteVoiceMailNumber*
* Added restriction *WiFi.Srv_getBatchedScanResults*
* Replaced unsafe restrictions by safe restrictions where possible (not in legacy mode)
* Updated German translation
* Updated Slovak translation

<a name="xprivacy2"></a>
XPrivacy 2
----------

**Next release**

* Displaying if an application has specific fake values ([issue](/../../issues/1831))
* Added help texts
* Updated in application documentation
* Added option to merge template to reset functions (not categories)
* Clear will also clear usage data and application specific settings
* Updated Arabic translation
* Updated Dutch translation
* Updated French translation
* Updated German translation
* Updated Italian translation
* Updated Japanese translation
* Updated Lithuanian translation
* Updated Norwegian translation
* Updated simplified Chinese translation
* Updated traditional Chinese translation

[Open issues](https://github.com/M66B/XPrivacy/issues?state=open)

**Version 2.2.12 BETA**

* Fixed crash caused by legacy restrictions ([issue](/../../issues/1893))
* Updated Slovak translation

**Version 2.2.11 BETA**

* Filtering restrictions available from version 2.99.x only (reported on XDA)

**Version 2.2.10 BETA**

* Filtering restrictions available from version 2.99.x only (reported on XDA)

**Version 2.2.9 TEST**

* Option to select duration to allow/deny once ([issue](/../../issues/1873))
* Added help items to template ([issue](/../../issues/1827))
* Added application specific quirks ([issue](/../../issues/1844))
* Added intent for update check ([issue](/../../issues/1867))
* Changed the default to *noresolve* and added quirk *resolve*
* Flush will clear the asked once cache too
* Display if update service is busy in reboot layout ([issue](/../../issues/1887))
* Fixed asking again in some cases ([issue](/../../issues/1885))
* Performance improvement (caching category restrictions)
* Local transient values only
* Updated Dutch translation
* Updated French translation
* Updated German translation
* Updated Japanese translation
* Updated Simplified Chinese translation
* Updated Slovenian translation

**Version 2.2.8 BETA**

* Moved *getAllByName*, *getByAddress* and *getByName* to *internet*
	* Since these functions were moved recently, it is not possible to update them
* Performance optimizations (introduced transient values)
* Fixed all problems reported through the support data
* Allow own uid for *getPackagesForUid* and *queryContentProviders* ([issue](/../../issues/1871))
* Updated Dutch translation
* Updated Italian translation

**Version 2.2.7 BETA**

* Removed restrictions *MapV1.getLatitudeE6* and *MapV1.getLongitudeE6*, since these are not needed and bad for performance ([issue](/../../issues/1862))
* Handling *MapV1.disableMyLocation* when *MapV1.enableMyLocation* is restricted

**Version 2.2.6 BETA**

* Enabled *Camera.permission*, *Record.Video.permission* and *Record.Audio.permission*

**Version 2.2.5 BETA**

* Enabled on demand restricting for *Camera.permission*, *Record.Video.permission* and *Record.Audio.permission*
* Fixed *Camera.permission*, *Record.Video.permission* and *Record.Audio.permission* for some ROMs ([issue](/../../issues/1855))
* Fixed repeated on demand restricting for functions with a whitelist
* Removed [VACUUM](http://sqlite.org/lang_vacuum.html) for usage data database
* Updated Dutch translation
* Updated Japanese translation
* Updated Simplified Chinese translation

**Version 2.2.4 TEST**

* Fixed alignment of the check boxes in the details view
* Added menu *Check for updates* (only [Pro license](http://www.xprivacy.eu/))
* Fixed restriction *Srv_startActivities* ([issue](/../../issues/1853))
* Updated Dutch translation
* Updated Simplified Chinese translation

**Version 2.2.3 TEST**

* Removed restriction *LinkAddress*, since it is not needed
* Added support for [MEID](http://en.wikipedia.org/wiki/Mobile_equipment_identifier) and removed restrictions *getNetworkType* and *getPhoneType* ([issue](/../../issues/1843))
* Prefixed *getAddress* and *getBondedDevices* with *Bluetooth*
* Replaced *setPreviewCallback*, *setPreviewCallbackWithBuffer*, *setOneShotPreviewCallback* by *startPreview*
* Prefixed *startPreview* and *takePicture* with *Camera*
* Prefixed *startRecording* with *Audio*
* Renamed *setOutputFile* into *MediaRecorder.start*
* Restructured restriction meta data (no visible changes)
* Fixed displaying white list entry state after scrolling ([issue](/../../issues/1850))
* Added restrictions *Camera.permission*, *Record.Audio.permission* and *Record.Video.permission*
* Hiding on demand restricting check box for functions without on demand restricting
* Added Croatian translation
* Updated Slovak translation

**Version 2.2.2 TEST**

The internet and network restrictions were reorganized.
The internet restrictions are for restricting internet access,
the network restrictions are for restricting network information, mainly IP addresses.
Restrictions which were moved from one to the other category will be updated automatically,
but a function can only be restricted if the category is restricted.
This means that if a function was moved from internet to network,
it will only be restricted if the network category was restricted.

* Fixed all problems reported through the support info
* Option to white/black list from usage data (long press entry) ([issue](/../../issues/1821)) (only [Pro license](http://www.xprivacy.eu/))
* Added IPC restrictions for *IApplicationThread*, *IContentProvider* and *IWindowSession* (on demand restricting only)
* Added wildcards for IPC transaction names
* Fixed translating transaction codes into transaction names in most cases
* Restricting [ACTION_PACKAGE_VERIFIED](http://developer.android.com/reference/android/content/Intent.html#ACTION_PACKAGE_VERIFIED) as intended
* *DATA_SMS_RECEIVED*, *SMS_RECEIVED* and *WAP_PUSH_RECEIVED* were always part of Android, but only [documented since KitKat](https://developer.android.com/reference/android/provider/Telephony.Sms.Intents.html)
* Translating isolated uids into originating application uids
* Fixed default on demand restricting for dangerous functions with a whitelist while no on demand restricting for category
* Added restriction [ACTION_DIAL](http://developer.android.com/reference/android/content/Intent.html#ACTION_DIAL)
* Removed restriction *ApplicationsProvider* for KitKat, since the provider doesn't exist anymore
* Made *accounts/hasFeatures* dangerous
* Added restrictions [SMS_DELIVER](https://developer.android.com/reference/android/provider/Telephony.Sms.Intents.html#SMS_DELIVER_ACTION) and [WAP_PUSH_DELIVER](https://developer.android.com/reference/android/provider/Telephony.Sms.Intents.html#WAP_PUSH_DELIVER_ACTION)
* Corrected some restriction permissions
* Moved restrictions *getAllByName*, *getByAddress* and *getByName* ([InetAddress](http://developer.android.com/reference/java/net/InetAddress.html)) from *internet* to *network*
* Fixed restrictions *getByAddress* and *getByName*; to prevent problems these dangerous restrictions need to be set again
* Prefixed *getHardwareAddress*, *getInetAddresses* and *getInterfaceAddresses* (network) with [NetworkInterface](http://developer.android.com/reference/java/net/NetworkInterface.html)
* Prefixed *getByInetAddress*, *getByName* and *getNetworkInterfaces* (internet) with [NetworkInterface](http://developer.android.com/reference/java/net/NetworkInterface.html)
* Added restriction [NetworkInterface.getByIndex](http://developer.android.com/reference/java/net/NetworkInterface.html#getByIndex(int))
* Prefixed *getDetailedState*, *getState*, *isConnected* and *isConnectedOrConnecting* with [NetworkInfo](http://developer.android.com/reference/android/net/NetworkInfo.html)
* Moved *getExtraInfo* to *network*
* Prefixed *getActiveNetworkInfo*, *getAllNetworkInfo* and *getNetworkInfo* with *Connectivity*
* Prefixed *getConfiguredNetworks*, *getConnectionInfo*, *getDhcpInfo*, *getScanResults*, and *getWifiApConfiguration* with *WiFi*
* Updated in application documentation
* Updated Dutch translation
* Updated German translation
* Updated Simplified Chinese translation
* Updated Slovak translation

**Version 2.2.1 STABLE**

* Fixed all problems reported through the support info
* Removed restriction *IPC.Reflection* to fix delays/freezes ([issue](/../../issues/1820))
* Better handling of [StrictMode](http://developer.android.com/reference/android/os/StrictMode.html)
* Updated Polish translation

**Version 2.2 STABLE**

Changes since last stable version:

* Added a series of new restrictions and improved/extended existing restrictions
* Redesigned on demand restricting dialog
* Added experimental support for [Cydia Substrate](http://www.cydiasubstrate.com/)
* Several improvements and bug fixes
* See for all details the changes since version 2.1.5

Changes since previous version:

* Updated German translation
* Updated Italian translation
* Updated Japanese translation
* Updated Norwegian translation
* Remember on demand dialog settings across reboots ([issue](/../../issues/1812))
* Showing stored version in *About* when different from current version
* Removed function help from on demand dialog
* Displaying less details in on demand dialog in expert mode

[Older changes](CHANGELOG-LEGACY.md)

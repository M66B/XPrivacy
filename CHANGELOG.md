Changelog
=========

**Release types**

* UNSUPPORTED: only install if you know how to fix things
* EXPERIMENTAL: only install if you know how to fix things
* TEST: new or updated features with a higher risk for bugs
* BETA: new or updated features with a lower risk for bugs
* STABLE: all known bugs are fixed; low risk for bugs

**Downloads**

* [Xposed module repository](http://repo.xposed.info/module/biz.bokhorst.xprivacy) (stable versions)
* [GitHub releases](https://github.com/M66B/XPrivacy/releases) (test and beta versions)

**Important**

* **Please send the support info when XPrivacy asks for it**

**Next release**

* ...

[Open issues](https://github.com/M66B/XPrivacy/issues?state=open)

**Version 3.6.10 TEST**

* Android 5.x compatibility

Big thanks to [dk-zero-cool](https://github.com/dk-zero-cool) !

**Version 3.6.9 STABLE**

**This release does not fix anything for Android 5.x**

**Please read the [release announcement](http://forum.xda-developers.com/showpost.php?p=60176107&postcount=14481)**

* Fixed all problems reported through the debug info
* Fixed restrictions *getToken* and *getTokenWithNotification* ([issue](/../../issues/2169))
* Fixed restriction *AdvertisingId* ([issue](/../../issues/2166))
* Added restriction *GMS5.getCurrentPlace*
* Updated Norwegian translation

**Version 3.6.8 UNSUPPORTED**

* Running in compatibility mode on Lollipop
* Updated to SDK 22 (Android 5.1)

**Version 3.6.7 UNSUPPORTED**

* Fixed need for editing kernel image by using an SELinux loophole
* Fixed bootloop caused by accessing */data/data* in SELinux restrictive mode

**Version 3.6.6 UNSUPPORTED**

* Android 5.x (Lollipop) support
* Reverted "Manage white/black lists from usage data" ([issue](/../../issues/2093))
* Added menu *Manage whitelists* to usage data view for a single application ([issue](/../../issues/2093))
* Updated Czech translation

**Version 3.6.5 BETA**

* Use application whitelist for *getPackagesForUid* and *Srv_getPackagesForUid* ([issue](/../../issues/2116))
* Manage white/black lists from usage data ([issue](/../../issues/2093))
	* Long pressing the uid will toggle the whitelist entry and show the whitelist manager

**Version 3.6.2 STABLE**

* Block *ACTION_NEW_OUTGOING_CALL* and *ACTION_PHONE_STATE_CHANGED* instead of faking phone number ([issue](/../../issues/2132))
* Renamed restriction *View.WebView* into *View.initUserAgentString*
* Added restriction *View.postUrl*
* Changed restriction *View.loadUrl* to restrict loading URLs instead of restriction the user agent string ([issue](/../../issues/2123))
	* Existing *loadUrl* restrictions will be reset and set to ask

**Version 3.6.1 STABLE**

* Fixed location restriction in AOSP mode ([issue](/../../issues/2129))

**Version 3.6 STABLE**

* Stable re-release of version 3.5.11

**Version 3.5.11 BETA**

* Prevent opening wrong application details ([issue](/../../issues/2109))
* Fixed restriction *USB.getSerialNumber*
* Added restriction *Cast.getDeviceId* and *Cast.getIpAddress* ([issue](/../../issues/2108))

**Version 3.5.10 BETA**

* Fixed disabling location updates in compatibility mode ([issue](/../../issues/2105))
* Removed Cydia Substrate library
* Updated Hindi translation
* Updated Slovak translation

**Version 3.5.9 BETA**

* Prevent accidental application icon/name clicks ([issue](/../../issues/2095))
* Scale application icons ([issue](/../../issues/2095))

**Version 3.5.8 BETA**

* Disabled application icon caching ([issue](/../../issues/2094))
* Removed Cydia Substrate support ([issue](/../../issues/2087))

**Version 3.5.7 BETA**

* Fixed allowing applications
* Updated Catalan translation

**Version 3.5.6 TEST**

KitKat or before:

* Added restriction *Srv_getAccountsForPackage*
* Added account type parameter for *Srv_getAccounts* and *Srv_getAccountsAsUser*
* Added restrictions for [LinkProperties](http://developer.android.com/reference/android/net/LinkProperties.html)
* Added quirk *nousage* to disable usage data for specific applications ([issue](/../../issues/2085))
* Updated support library
* Updated Catalan translation
* Updated Slovak translation

Lollipop:

* Added restriction *Srv_startActivityAsCaller* ([issue](/../../issues/1757))
* Check external storage directory for *open* restriction ([issue](/../../issues/1757))
* Added restriction *getInstalledProvidersForProfile* ([issue](/../../issues/1757))
* Allow white listing / show parameter of *getExternalStorageState* ([issue](/../../issues/1757))
* Added restrictions for [UsageStatsManager](https://developer.android.com/reference/android/app/usage/UsageStatsManager.html) ([issue](/../../issues/1757))
* Added restrictions *IpPrefix.getAddress* and *IpPrefix.getRawAddress* ([issue](/../../issues/1757))
* Added restrictions *InetAddress.getAllByNameOnNet* and *InetAddress.getByNameOnNet* ([issue](/../../issues/1757))
* Added restriction *Srv_getCurrentSyncsAsUser* ([issue](/../../issues/1757))
* Added restrictions *Srv_addGpsMeasurementsListener* and *Srv_addGpsNavigationMessageListener* ([issue](/../../issues/1757))
* Added restrictions *getCarrierConfigValues* and *sendMultimediaMessage* ([issue](/../../issues/1757))
* Added restrictions *Srv_getImei*, *Srv_getIsimIst* and *Srv_getIsimPcscf* ([issue](/../../issues/1757))
* Added restrictions *Srv_enableLocationUpdatesForSubscriber*, *Srv_getCdmaMdn*, *Srv_getCdmaMin*, *getLine1AlphaTagForDisplay* and *Srv_getLine1NumberForDisplay* ([issue](/../../issues/1757))

**Version 3.5.5 BETA**

* Silently allow *Srv_getPackageInfo*/*Srv_getApplicationInfo* for own packages again

**Version 3.5.4 BETA**

* Broadcast *biz.bokhorst.xprivacy.action.EXCEPTION* if the database could not be read ([issue](/../../issues/2081))
* Fixed all problems reported through the debug info
* Allow querying information about own package in most cases ([issue](/../../issues/2079))
* Updated French translation
* Updated Indonesian translation
* Updated Polish translation

**Version 3.5.3 BETA**

* Restart notification for *requestLocationUpdates*, *requestSingleUpdate* and *Srv_requestLocationUpdates*
* Notify and do not send the intent *biz.bokhorst.xprivacy.action.ACTIVE* when the privacy database was corrupt (discussed on XDA)

**Version 3.5.2 BETA**

* Fixed global *freeze* quirk

**Version 3.5.1 BETA**

* Show parameter to *getPackagesForUid*/*Srv_getPackagesForUid*
* Fixed renaming long template names ([issue](/../../issues/2052))
* Fixed using *Srv_requestLocationUpdates* in compatibility mode ([issue](/../../issues/2050))
* Allow quirk *freeze* per application

[Open issues](https://github.com/M66B/XPrivacy/issues?state=open)

**Version 3.5 STABLE**

* Material design
* New icon, thanks @[Primokorn](http://forum.xda-developers.com/member.php?u=4958579)

**Version 3.4.14 BETA**

* Fixed all problems reported through the debug info
* Added support for Android TV
* Using accent color for in view progress bar
* New icon, thanks @[daniel_m](http://forum.xda-developers.com/member.php?u=4885896)
* Added Korean translation
* Updated Arabic translation
* Updated Norwegian translation

**Version 3.4.13 BETA**

* Fixed search by reverting to holo search ([issue](/../../issues/2043))

**Version 3.4.12 BETA**

* Usage data for functions which cannot be restricted
* Fixed double tap search
* Allow more identification functions to be restricted for XPrivacy itself
* Updated French translation

**Version 3.4.11 BETA**

* Use material design toolbar
* Updated Lithuanian translation

**Version 3.4.10 BETA**

* Progress dialog bar in accent color

**Version 3.4.9 BETA**

* Changed accent color to orange

**Version 3.4.8 TEST**

* Use material design switch in application details view

**Version 3.4.7 TEST**

* Using teal as material design colors

**Version 3.4.6 TEST**

* Use accent color for custom check boxes and list item press

**Version 3.4.5 TEST**

* Fixed search view ([issue](/../../issues/2037))

**Version 3.4.4 TEST**

* Custom material design colors

**Version 3.4.3 TEST**

* Fixed settings save/cancel action

**Version 3.4.2 TEST**

* Fixed all problems reported through the debug info
* Updated Android support library project
* Using Android SDK CardView library
* Material design
* Use subtitle for operation name
* Updated Norwegian translation

**Version 3.4.1 BETA**

* Fixed all problems reported through the debug info
* Fixed warning *Native call method*
* Fixed documentation icons not appearing when opening from notification ([issue](/../../issues/2025))
* Added title to original value in usage data
* Added support for [Omega ROM](http://omegadroid.co/omega-roms/)
* Updated Indonesian translation
* Updated Russian translation
* Updated Slovak translation
* Updated Vietnamese translation

**Version 3.4 STABLE**

* Fixed all problems reported through the debug info
* Updated French translation
* Updated German translation
* Updated Japanese translation

**Version 3.3.4 BETA**

* Strip IP address from IP address / domain name pair for better wildcards ([issue](/../../issues/2014))
* Added Welsh translation

**Version 3.3.3 BETA**

* Fixed displaying applications with the same name once in select to allow list ([issue](/../../issues/2013))

**Version 3.3.2 BETA**

* Improved IP address / host name parsing

**Version 3.3.1 BETA**

* Fixed all problems reported through the debug info
* Strip leading slash from IP address for improved wildcards
* Better [Cydia Substrate](http://www.xda-developers.com/android/cydia-substrate-released-by-saurik-for-android/) support
* Updated Malay translation
* Updated Polish translation
* Updated Russian translation

**Version 3.3 STABLE**

* Display legend on first run
* Updated Japanese translation
* Updated Lithuanian translation

**Version 3.2.5 BETA**

* Fixed all problems reported through the debug info
* Updated Indonesian translation
* Updated Slovak translation

[Open issues](https://github.com/M66B/XPrivacy/issues?state=open)

**Version 3.2.4 TEST**

* Persist/show original values for: ([pro license](http://www.xprivacy.eu/) only) ([issue](/../../issues/1297))
	* advertisement ID
	* phone number
	* Android ID
	* Persisting/showing fake values would require an extra call to the privacy service, which would impact performance
* Added settings menu to usage data view
* Using [CardView library](https://github.com/yongjhih/CardView)
	* There are no rounded corners when using the dark theme
* Added [Material Design](https://developer.android.com/preview/material/index.html) styles (this will **not** work on Android KitKat and before)

**Version 3.2.3 TEST**

* Persist/show original values ([pro license](http://www.xprivacy.eu/) only) ([issue](/../../issues/1297))
	* Persisting/showing fake values would require an extra call to the privacy service, which would impact performance
	* Proof of concept with *SERIAL*
* Flush local application caches too
* Updated Dutch translation
* Updated French translation
* Updated German translation
* Updated Indonesian translation
* Updated Japanese translation
* Updated Slovak translation

**Version 3.2.2 BETA**

* Added option to rename templates ([issue](/../../issues/1723))
* Restored select all ([issue](/../../issues/1977)) ([issue](/../../issues/1986))
* Updated Dutch translation

**Version 3.2.1 BETA**

* Fixed details view tutorial header
* Made disabled main list entries clickable
* Show half check box only to expert users
* Always show default for on demand restricting time out
* Updated Dutch translation
* Updated German translation
* Updated traditional Chinese translation

**Version 3.2 STABLE**

* Updated Slovak translation
* Updated Spanish translation

**Version 3.1.5 BETA**

* Select all enabled applications ([issue](/../../issues/1977))

**Version 3.1.4 BETA**

* Added help text to application specific settings ([issue](/../../issues/1968))
* Updated Dutch translation
* Updated French translation
* Updated German translation
* Updated Indonesian translation

**Version 3.1.3 BETA**

* Mark application as changed / update last modification time when white/black listing
* Updated Slovak translation

**Version 3.1.2 BETA**

* Fixed all problems reported through the debug info
* Added usage data white/black list help text
* Mark application as changed / update last modification time when white/black listing
* Updated Dutch translation
* Updated French translation
* Updated German translation
* Updated Indonesian translation
* Updated Japanese translation

**Version 3.1.1 STABLE**

* Fixed all problems reported through the debug info
* Showing message when enabling expert mode / using an expert function
* Scroll view for toggle restrictions radio buttons
* Updated Dutch translation
* Updated Indonesian translation

**Version 3.1 STABLE**

* Showing appropriate main and details help ([issue](/../../issues/1921))
* Showing application name above usage data ([issue](/../../issues/1949))
* Showing category help ("i"-icon application list) in web view ([issue](/../../issues/1943))
* Showing category help ("i"-icon application details) in dialog
* Removed menu *Check for updates*

**Version 3.0.3 BETA**

* Improved settings layout ([pull request](/../../issues/1946))
* Improved function help layout ([pull request](/../../issues/1947))

**Version 3.0.2 BETA**

* Layout improvements
* Fixed tutorial header
* Fixed Indonesian translation
* Updated Lithuanian translation

**Version 3.0.1 BETA**

* Improved database locking ([pull request](/../../issues/1939))
* Changed settings dialog to settings activity ([pull request](/../../issues/1938))
* Moved flush button to expert mode section ([issue](/../../issues/1934))
* Added category merge/reset ([issue](/../../issues/1909))
* Display introductory tutorial only on first run after *About* ([issue](/../../issues/1942))
* Moved search to action bar ([issue](/../../issues/1918))
* Added Indonesian translation
* Updated simplified Chinese translation

**Version 3.0 STABLE**

Main changes since stable version 2.2.1:

* Reviewed all about 250 restrictions, resulting in numerous changes, mainly visible as performance improvements
* Added about 150 new restrictions to fix the unhook vulnerability, see [FAQ #68](https://github.com/M66B/XPrivacy#FAQ68) for details
* Reorganized menus, action bar items and other user interface elements to improve usability
* Fixed all reported bugs and implemented lots of requested features
* See for all details the changelogs of version 2.99.x

Other changes:

* Updated Italian translation
* Updated Japanese translation
* Updated Norwegian translation

**Version 2.99.43 BETA**

* Layout improvement ([pull request](/../../issues/1933))
* Show usage statistics as subtitle

**Version 2.99.42 BETA**

* Layout and text improvements
* Updated Arabic translation
* Updated French translation
* Updated German translation
* Updated Lithuanian translation
* Updated Slovak translation

**Version 2.99.41 BETA**

* Replaced *Play* action by *Operations* menu
* Moved *Help* action bar item to *Legend* menu
* Renamed filter button *Clear all* to *Default* ([issue](/../../issues/1920))
* Updated Dutch translation
* Updated Japanese translation

**Version 2.99.40 BETA**

* Fixed long application names not showing

**Version 2.99.39 BETA**

* Moved clear all data to settings dialog

**Version 2.99.38 BETA**

* Clear filters will reset the filters to their default state
* Changed on demand restricting progress bar to holo style
* Clicking the application name will open the application details view
* Changed multiple selection background color
* Reorganized menus and action bar items
* Updated French translation
* Updated German translation
* Updated Lithuanian translation
* Updated Russian translation
* Updated Slovak translation

**Version 2.99.37 BETA**

* Clear will also clear usage data and application specific settings
* Changed application specific fake values icon to a star ([issue](/../../issues/1831))
* Showing changelog if new version installed
* Updated Arabic translation
* Updated Italian translation
* Updated Japanese translation

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
For XPrivacy version 2.x, see the [older changelogs](CHANGELOG-LEGACY.md)

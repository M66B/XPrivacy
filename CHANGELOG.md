Changelog
=========

**Release types**

* TEST: new or updated features with a higher risk for bugs
* BETA: new or updated features with a lower risk for bugs
* STABLE: all known bugs fixed; low risk for bugs

Stable releases have odd minor version numbers from version 1.11.x.<br /> 
Test and beta releases have even minor version numbers from version 1.12.x.<br />
Test and beta releases will have experimental functions enabled by default.

**Next test/beta release**

* Fixed restricting internet and storage for multi-user environments ([issue](https://github.com/M66B/XPrivacy/issues/357))
* Experimental functions enabled by default
* Updated German translation

**Next stable release**

* Fixed restricting internet and storage for multi-user environments ([issue](https://github.com/M66B/XPrivacy/issues/357))
* Updated German translation

[Open issues](https://github.com/M66B/XPrivacy/issues?state=open)

**Version 1.11 STABLE**

* Fixed application notification setting not sticking ([issue](https://github.com/M66B/XPrivacy/issues/1006))
* Sort applications respecting locale, thanks @[jpeg729](https://github.com/jpeg729)
* Display if Pro version (only [Pro license](http://www.xprivacy.eu/)) ([issue](https://github.com/M66B/XPrivacy/issues/996))
* Updated Hindi translation
* Updated Slovak translation
* Updated Slovenian translation
* Updated Ukrainian translation

**Version 1.10.51 BETA**

* Fixed boot loop on some devices/ROMs ([issue](https://github.com/M66B/XPrivacy/issues/995))

**Version 1.10.50 BETA**

* Fixed missing version names
* Fixed wrong package for application, thanks @[jpeg729](https://github.com/jpeg729)
* Fixed getting settings for isolated processes
* Submit if accounts, application, contacts are allowed ([issue](https://github.com/M66B/XPrivacy/issues/986))
* Proguard to reduce the application size (saves about 200 KB of 650 KB)
* Experimental: kill applications
* Experimental: auto flush restriction/settings cache
* Updated Arabic translation
* Updated Chinese translation
* Updated French translation
* Updated Russian translation
* Updated Slovak translation

**Version 1.10.49 BETA**

* Make application title scrollable

**Version 1.10.48 BETA**

* Fixed application package list scroll

**Version 1.10.47 BETA**

* Fixed application package list filling screen

**Version 1.10.46 BETA**

* Fixed GitHub links by forcing desktop mode
* Updated Chinese translation

**Version 1.10.45 BETA**

* Fixed uncaught exception handler ([issue](https://github.com/M66B/XPrivacy/issues/951))
* Fixed confidence settings (application details view)
* Fixed always restart required for dangerous functions
* Moved confidence setting to expert mode
* Replaced wiki links by links to GitHub
* Added menu to clear usage data
* Added refresh menu to application list
* Handle applications with shared components, also thanks @[jpeg729](https://github.com/jpeg729)
* Updated Chinese translations
* Updated Norwegian translation
* Updated Polish translation
* Updated Lithuanian translation

**Version 1.10.44 BETA**

* Better progress report while importing, thanks @[jpeg729](https://github.com/jpeg729)
* Setting to set maximum confidence fetch interval
* Changed connection time out from 45 to 20 seconds
* Updated Chinese translations
* Updated Hungarian translation
* Updated Slovak translation

**Version 1.10.43 BETA**

* Fixed exporting legacy settings
* Fixed exporting default template
* Updated Dutch translation
* Updated Russian translation
* Updated Ukrainian translation

**Version 1.10.42 BETA**

* Corrected submit ready message
* Better error messages for import, export and fetch
* Workaround for Google file chooser new URI format (KitKat)
* Made tutorial translatable
* Updated Chinese translations
* Updated Lithuanian translation
* Updated Polish translation
* Updated Russian translation
* Updated Ukrainian translation
* Updated Vietnamese translation

**Version 1.10.41 BETA**

* Delete old restrictions on new application install, thanks @[jpeg729](https://github.com/jpeg729)
* Delete application specific settings on application uninstall ([issue](https://github.com/M66B/XPrivacy/issues/901))
* Replaced notification by 'toast' for fetch restrictions
* Added application state colors to legend
* Ask if sure when fetching
* Display *Template applied* for new applications in the notification
* Changed application state colors, thanks @[danielmmmm](https://github.com/danielmmmm)
* Updated Android Support Library package
* Added tutorial ([issue](https://github.com/M66B/XPrivacy/issues/890))
* Reverted "Updated Italian translation" (update for another language)
* Added Ukrainian translation
* Updated Chinese translation
* Updated German translation
* Updated Lithuanian translation
* Updated Norwegian translation
* Updated Russian translation
* Updated Vietnamese translation

**Version 1.10.40 TEST**

* Fixed exporting of legacy settings

**Version 1.10.39 TEST**

* Fixed fetching unknown functions
* Fixed restrictions of isolated processes on KitKat
* Fixed importing wrong application specific settings
* Color for state ([issue](https://github.com/M66B/XPrivacy/issues/887))
* Added context menu to icon in application details view, thanks @[jpeg729](https://github.com/jpeg729)
* Disabled fast scroll of application list, since it is buggy on KitKat
* Added new restriction category *Overlay*
* Less resource intensive export/import (export file format has changed)
* Small performance improvement
* Updated Arabic translation
* Updated Chinese translation
* Updated French translation
* Updated Italian translation
* Updated Polish translation
* Updated Vietnamese translation

**Version 1.10.38 BETA**

* Fixed toggling all restrictions
* Fixed template only being partially applied, thanks @[jpeg729](https://github.com/jpeg729)
* Fixed index out of range in web view hook
* Updated Italian translation
* Updated Lithuanian translation

**Version 1.10.37 BETA**

* Fixed setting dangerous restrictions ([issue](https://github.com/M66B/XPrivacy/issues/876))
* Fixed web view usage data
* Attempt to fix user agent restriction for KitKat ([issue](https://github.com/M66B/XPrivacy/issues/825))
* Show relative usage time
* Filter application details on data usage
* Notify if application needs restart, thanks @[jpeg729](https://github.com/jpeg729)
* Chain new uncaught exception handlers
* Send usage data on service destroy
* Updated German translation
* Updated Polish translation

**Version 1.10.36 TEST**

* Better titles for dialogs
* Fixed error message fetch restrictions
* Fixed canceling fetch all restrictions
* Better title for toggle all restrictions, thanks @[jpeg729](https://github.com/jpeg729)
* Run toggle all restriction in a background task, thanks @[jpeg729](https://github.com/jpeg729)
* Updated Lithuanian translation

**Version 1.10.35 EXPERIMENTAL**

* Attempt to fix user agent restriction for KitKat ([issue](https://github.com/M66B/XPrivacy/issues/825))
* Attempt to fix clipboard restriction for some devices ([issue](https://github.com/M66B/XPrivacy/issues/857))
* Dynamic hooking of service managers for better compatibility
* Progress reports while filtering, thanks @[jpeg729](https://github.com/jpeg729)
* Updated Irish translation
* Updated Russian translation
* Updated Slovak translation
* Updated Spanish translation

**Version 1.10.34 TEST**

* Fixed missing usage data
* Send usage data on application close or force close (FC)
* Progress reports while export is loading, thanks @[jpeg729](https://github.com/jpeg729)
* Require Xposed version 2.4

**Version 1.10.33 BETA**

* Fixed missing usage data, thanks @[jpeg729](https://github.com/jpeg729)
* Updated Chinese translations
* Updated Japanese translation

**Version 1.10.31 BETA**

* Fixed ongoing notification of fetch restrictions
* Attempt to fix non-aligned check boxes
* New check mark images, thanks @[Looki75](http://forum.xda-developers.com/member.php?u=2468642)
* Updated German translation

**Version 1.10.30 BETA**

* Caching of application information for better response times
* Restriction of draw over / on top ([issue](https://github.com/M66B/XPrivacy/issues/830))
* Modified tri-state check box implementation ([issue](https://github.com/M66B/XPrivacy/issues/833))
* Fixed cancel import file select ([issue](https://github.com/M66B/XPrivacy/issues/837))
* Updated Arabic translation
* Updated Dutch translation
* Updated Polish translation
* Updated Russian translation

**Version 1.10.29 TEST**

* Fixed Android KitKat message permissions
* Do not disable Android/extra usage data when not in expert mode
* Separate setting to enable restricting dangerous functions, thanks @[jpeg729](https://github.com/jpeg729)
* Show exported file name while sharing, thanks @[jpeg729](https://github.com/jpeg729)
* Progress indication for export, import and fetch, thanks @[jpeg729](https://github.com/jpeg729)
* Selected tab (categories, filter) in theme color, thanks @[jpeg729](https://github.com/jpeg729)
* Added tri-state check box to legend ([issue](https://github.com/M66B/XPrivacy/issues/827))
* Another method to restrict the browser user agent ([issue](https://github.com/M66B/XPrivacy/issues/825))
* Updated Lithuanian translation
* Updated Norwegian translation
* Updated Slovak translation

**Version 1.10.27 TEST**

* Faster startup
* Modified tri-state check box implementation
* Improved application icon image quality
* Allow Android / extra usage data setting without expert mode

**Version 1.10.26 TEST**

* Increase category selector drop down size
* Display click effect to prevent unintended restriction changes
* Added half state check box to application details view
* Updated Arabic translation
* Updated Polish translation
* Updated Vietnamese translations

**Version 1.10.25 TEST**

* Display third state for categories not fully restricted ([issue](https://github.com/M66B/XPrivacy/issues/815))
* Automatically open and scroll to category in application view
* Updated Chinese translations

**Version 1.10.23 BETA**

* Allow fetching restrictions for system applications per application
* Do not restrict the [Pro enabler](https://play.google.com/store/apps/details?id=biz.bokhorst.xprivacy.pro) on new install ([issue](https://github.com/M66B/XPrivacy/issues/812))
* Samsung multi window support ([issue](https://github.com/M66B/XPrivacy/issues/816))
* Updated Chinese translation
* Updated Finnish translation
* Updated Lithuanian translation
* Updated Polish translation

**Version 1.10.21 BETA**

* Marked all functions in the system category as dangerous, except one
* Don't return an empty gids list to prevent exception
* Require [Pro enabler](http://play.google.com/store/apps/details?id=biz.bokhorst.xprivacy.pro) version 1.12
* Leave randomize button enabled when randomize on boot is enabled, thanks @[jpeg729](https://github.com/jpeg729)
* Changed title of usage data view
* Fixed activity history ([issue](https://github.com/M66B/XPrivacy/issues/804))

**Version 1.10.20 TEST**

* Fixed opening usage data from application details ([issue](https://github.com/M66B/XPrivacy/issues/793))
* Fixed refreshing application list during fetch restrictions
* Workaround for PAC boot problem ([issue](https://github.com/M66B/XPrivacy/issues/785))
* Moved selecting accounts to allow to free version
* Moved settings Android usage data / extra usage data to expert mode
* Removed dangerous categories, only functions can be dangerous
* Made *inet*, *media*, *sdcard* dangerous
* Always highlight dangerous functions ([issue](https://github.com/M66B/XPrivacy/issues/796))
* Made Xposed Installer a system application
* Added application name to title of application details view
* Removed version warning for KitKat
* Redesigned filters/categories, thanks @[jpeg729](https://github.com/jpeg729)
* Redesigned settings dialog, thanks @[jpeg729](https://github.com/jpeg729)
* Small performance improvements
* Added Finnish translation
* Updated German translation
* Updated Chinese translation
* Updated Lithuanian translation

**Version 1.10.18 TEST**

* Fixed opening wrong settings from notification (again) ([issue](https://github.com/M66B/XPrivacy/issues/777))
* Draw border around application icon in application details view
* Option to enable extra usage data ([issue](https://github.com/M66B/XPrivacy/issues/783)) ([issue](https://github.com/M66B/XPrivacy/issues/785)) ([issue](https://github.com/M66B/XPrivacy/issues/786))
* Updated Arabic translation
* Updated Chinese translation
* Updated Slovak translation

**Version 1.10.16 TEST**

* Compatibility with Android 4.4 KitKat ([issue](https://github.com/M66B/XPrivacy/issues/733))
* Draw border around application icons in main list
* Updated Arabic translation
* Updated Chinese translation
* Updated German translation

**Version 1.10.15 TEST**

* Fixed empty/crashing template
* Restrict input device descriptor ([issue](https://github.com/M66B/XPrivacy/issues/768))

**Version 1.10.14 TEST**

* Filter on permissions filters functions too ([issue](https://github.com/M66B/XPrivacy/issues/695))
* Select applications to allow ([issue](https://github.com/M66B/XPrivacy/issues/686))
* Require [Pro license](http://www.xprivacy.eu/) to allow individual accounts, applications and contacts
* Build for Android 4.4 KitKat ([issue](https://github.com/M66B/XPrivacy/issues/733))
* Added expert mode (disables dangerous restrictions) ([issue](https://github.com/M66B/XPrivacy/issues/716)) ([issue](https://github.com/M66B/XPrivacy/issues/724))
* Updated Chinese translation
* Updated French translation
* Updated Lithuanian translation
* Updated Polish language

**Version 1.10.13 BETA**

* Fixed crash on empty latitude/longitude ([issue](https://github.com/M66B/XPrivacy/issues/755))
* Option to negate filter on restrictions ([issue](https://github.com/M66B/XPrivacy/issues/754))
* Option to make XPrivacy device administrator (to prevent unwanted uninstall)
* Updated Chinese translation
* Updated Lithuanian translation

**Version 1.10.12 BETA**

* Fixed *java.lang.NoClassDefFoundError: de.robv.android.xposed.XposedHelpers*

**Version 1.10.11 BETA**

* Fixed rare null pointer exception ([issue](https://github.com/M66B/XPrivacy/issues/739))
* Fixed flickering with black theme
* Fixed setting global and randomize on boot settings
* Fixed width settings dialog
* Require Android 4.0.3 (minimum for Xposed)
* Added settings for Advertisement ID (including randomization) ([issue](https://github.com/M66B/XPrivacy/issues/738))
* Added settings for SSID (including randomization) ([issue](https://github.com/M66B/XPrivacy/issues/652))
* Added setting to disable update notification (per application) ([issue](https://github.com/M66B/XPrivacy/issues/635))
* Not filtering on restricted will filter on not restricted ([issue](https://github.com/M66B/XPrivacy/issues/682))
* Option to randomize subscriber ID (IMSI) ([issue](https://github.com/M66B/XPrivacy/issues/690))
* Added application icons to usage view ([issue](https://github.com/M66B/XPrivacy/issues/693))
* Added traditional Chinese translation
* Updated German translation
* Updated Vietnamese translation

**Version 1.10.8 TEST**

* Fixed import of filter settings
* Fixed up navigation (again) ([issue](https://github.com/M66B/XPrivacy/issues/681))

**Version 1.10.7 TEST**

* Mark categories dangerous in template
* Permanently cache XPrivacy version and Android usage settings
* Do not display number of packages in progress dialog
* Show location for license file in about dialog
* Fixed up navigation (again) ([issue](https://github.com/M66B/XPrivacy/issues/681))
* Auto import pro license file ([issue](https://github.com/M66B/XPrivacy/issues/703))
* Allow application specific disable of globally set randomization ([issue](https://github.com/M66B/XPrivacy/issues/706))
* Compatibility with *MultiSimTelephonyManager* ([issue](https://github.com/M66B/XPrivacy/issues/732))
* Restrict */sys/block/.../cid* and */sys/class/.../cid* ([issue](https://github.com/M66B/XPrivacy/issues/734))
* Restrict system properties ending with *cid* ([issue](https://github.com/M66B/XPrivacy/issues/734))
* Updated Slovak translation

**Version 1.10.5 TEST**

* Restrict access to sensors ([issue](https://github.com/M66B/XPrivacy/issues/723))
* Restrict Google advertising ID ([issue](https://github.com/M66B/XPrivacy/issues/731))
* Send more usage data and faster

**Version 1.10.4 BETA**

* Added setting to enable Android usage data (default disabled) ([issue](https://github.com/M66B/XPrivacy/issues/694))
* Randomize settings at boot in separate thread ([issue](https://github.com/M66B/XPrivacy/issues/700))
* Added dangerous restrictions to template ([issue](https://github.com/M66B/XPrivacy/issues/716), [issue](https://github.com/M66B/XPrivacy/issues/724))
* Fixed opening correct application settings from notification ([issue](https://github.com/M66B/XPrivacy/issues/717))
* Do not use current application context ([issue](https://github.com/M66B/XPrivacy/issues/722))
* Handle all exceptions while processing package events ([issue](https://github.com/M66B/XPrivacy/issues/722))
* Require Xposed 2.3.1
* Updated Arabic translation
* Updated German translation
* Updated Italian translation

**Version 1.10 stable**

* Updated Catalan translation

**Version 1.9.27 release candidate**

* Restore previous up navigation, but disable up navigation if started from notification

**Version 1.9.26 release candidate**

* Fixed up navigation (again) ([issue](https://github.com/M66B/XPrivacy/issues/681))
* Updated German translation
* Updated Norwegian translation
* Updated Simplified Chinese translation
* Updated Swedish translation

**Version 1.9.25 release candidate**

* Fixed always asking for clear when tapping notification
* Remove notification when selecting clear

**Version 1.9.24 release candidate**

* Option to clear restrictions from notification (if your Android version supports this)

**Version 1.9.23 release candidate**

* Fixed up navigation ([issue](https://github.com/M66B/XPrivacy/issues/681))
* Ask if sure when clearing restrictions / applying template

**Version 1.9.22 release candidate**

* Group some application menu items (thanks @[tonymanou](https://github.com/tonymanou))
* Prevent user/system application filter both being applied
* Auto fix folder permissions
* Added icon for user application filter, thanks @[Looki75](http://forum.xda-developers.com/member.php?u=2468642)
* Display file name after export ([issue](https://github.com/M66B/XPrivacy/issues/680))
* Updated Hindi translation
* Updated Japanese translation
* Updated Lithuanian translation
* Updated Slovenian translation

**Version 1.9.21 release candidate**

* Fixed hang on boot / start application
* Fixed filter on restrictions

**Version 1.9.20 release candidate**

* Fixed templates no longer applied ([issue](https://github.com/M66B/XPrivacy/issues/669))
* Fixed wrong restrictions sometimes applied (possibly fixing [issue](https://github.com/M66B/XPrivacy/issues/627))
* Updated French translation
* Updated Simplified Chinese translation

**Version 1.9.19 release candidate**

* Revert *Use privacy provider 60 seconds after system ready for all packages*

**Version 1.9.18 release candidate**

* Allow uncheck half checked restrictions
* Display application name in settings
* Use privacy provider 60 seconds after system ready for all packages
* Fixed system application getting restricted on update
* Sort application names for same uid
* Updated Arabic translation
* Updated German translation
* Updated Spanish translation
* Updated Vietnamese translation

**Version 1.9.17 BETA!**

* Fixed true randomization setting ([issue](https://github.com/M66B/XPrivacy/issues/658))
* Add filter on user applications
* Updated German translation

**Version 1.9.16 BETA!**

* Retry read settings/restrictions
* Updated Arabic translation
* Updated Hindi translation
* Updated Lithuanian translation
* Updated Slovenian translation

**Version 1.9.15 BETA!**

* Make filter settings persistent
* Simplified filtering on application type
* Fixed default for randomize on boot
* Fixed settings text width
* Updated Simplified Chinese translation

**Version 1.9.14 BETA!**

* Application specific settings
* True randomization (both global/per app)
* Added clear button to settings
* Auto fix file permissions by privacy provider (not in Zygote anymore)
* Removed popup for pro license (not enabler)
* Updated Danish translation
* Updated French translation
* Updated German translation
* Updated Japanese translation
* Updated Lithuanian translation
* Updated Simplified Chinese translation
* Updated Slovak translation
* Updated Vietnamese translation

**Version 1.9.13 BETA!**

* Auto fix file permissions ([issue](https://github.com/M66B/XPrivacy/issues/627))
* Display new/update in notification ([issue](https://github.com/M66B/XPrivacy/issues/631))
* Attempt to fix opening wrong settings from notification
* Support for randomize at boot per application (no GUI yet)
* Fetch restrictions for all user applications ([issue](https://github.com/M66B/XPrivacy/issues/514))
* Added Estonian translation
* Updated Arabic translation
* Updated German translation
* Updated Slovenian translation

**Version 1.9.12 BETA!**

* Fixed title randomize button
* Broadcast intent *biz.bokhorst.xprivacy.action.ACTIVE* after boot ([issue](https://github.com/M66B/XPrivacy/issues/606))
* Compiled for Xposed version 2.2
* Removed update menu (Xposed 2.2 will take care of updates)
* This is the [last release on goo.im](http://forum.xda-developers.com/showpost.php?p=45276849&postcount=3233)
* Updated Arabic translation
* Updated Czech translation
* Updated Slovak translation
* Updated Vietnamese translation

**Version 1.9.11 BETA!**

* Fixed FC at boot by reverting intent at boot

**Version 1.9.10 BETA!**

* Split restriction cache timeout (15 seconds) and settings cache timeout (30 seconds)
* Prepare application specific settings / true randomization ([issue](https://github.com/M66B/XPrivacy/issues/540))
* Fixed not handled restriction ([issue](https://github.com/M66B/XPrivacy/issues/610))
* Fixed check for file manager ([issue](https://github.com/M66B/XPrivacy/issues/327))
* Send intent *biz.bokhorst.xprivacy.action.ACTIVE* after boot ([issue](https://github.com/M66B/XPrivacy/issues/606))
* Setting for browser user agent ([issue](https://github.com/M66B/XPrivacy/issues/608))
* Updated German translation
* Updated Lithuanian translation
* Updated Russian translation
* Updated Turkish translation

**Version 1.9.9 BETA!**

* Fixed half state for applications with only dangerous restricted ([pull request](https://github.com/M66B/XPrivacy/pull/593))
* Fixed empty app list on first launch ([pull request](https://github.com/M66B/XPrivacy/pull/595))
* Fixed "has permission" filter not updating application list ([pull request](https://github.com/M66B/XPrivacy/pull/601))
* Added Farsi translation
* Updated Japanese translation
* Updated Polish translation
* Updated Slovak translation
* Updated Vietnamese translation

**Version 1.9.8 BETA!**

* Updated Arabic translation
* Updated German translation
* Updated Hindi translation
* Updated Lithuanian translation
* Updated Simplified Chinese translation
* Updated Slovak translation

Contributed by [tonymanou](https://github.com/tonymanou): ([pull request](https://github.com/M66B/XPrivacy/pull/586))

* Added three-states checkboxes in main activity
* Improved accessibility: clickable images are focusable with DPAD in filter frame
* Added the ability to show apps of user/system/both (new strings to translate)
* Added infos about the user/system/both icons in help section
* Various fixes (bad margins on large screens, 9-patches ...)

**Version 1.9.7 BETA!**

* Setting for IP address ([issue](https://github.com/M66B/XPrivacy/issues/554))
* Restrict browser user agent string ([issue](https://github.com/M66B/XPrivacy/issues/537))
* Restrict /system/build.prop ([issue](https://github.com/M66B/XPrivacy/issues/575))
* Fixed margins on large screens
* Fixed restricting NFC ([issue](https://github.com/M66B/XPrivacy/issues/530))
* Fixed exporting dangerous function exceptions ([issue](https://github.com/M66B/XPrivacy/issues/283))
* Setting to enabled/disable logging (default disabled)
* Add seconds to export file name ([issue](https://github.com/M66B/XPrivacy/issues/327))
* Added Hindi translation
* Updated Arabic translation
* Updated French translation
* Updated German translation
* Updated Japanese translation
* Updated Portuguese translation
* Updated Simplified Chinese translation
* Updated Slovenian translation
* Updated Vietnamese translation

**Version 1.9.6 BETA!**

* Updated Arabic translation
* Updated Catalan translation
* Updated Czech translation
* Updated Dutch/Flemish translation
* Updated French translation
* Updated German translation
* Updated Italian translation
* Updated Japanese translation
* Updated Lithuanian translation
* Updated Norwegian translation
* Updated Polish translation
* Updated Portuguese translation
* Updated Simplified Chinese translation
* Updated Slovenian translation
* Updated Swedish translation
* Updated Turkish translation
* Updated Vietnamese translation

Contributed by [tonymanou](https://github.com/tonymanou): ([pull request](https://github.com/M66B/XPrivacy/pull/566))

* Added a circular progress bar when the filter is processing
* Added stats: apps filtered / total apps
* Moved *category* filter outside of the collapsible filter frame
* Changed clickable images for filters to checkboxes
* Added new strings for the filters
* New card-like UI theme

Note for the translators: be careful! A too long string will be cut on small screens ...

**Version 1.9.5 BETA!**

* Updated Greek translation
* Updated Japanese translation

Contributed by [tonymanou](https://github.com/tonymanou): ([pull request](https://github.com/M66B/XPrivacy/pull/534))

* Fixed filter frame's hidden state that was not restored when restoring the instance state
* Moved the filters 'hide system app' and 'filter by permission' from the settings to the filter frame
* Bigger expand icon for filter section (asked on XDA)
* Fixed alignment of the items in the filter section
* Improved the design of the filter frame (asked on XDA) : the corners are less round, the background is now a gradient

**Version 1.9.4 BETA!**

* Restrict [NfcAdapter](http://developer.android.com/reference/android/nfc/NfcAdapter.html) ([issue](https://github.com/M66B/XPrivacy/issues/530))
* Changed [ALL](http://en.wikipedia.org/wiki/255.255.255.255) into [ANY](http://en.wikipedia.org/wiki/0.0.0.0)
* Updated Slovenian translation

Contributed by [tonymanou](https://github.com/tonymanou): ([pull request](https://github.com/M66B/XPrivacy/pull/532))

* Moved help button to the action bar
* Moved the edit help (pen icon) into the help pop-up (only for the main activity)
* Added scrollbars to the main help pop-up in order to avoid cut content on small screens
* Added a frame around the filter section
* Ability to show/hide the filter section (hidden when the application starts)

**Version 1.9.3 BETA!**

* Submit MD5 of android ID for more privacy
* Layout/menu/text improvements, thanks [tonymanou](https://github.com/tonymanou)
* Do not clear existing restrictions when no restrictions fetched ([issue](https://github.com/M66B/XPrivacy/issues/523))
* Updated English translation
* Updated Simplified Chinese translation

**Version 1.9.2 BETA!**

* Fixed fetching function exceptions ([issue](https://github.com/M66B/XPrivacy/issues/515))
* Updated Czech translation
* Updated French translation
* Updated German translation
* Updated Polish translation
* Updated Portuguese translation
* Updated Slovak translation
* Updated Turkish translation

**Version 1.9.1 BETA!**

* Fetch crowd sourced restrictions (experimental)
* Suppress warning in location manager
* Updated Turkish translation

**Version 1.9 stable**

* Delete usage data when removing application
* Updated Catalan translation
* Updated Lithuanian translation
* Updated Japanese translation
* Updated Russian translation

**Version 1.8.13 RC!**

* Delete restrictions when removing application
* Fixed exception logging of TelephonyManager
* Wait 500 ms between sending usage data
* Updated Arabic translation
* Updated French translation
* Updated German translation
* Updated Lithuanian translation
* Updated Polish translation
* Updated Portuguese translation
* Updated Simplified Chinese translation
* Updated Slovak translation
* Updated Swedish translation

**Version 1.8.12 RC!**

* Menu to clear all application restrictions
* Wait 60 seconds after system ready before sending Android usage data ([issue](https://github.com/M66B/XPrivacy/issues/483))
* Updated German translation
* Updated Lithuanian translation

**Version 1.8.11 BETA!**

* Link application info to submitted restrictions
* Ask if sure when submitting restriction data
* Notify on start of submit

**Version 1.8.10 BETA!**

* Submit application name

**Version 1.8.9 BETA!**

* Submit data to central server (only in application details view) **Experimental!**
* Updated Arabic translation
* Updated German translation
* Updated Japanese Translation

**Version 1.8.8 BETA!**

* Fixed a bug in location restriction ([issue](https://github.com/M66B/XPrivacy/issues/477))
* Updated French translation
* Updated German translation
* Updated Lithuanian translation
* Updated Simplified Chinese translation

**Version 1.8.7 BETA!**

* Enable Android usage data 60 seconds after boot
* Filter usage data for the last 24 hours
* Option to set network/SIM operator name

**Version 1.8.6 BETA!**

* Always lock fallback restriction loading
* Limit help icon size

**Version 1.8.5 BETA!**

* Disabled usage data for Android (results in slow/hanging Android start)
* Updated French translation
* Updated Polish translation

**Version 1.8.4 ALPHA!**

* Restrict serial number for Android again
* Usage data for Android
* Updated Japanese translation
* Updated Lithuanian translation
* Updated Slovak translation

**Version 1.8.3**

* Restrict *getNetworkType* and *getPhoneType* ([issue](https://github.com/M66B/XPrivacy/issues/441))
* Updated Norwegian translation
* Updated Russian translation

**Version 1.8.2 BETA!**

* Redesigned restriction storage (less resource usage)
* Updated Catalan translation

**Version 1.8.1**

* Workaround for crash while importing in some situations / some devices
* Updated source code borrowed from Android
* Updated Hungarian translation

**Version 1.8**

* Randomization of: ([issue](https://github.com/M66B/XPrivacy/issues/418))
	* Android ID
	* Country
	* GSF ID
	* IMEI
	* Location
	* Phone number
	* Serial#
* Added hints to settings ([issue](https://github.com/M66B/XPrivacy/issues/422))
* Updated Japanese Translation
* Updated Polish translation
* Updated Portuguese translation
* Updated Swedish translation
* Updated Simplified Chinese translation
* Updated Vietnamese translation

**Version 1.7.30 BETA!**

* Fixed Skype crash ([issue](https://github.com/M66B/XPrivacy/issues/424))
* Fixed navigate from usage view crash ([issue](https://github.com/M66B/XPrivacy/issues/426))
* Intent for import/export ([issue](https://github.com/M66B/XPrivacy/issues/421))
* Randomization of: ([issue](https://github.com/M66B/XPrivacy/issues/418))
	* MAC address
* Updated French translation
* Updated German translation
* Updated Lithuanian translation

**Version 1.7.29 BETA!**

* Go to method restriction from application usage view ([issue](https://github.com/M66B/XPrivacy/issues/409))
* Fixed usage data for contacts ([issue](https://github.com/M66B/XPrivacy/issues/395))
* Fixed navigation to application in wiki ([issue](https://github.com/M66B/XPrivacy/issues/394))
* Better detection of sh / su command
* Updated Czech translation
* Updated French translation
* Updated German translation
* Updated Japanese Translation
* Updated Norwegian translation
* Updated Russian translation
* Updated Slovak translation
* Updated Swedish translation

**Version 1.7.28 BETA!**

* Export/import selected accounts/contacts *for the same device only* ([pro version](http://www.xprivacy.eu/) only)
* Fixed crash while filtering ([issue](https://github.com/M66B/XPrivacy/issues/402))
* Updated Simplified Chinese translation
* Updated Slovenian translation

**Version 1.7.27 BETA!**

* Pre-load application icons again
* Moved restricting statusbar notifications and C2DM to new category *Notifications*
* Updated German translation

**Version 1.7.26 BETA!**

* Performance improvements
	* Caching of fallback restrictions
	* Usage private executors with normal priority for async tasks
* Faster application details view by fetching data asynchronous
* Faster usage view by applying the holder pattern
* Revert "Run privacy provider in separate process" ([issue](https://github.com/M66B/XPrivacy/issues/384))
* Moved clipboard restrictions for *System* to new *Clipboard* category ([issue](https://github.com/M66B/XPrivacy/issues/359))
* Add restrictions to the *System* category: ([issue](https://github.com/M66B/XPrivacy/issues/373), [issue](https://github.com/M66B/XPrivacy/issues/387))
	* android.intent.action.PACKAGE_CHANGED
	* android.intent.action.PACKAGE_DATA_CLEARED
	* android.intent.action.PACKAGE_FIRST_LAUNCH
	* android.intent.action.PACKAGE_FULLY_REMOVED
	* android.intent.action.PACKAGE_NEEDS_VERIFICATION
	* android.intent.action.PACKAGE_VERIFIED
	* android.intent.action.EXTERNAL_APPLICATIONS_AVAILABLE
	* android.intent.action.EXTERNAL_APPLICATIONS_UNAVAILABLE
	* com.google.android.c2dm.intent.REGISTRATION
	* com.google.android.c2dm.intent.RECEIVE
* New setting for serial number ([issue](https://github.com/M66B/XPrivacy/issues/388))
* Updated Lithuanian translation
* Updated Vietnamese translation

**Version 1.7.25 BETA!**

* Faster application list by fetching data asynchronous and using [these tricks](http://developer.android.com/training/improving-layouts/smooth-scrolling.html)
* Reduce memory usage by limiting number of parallel usage data updates
* Updated Bulgarian translation
* Updated Catalan translation
* Updated Slovak translation

**Version 1.7.24 BETA!**

* Fixed for Android 4.3 ([issue](https://github.com/M66B/XPrivacy/issues/372))
* Updated French translation
* Updated Japanese Translation
* Updated Simplified Chinese translation
* Updated Slovenian translation

**Version 1.7.23 BETA!**

* Run privacy provider in separate process
* Updated Catalan translation

**Version 1.7.22 BETA!**

* Always show categories with data usage (even without permssions)
* Performance improvements (enum hooks)
* Updated Slovak translation

**Version 1.7.21 BETA!**

* Show permissions for indivudual functions
* Restructured method hooking
* Updated Portuguese translation

**Version 1.7.20 BETA!**

* Restructured meta data (restriction/function definitions)

**Version 1.7.19 BETA!**

* Restrict [BluetoothDevice](http://developer.android.com/reference/android/bluetooth/BluetoothDevice.html)
* Restrict [ClipboardManager](http://developer.android.com/reference/android/content/ClipboardManager.html) (category *System*) ([issue](https://github.com/M66B/XPrivacy/issues/359))
* 4.3: restrict [NotificationListenerService](http://developer.android.com/reference/android/service/notification/NotificationListenerService.html) (category *System*) ([issue](https://github.com/M66B/XPrivacy/issues/360))
* Better fake offline state location providers
* More accurate usage data (in case of Android denying permission)
* Performance and compatibility improvements

**Version 1.7.18 BETA!**

* Option to show usage data from the application details view
* Fixed usage data ([issue](https://github.com/M66B/XPrivacy/issues/364), [issue](https://github.com/M66B/XPrivacy/issues/365))
* Revert 1.7.16 change

**Version 1.7.17 BETA!**

* Contacts: also check for raw contacts ([issue](https://github.com/M66B/XPrivacy/issues/361))

**Version 1.7.16 BETA!**

* Run provider in separate process to solve heap problems

**Version 1.7.15 BETA!**

* Performance improvements (the usage data will be reset because of this, but **not** the restrictions)
* Disable Android version warning for version 4.3
* Updated telephony function permissions ([issue](http://forum.xda-developers.com/showpost.php?p=43949917&postcount=1820))
* Updated German translation
* Updated Polish translation
* Updated Russian translation
* Updated Vietnamese translation

**Version 1.7.14 BETA!**

* Request large heap

**Version 1.7.13 BETA!**

* Filter usage data by restricted or not (menu)
* Display icon if restricted in usage data view
* Menu to refresh usage data
* Less memory usage during import ([issue](https://github.com/M66B/XPrivacy/issues/329))
* Export file name with date / import with file chooser ([issue](https://github.com/M66B/XPrivacy/issues/327))
* Share exported file ([issue](https://github.com/M66B/XPrivacy/issues/337))
* Updated target SDK version to 18 (Android 4.3)
* 4.3: restrict [getGroupIdLevel1](http://developer.android.com/reference/android/telephony/TelephonyManager.html)
* 4.3: restrict [ACTION_RESPOND_VIA_MESSAGE](http://developer.android.com/reference/android/telephony/TelephonyManager.html)
* 4.3: restrict [getAccountsByTypeForPackage](http://developer.android.com/reference/android/accounts/AccountManager.html)
* Performance improvements
* Updated German translation
* Updated Japanese translation
* Updated Lithuanian translation
* Updated Simplified Chinese translation

**Version 1.7.10 BETA!**

* Open application settings when clicking in the data usage view

**Version 1.7.9 BETA!**

* Performance improvements
* Consider no Android permissions required as having Android permissions for a category
* Added data usage view for debugging purposes
* Updated Norwegian translation
* Updated Slovak translation

**Version 1.7.8 BETA!**

* Performance improvements
* Filter on internet permission
* Transparent spinner background ([issue](https://github.com/M66B/XPrivacy/issues/323))
* Attempt to fix select contacts crash (reported on XDA)
* Display uid's (as discussed on XDA)
* Added Vietnamese translation
* Updated Catalan translation
* Updated French translation
* Updated Lithuanian translation
* Updated Polish translation
* Updated Turkish translation

**Version 1.7.7**

* Restrict [Google auth](https://developer.android.com/reference/com/google/android/gms/auth/GoogleAuthUtil.html)
* Fetch accounts/contacts in the background
* Show orange triangles for function usage
* Display icon for frozen (not enabled) applications, thanks @[Looki75](http://forum.xda-developers.com/member.php?u=2468642) for the icon
* Added Lithuanian translation
* Updated Catalan translation
* Updated French translation
* Updated German translation
* Updated Japanese translation
* Updated Russian translation
* Updated Simplified Chinese translation
* Updated Slovak translation

**Version 1.7.6**

* Added disclaimer on first run
* Removed duplicate *getByName* in category *internet*

**Version 1.7.5**

* Show disabled applications when not filtering system applications
* Marked *getActiveNetworkInfo* and *getNetworkInfo* as dangerous
* Updated Italian translation

**Version 1.7.4**

* Rebuild application list on package change ([issue](https://github.com/M66B/XPrivacy/issues/226))
* Fake offline location providers (not enabled by default) ([issue](https://github.com/M66B/XPrivacy/issues/266))
* Use existing settings when installing an application again ([issue](https://github.com/M66B/XPrivacy/issues/295))
* Added Serbian translation
* Updated Dutch translation
* Updated French translation
* Updated Portuguese translation
* Updated Slovenian translation

**Version 1.7.3**

* Updated Spanish translation
* Fixed YouTube crash ([issue](https://github.com/M66B/XPrivacy/issues/297))

**Version 1.7.2**

* Better hide internet connectivity, thanks @[sorgelig](https://github.com/sorgelig)
* Fixed exporting dangerous functions ([issue](https://github.com/M66B/XPrivacy/issues/283))
* Location through Google Play services restricted (function *connect*) ([issue](https://github.com/M66B/XPrivacy/issues/233))
* Added Portuguese translation
* Updated Czech translation
* Updated Hebrew translation
* Updated Greek translation
* Updated Spanish translation

**Version 1.7**

* Restrict access to /proc (fixed again)
* Restrict package change notifications ([issue](https://github.com/M66B/XPrivacy/issues/256))
* Restrict *addGeofence* and *getLastLocation* (hidden function)
* Restrict *getWifiApConfiguration* (hidden function), thanks @[vipere](https://github.com/vipere)
* Restrict DNS in category *Internet*, thanks @[sorgelig](https://github.com/sorgelig) ([issue](https://github.com/M66B/XPrivacy/issues/132))
* Marked *location/getScanResults* as dangerous
* Higher resolution launcher icons ([source](http://openiconlibrary.sourceforge.net/gallery2/?./Icons/mimetypes/tango-style/application-pgp-signature.png))
* Sort function names
* Fixed leaking SSID on Android 4.2+
* Fixed notification icons
* Updated Catalan translation
* Updated French translation
* Updated German translation
* Updated Japanese translation
* Updated Simplified Chinese translation
* Updated Slovenian translation
* Updated Swedish translation

**Version 1.6.6**

* Reverted "Restrict access to /proc" (again)
* Updated Arabic translation
* Update French translation
* Updated Polish translation

**Version 1.6.5**

* Explicitly allow /proc for Android

**Version 1.6.4**

* User interface improvements
* Stability improvements
* Restrict access to /proc (fixed)
* Updated Catalan translation
* Updated Simplified Chinese translation
* Updated Slovak trabnslation

**Version 1.6.3**

* Fixed toggling all in main view
* Add Catalan translation

**Version 1.6.2**

* Do not apply dangerous restriction categories to new applications / all
* Add legend for colors in help
* Fixed filtering after switching theme

**Version 1.6.1**

* Reverted "Restrict access to /proc"
* Integrate clear icon into application name filter
* Updated Polish translation

**Version 1.6**

* Restrict access to /proc ([issue](https://github.com/M66B/XPrivacy/issues/227))
* New internet icon, thanks @[Looki75](http://forum.xda-developers.com/member.php?u=2468642)
* Replace expert mode by color coding, thanks @[Looki75](http://forum.xda-developers.com/member.php?u=2468642) for the colors
* Always allow drilling down in the application details view
* Fixed registering property names
* Added Swedish translation
* Updated French translation
* Updated Polish translation
* Updated Simplified Chinese translation

**Version 1.5**

* User interface improvements
* Performance improvements
* More generic matching of system properties
* Move categories internet, storage and system to expert mode ([issue](https://github.com/M66B/XPrivacy/issues/213), [issue](https://github.com/M66B/XPrivacy/issues/214))
* Setting to filter system applications ([issue](https://github.com/M66B/XPrivacy/issues/215))
* Option to set a template for new applications / application all menu ([issue](https://github.com/M66B/XPrivacy/issues/17))
* New colored icon set, thanks @[Looki75](http://forum.xda-developers.com/member.php?u=2468642) ([issue](https://github.com/M66B/XPrivacy/issues/206))
* Menu to select allowed contacts for an application ([issue](https://github.com/M66B/XPrivacy/issues/41))
* Increase length of ICCID to 20 digits ([issue](https://github.com/M66B/XPrivacy/issues/222))
* Filter categories by permission in the details view ([issue](https://github.com/M66B/XPrivacy/issues/225))
* Fixed checking for E-mail permission
* Fixed restricting recording in some situations ([issue](https://github.com/M66B/XPrivacy/issues/209))
* Fixed navigation of application detailed view ([issue](https://github.com/M66B/XPrivacy/issues/205))
* Fixed clearing export/import done notification
* Added Danish translation
* Updated Hungarian translation
* Updated Japanese translation
* Updated Norwegian translation
* Updated Simplified Chinese translation
* Updated Slovak translation
* Updated Turkish translation

**Version 1.4**

* Menu to select allowed accounts for an application
* Enable navigate up again
* Added info icon to application view
* Menu to restrict all applications ([issue](https://github.com/M66B/XPrivacy/issues/172))
* Restrict E-mail provider ([issue](https://github.com/M66B/XPrivacy/issues/199))
* Restrict application provider (category system)
* Fixed geocoding name feedback ([issue](https://github.com/M66B/XPrivacy/issues/198))
* Updated Japanese translation
* Updated Polish translation
* Updated Simplified Chinese translation

**Version 1.3**

* Dark and light holo theme, thanks @[Looki75](http://forum.xda-developers.com/member.php?u=2468642) for the icons and reviewing
* Filter on used AND name AND restricted
* Icon to clear text filter
* Display application version number in new/updated notification
* Increased maximum length of MNC to three digits ([issue](https://github.com/M66B/XPrivacy/issues/178))
* Some peformance improvements
* Fixed storage/internet restriction sometimes not working ([issue](https://github.com/M66B/XPrivacy/issues/174))
* Fixed shell commands starting with *sh* and *su*, like *show*, thanks @[Tungstwenty](https://github.com/Tungstwenty)
* Fixed stuck notification bar while exporting ([issue](https://github.com/M66B/XPrivacy/issues/170))
* Updated Arabic translation
* Updated simplified Chinese translation
* Updated Czech translation
* Updated French translation
* Updated Hungarian translation
* Updated Japanese translation
* Updated Norwegian translation
* Updated Polish translation
* Updated Slovak translation
* Updated Slovenian translation
* Updated Turkish translation

**Version 1.2**

* Setting for SIM serial# ([ICCID](http://en.wikipedia.org/wiki/ICCID#ICCID)) ([issue](https://github.com/M66B/XPrivacy/issues/167))
* Fixed possible location listener leak, thanks @[Tungstwenty](https://github.com/Tungstwenty)
* Fixed MAC address for network info
* Fixed application list leak ([issue](https://github.com/M66B/XPrivacy/issues/157))

**Version 1.1**

* *All* view (disabled check box means *some* restrictions) ([issue](https://github.com/M66B/XPrivacy/issues/154))
* Filter applications by permissions (default enabled)
* Restrict activity manager (running/recent processes/services/tasks) ([issue](https://github.com/M66B/XPrivacy/issues/157))
* Restrict app widget manager ([issue](https://github.com/M66B/XPrivacy/issues/157))
* Add Wi-Fi scan result to location category (Google Maps)
* Setting for subscriber ID ([IMSI](http://en.wikipedia.org/wiki/International_mobile_subscriber_identity)) ([issue](https://github.com/M66B/XPrivacy/issues/166))
* Filter disabled applications ([issue](https://github.com/M66B/XPrivacy/issues/165))
* Fixed switching filtering
* Fixed empty default settings (empty MCC, MNC, country, etc)
* Fixed navigate up (ICS)
* Added Hungarian translation
* Updated Polish translation
* Updated simplified Chinese translation
* Updated Slovak translation

**Version 1.0**

* Display geocoded address
* Setting for GSF ID, thanks @[vipere](https://github.com/vipere) ([issue](https://github.com/M66B/XPrivacy/issues/149))
* Settings for [MCC](https://en.wikipedia.org/wiki/Mobile_country_code), [MNC](https://en.wikipedia.org/wiki/Mobile_Network_Code) and country ([ISO 3166-1](http://en.wikipedia.org/wiki/ISO_3166-1))
* Delete existing settings before import
* Fixed switching between filtering selected/used
* Added Italian translation
* Updated Polish translation
* Updated Slovenian translation

**Version 0.43**

* Split media/sdcard storage restriction (expert only) ([issue](https://github.com/M66B/XPrivacy/issues/125))
* Move shell restriction out of expert mode, load/Library by default not restricted
* Move XPrivacy files to the folder .xprivacy ([pro version](http://www.xprivacy.eu/) only) ([issue](https://github.com/M66B/XPrivacy/issues/27))
* Fake disconnected state for internet restriction ([issue](https://github.com/M66B/XPrivacy/issues/132))
* Fake unmounted state for storage restriction (external storage only)
* Fake Google services framework ID, not restricted by default, thanks @[vipere](https://github.com/vipere) ([issue](https://github.com/M66B/XPrivacy/issues/134))
* Progress bar while loading applications, thanks @[Tungstwenty](https://github.com/Tungstwenty)
* Removed boot restriction
* Check package manager service, Wi-Fi info and activity thread for compatibility
* [Geocoding](http://en.wikipedia.org/wiki/Geocoding) of location names ([issue](https://github.com/M66B/XPrivacy/issues/139))
* Fixed internet/storage restriction for ICS ([issue](https://github.com/M66B/XPrivacy/issues/123))
* Fixed keyboard popup, thanks @[vipere](https://github.com/vipere)
* Fixed and improved fake phone data ([issue](https://github.com/M66B/XPrivacy/issues/116), see also [Restrictions](https://github.com/M66B/XPrivacy#restrictions))
* Fixed fake SSID for Android 4.2+ ([issue](https://github.com/M66B/XPrivacy/issues/116))
* Fixed rebuilding application list on orientation change
* Added Czech translation
* Added Norwegian translation
* Added Turkish translation
* Updated German translation
* Updated Japanese translation
* Updated Polish translation
* Updated Russian translation
* Updated Simplified Chinese translation
* Updated Slovak translation
* Updated Slovenian translation

**Version 0.42**

* Display notification when XPrivacy is not enabled also when Xposed installer is not present
* Better ICS compatibility ([issue](https://github.com/M66B/XPrivacy/issues/108))
* Fixed usage data for Build.SERIAL
* Fixed crash on back ([issue](https://github.com/M66B/XPrivacy/issues/112))
* Added Russian translation
* Updated French translation
* Updated German translation
* Updated Japanese translation
* Updated Polish translation
* Updated Slovenian translation

**Version 0.41**

* Filter applications by used data ([issue](https://github.com/M66B/XPrivacy/issues/79))
* Display notification when XPrivacy is not enabled in Xposed after boot ([issue](https://github.com/M66B/XPrivacy/issues/101))
* **Experimental** support for Android ICS 4.0.x
* Attempt to fix delay when applying boot restriction
* Added Polish translation
* Updated Slovak translation

**Version 0.40**

* Restrict NFC discovery
* Restrict access to (internal) media storage

**Version 0.39**

* Restrict user dictionary ([issue](https://github.com/M66B/XPrivacy/issues/89))
* Notification for updated applications (restrictions will be left alone) ([issue](https://github.com/M66B/XPrivacy/issues/96))

**Version 0.38**

* Added check box to filter restricted applications ([issue](https://github.com/M66B/XPrivacy/issues/79))
* Options to set IMEI and phone number ([issue](https://github.com/M66B/XPrivacy/issues/39))
* Use set MAC address for ro.boot.btmacaddr/ro.boot.wifimacaddr
* Removed info icon from methods
* Updated Slovak translation

**Version 0.37**

* Use fastscroll for application browser (thanks @[Tungstwenty](https://github.com/Tungstwenty))
* Application option menu *Launch*, *Settings* and *Play Store* (thanks @[Tungstwenty](https://github.com/Tungstwenty))
* Restrict LocationManager.sendExtraCommand (aGPS data)
* Info links to [wiki](http://wiki.faircode.eu/index.php?title=XPrivacy) ([issue](https://github.com/M66B/XPrivacy/issues/65))
* Fixed potential resource leak in phone state listener
* Fixed incompatible location manager message ([issue](https://github.com/M66B/XPrivacy/issues/83))
* Fixed incompatible telephony manager message ([issue](https://github.com/M66B/XPrivacy/issues/82))
* Increased thread priority to fetch app list
* Added Spanish translation
* Updated Japanese translation
* Updated Slovenian translation

**Version 0.36**

* Forgot to remove debug code

**Version 0.35**

* Speed up import ([pro version](http://www.xprivacy.eu/) only) ([issue](https://github.com/M66B/XPrivacy/issues/70))
* Remove pro menu when pro version activated
* Send support information for detected incompatibilities ([issue](https://github.com/M66B/XPrivacy/issues/82), [issue](https://github.com/M66B/XPrivacy/issues/83))
* Fixed potential account data leak ([issue](https://github.com/M66B/XPrivacy/issues/75))
* Fixed potential resource leak in location listener
* Updated German translation
* Updated simplified Chinese translation

**Version 0.34**

* Show edit icon in category browser

**Version 0.33**

* Option to switch between light/dark theme

**Version 0.32**

* Move *Shell* category to expert mode
* Export/import function restrictions ([pro version](http://www.xprivacy.eu/) only)
* Ongoing notification while exporting/importing
* Added link to restrictions in help
* Prevent application details reached from notification to show up in recent
* Added Japanese translation

**Version 0.31**

* Show storage folder in about
* Support for XPrivacy Pro from Google play
* Fixed Skype crash ([issue](https://github.com/M66B/XPrivacy/issues/50))

**Version 0.30**

* Option to set MAC address
* Workaround Skype crash ([issue](https://github.com/M66B/XPrivacy/issues/50))
* Fixed notification for uninstalled apps ([issue](https://github.com/M66B/XPrivacy/issues/51))
* Attemp to fix initial lag ([issue](https://github.com/M66B/XPrivacy/issues/59))
* Added Romanian translation

**Version 0.29**

* Fixed initial display of restrictions

**Version 0.28**

* Fixed wrong usage data in app list

**Version 0.27**

* Register more usage data (orange triangle)
* Display time of last usage (application restriction details)
* Highlight system applications (in expert mode)
* New expander symbol
* Menu *All* in application details to toggle all check boxes
* Action bar navigate up ([issue](https://github.com/M66B/XPrivacy/issues/12))
* Refresh application list when toggling expert mode
* Basic application search/filter ([issue](https://github.com/M66B/XPrivacy/issues/13))
* Added French translation
* Added Slovak translation
* Added simplified Chinese translation
* Updated Greek translation

**Version 0.26**

* Restrict process builder (shell, superuser) ([issue](https://github.com/M66B/XPrivacy/issues/44))

**Version 0.25**

* Fixed network usage data
* Fixed text wrapping application info
* Fixed to strict network restrictions (network state available now)
* Fixed to strict system restrictions (start activity available now)
* Fixed multiple installation notifications ([issue](https://github.com/M66B/XPrivacy/issues/42))
* Restrict bluetooth MAC / devices
* More compatibility checks
* Granular restrictions in expert mode only
* Hide indicator when no functions in restriction group
* Start application from application details icon
* Restrict shell commands, including superuser
* Restrict loading libraries (category shell)

**Version 0.24**

* Display application version
* More granular restrictions
* Restrict system properties:
	* ro.gsm.imei
	* net.hostname
	* ro.boot.serialno
	* ro.boot.wifimacaddr
	* ro.boot.btmacaddr
	* Let me know if there more should be restricted
* Fixed location battery drain ([issue](https://github.com/M66B/XPrivacy/issues/38))

**Version 0.23**

* Location range check
* Other location input method
* Simplified user interface
* Added Hebrew translation

**Version 0.22**

* Renamed category *Actions* to *Calling* again
* Moved opening links to new category *View*
* Export/import settings, like expert, location (export/import features [pro version](http://www.xprivacy.eu/) only)

**Version 0.21**

* Settings for latitude/longitude (Christmas Island is at latitude -10.5, longitude 105.667)
* Undo existing restrictions for apps when importing restrictions (only for apps with exported settings)

**Version 0.20**

* Consider XPrivacy as system app (for expert mode)
* Fixed category browser ([issue](https://github.com/M66B/XPrivacy/issues/30), [fix](https://github.com/M66B/XPrivacy/pull/28))
* Notify when new application installed ([feature request](https://github.com/M66B/XPrivacy/issues/10))
* Notify when new system application is installed in expert mode (default allow)

**Version 0.19**

* More environment checks when starting
* More privacy (fallback procedure when low memory)

**Version 0.18**

* Basic check for updates (manually)
* Link to [pro version](http://www.xprivacy.eu/)
* Renamed category *Calling* to *Actions* and moved opening browsers links into this category

**Version 0.17**

* User interface improvements
* Display system apps in expert mode only
* Restrict opening links ([issue](https://github.com/M66B/XPrivacy/issues/15))
* Restrict serial number (system/build properties)
* Export/import ([issue](https://github.com/M66B/XPrivacy/issues/18)) ([pro version](http://www.xprivacy.eu/) only)

**Version 0.16**

* Removed XPrivacy from *Manage apps*
* Added application list to XPrivacy

**Version 0.15**

* Restrict Android (expert mode)
* Restrict Google Service Framework (see also [limitations](https://github.com/M66B/XPrivacy#limitations))
* Restrict Wi-Fi BSSID, IP and SSID
* Restrict network info
* No default deny for updated apps

**Version 0.14**

* Restrict calling (untested)
* Restrict sending SMS
* Restrict sending MMS (untested)
* Fixed setting restrictions in the app list

**Version 0.13**

* Internet restriction (revoke permission)
* Remove restrictions/audit trail when uninstalling an app
* Custom write permission for privacy provider
* Restriction caching (Play store installed app browse should be fast again)

**Version 0.12**

* Android version check
* Check if XPrivacy is enabled
* Async app list fetch
* Several user interface improvements
* Expert mode: prevent app start
* Added Dutch/Flemish translation
* Added Bulgarian translation, thanks [borislavba](https://github.com/borislavba)
* Added German translation, thanks [NosferatuAlucard](https://github.com/NosferatuAlucard)
* Added Greek translation, thanks [mikeNG](https://github.com/mikeNG)
* Added Slovenian translation, thanks [kv1dr](https://github.com/kv1dr)

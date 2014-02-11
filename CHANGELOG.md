Changelog
=========

**Release types**

* TEST: new or updated features with a higher risk for bugs
* BETA: new or updated features with a lower risk for bugs
* STABLE: all known bugs fixed; low risk for bugs

Test and beta releases will have experimental functions enabled by default.

**Experimental functions**

* None at this time

**Next release**

* Rewritten URI restrictions for better compatibility
* Simplified restricting / allowing contacts
* Added restriction for browser downloads
* Updated traditional Chinese translation

[Open issues](https://github.com/M66B/XPrivacy/issues?state=open)

**Version 1.99.39 BETA**

* Fixed allowing contacts in some situations ([issue](/../../issues/1155))
* Fixed checkbox logic when *Restrict dangerous functions* was not set, thanks @[jpeg729](https://github.com/jpeg729)
* Fixed keyboard interfering with on demand restricting dialog (reported on XDA)
* Improved performance
* Updated German translation

**Version 1.99.38 TEST**

* Fixed allowing contacts in some situations ([issue](/../../issues/1155))
* Show URI for content providers
* Added restriction for ICC provider (SIM contacts)
* Added restriction for profile provider (contacts)
* Added restriction for contacts provider (fallback)
* Option to disable asking per function, thanks @[jpeg729](https://github.com/jpeg729)
* Updated Arabic translation

**Version 1.99.37 BETA**

* Fixed all issues reported with the support info
* Fixed layout of on demand restricting dialog for long functions/parameters
* Increased on demand restricting timeout to 20 seconds ([issue](/../../issues/1293))
* When registering, nicely ask for e-mail address
* Better protection of IPCs
* Updated German translation
* Updated Lithuanian translation

**Version 1.99.36 BETA**

* Fixed filter count (adding on-demand filter)
* XPrivacy will not start with LBE Security master installed ([issue](/../../issues/1231))
* Respect *Restrict dangerous functions* setting when fetching restrictions

**Version 1.99.35 BETA**

* Fixed all issues reported with the support info
* Fixed a locking bug for new installations, which resulted in a bootloop ([issue](/../../issues/1282))
* Clear button for filters, thanks @[jpeg729](https://github.com/jpeg729) ([issue](/../../issues/1281))
* Added filter for on-demand restricting, thanks @[jpeg729](https://github.com/jpeg729)
* Smooth count down for on-demand restricting, thanks @[jpeg729](https://github.com/jpeg729)
* Updated documentation, thanks @[Jimmy34742](https://github.com/Jimmy34742) and reviewers
* Updated Dutch translation
* Updated German translation
* Updated Spanish translation

**Version 1.99.34 BETA**

* Fixed all issues reported with the support info
* Fixed database locking problem
* Attempt to fix usage data list
* Improved layout of on demand restricting dialog
* Added category and function name to *Restricted by XPrivacy* message
* Updated Dutch translation
* Updated Slovak translation
* Updated Polish translation

**Version 1.99.33-3 TEST**

* Fixed all issues reported with the support info
* Fixed caching of category restrictions for on demand restricting
* Fixed a few corner cases in on demand restricting logic
* Fixed on demand dialog not appearing ([issue](/../../issues/1261))
* Set category to restricted on change for on demand restricting
* Remember last choice for *Apply to entire category* and *Once for ... seconds*
* Display on demand restriction status in application list
* Clearing restrictions from the application list will enable on demand restricting
* Increased usage data list to maximum 500 entries (was 250 entries)
* If a corrupt database is detected:
	* Backup old database
	* Create new, empty database
	* This is default Android behavior
* Automatically handle *removeOnAccountsUpdatedListener*, *removeUpdates* and *GMS.removeLocationUpdates*
* Toggle will apply template and take into account dangerous functions ([issue](/../../issues/1266))
* Notify usage for *Calling* and *Clipboard* category and *action.VIEW* (like the *Media* category)
* Display account name when retrieving accounts through Google Play services
* Display URL for *View/loadUrl*
* Fixed cursor blinking with an ugly hack
* New icon for filter, thanks @[danielmmmm](https://github.com/danielmmmm)
* Updated Dutch translation
* Updated Lithuanian translation

**Version 1.99.32 TEST**

* Fixed respecting dangerous while applying template ([issue](/../../issues/1251))
* Fixed fetching function exceptions
* Moved filter to action bar, thanks @[jpeg729](https://github.com/jpeg729) ([issue](/../../issues/1240))
* Support info for user interface errors
* Rewritten on demand restricting logic ([issue](/../../issues/1245)) ([issue](/../../issues/1247)) ([issue](/../../issues/1253))
* Added progress bar for on demand restricting time out
* Show shell command/library ([issue](/../../issues/1258))
* Show account name/type ([issue](/../../issues/1258))
* Show /proc/... ([issue](/../../issues/1258))
* Limited usage data display to prevent crash
* Updated traditional Chinese translation
* Updated Japanese translation
* Updated Lithuanian translation
* Updated Polish translation

**Version 1.99.31 TEST**

* Fixed on demand asking for secondary users
* Fixed on demand asking for same restriction again
* Fixed restrictions undone when using on demand restricting in some situation
* Disabling a restriction category will delete exceptions in application details view ([issue](https://github.com/M66B/XPrivacy/issues/1198))
* No on demand restricting while device is locked ([issue](https://github.com/M66B/XPrivacy/issues/1243))
* No on demand restricting while device is sleeping ([issue](https://github.com/M66B/XPrivacy/issues/1243))
* Several speed optimizations for the user interface
* Checking integrity of privacy database on boot and better error handling in privacy service

**Version 1.99.30 TEST**

* Fixed not restricting if not using on demand ([issue](https://github.com/M66B/XPrivacy/issues/1242))
* Fixed multiple on demand dialogs ([issue](https://github.com/M66B/XPrivacy/issues/1243))
* Broken on demand asking for same restriction

**Version 1.99.29 TEST**

* Fixed on demand choice not sticking
* Fixed on demand asking for same restriction
* Fixed on demand reboot problem ([issue](https://github.com/M66B/XPrivacy/issues/1238))
* Added setting for height ([issue](https://github.com/M66B/XPrivacy/issues/1179))
* Added restriction for *getGpsStatus* ([issue](https://github.com/M66B/XPrivacy/issues/1179))
* Removed restriction *Srv.getConfiguredNetworks*, use [Pry-Fi](http://forum.xda-developers.com/showthread.php?t=2631512)

**Version 1.99.28 TEST**

* Attempt to fix reboot in relation to on demand ([issue](https://github.com/M66B/XPrivacy/issues/1238))
* Upgrading *Srv.getConfiguredNetworks* (dangerous function)
* Updated Lithuanian translation

**Version 1.99.27 TEST**

* Fixed some invalid *restart required* messages
* Fixed restriction of functions without usage data
* Added restriction for *Srv.getConfiguredNetworks*
* Updated Dutch translation

**Version 1.99.26 TEST**

* Less restrictive database file permissions, always set on boot
* Support for multi SIM IPC restrictions (Huawei and maybe others)
* Added *act on category* check box to restricting on demand prompt, thanks @[jpeg729](https://github.com/jpeg729)
* Added application icon to restricting on demand prompt
* On demand restricting default enabled (there is a new main setting to disable it)
* If LBE Security Master is enabled, only applications can be restricted ([issue](https://github.com/M66B/XPrivacy/issues/1231))
* Fixed randomize on access not sticking for latitude and longitude
* Updated German translation
* Updated Slovak translation

**Version 1.99.25 BETA**

* Fixed handling for automated export, thanks @[jpeg729](https://github.com/jpeg729) ([issue](https://github.com/M66B/XPrivacy/issues/1211))
	* See the [FAQ question 6](https://github.com/M66B/XPrivacy#FAQ6) for how to
* Added function name to on demand restricting dialog
* Moved database back to /data/xprivacy, see also the [FAQ question 6](https://github.com/M66B/XPrivacy#FAQ6)

**Version 1.99.24 BETA**

* Fixed accessing Google services ([issue](https://github.com/M66B/XPrivacy/issues/1206)) ([issue](https://github.com/M66B/XPrivacy/issues/1209))
* Compatibility with LBE Security Master ([issue](https://github.com/M66B/XPrivacy/issues/1167))
* Display message for *media* restrictions
* Added *deny once* (this will be cached for 15 seconds)
* Added restriction for *getAuthenticatorTypes* (accounts)
* Added restrictions for *getCurrentSync*, *getCurrentSyncs* and *getSyncAdapterTypes* (accounts)
* Added restriction for *onConfigurationChanged* ([issue](https://github.com/M66B/XPrivacy/issues/1201))
* Restrict isolated processes (since JellyBean)
* Disabled [largeHeap](http://developer.android.com/reference/android/R.attr.html#largeHeap) to conserve memory
* Disabled [allowBackup](http://developer.android.com/reference/android/R.attr.html#allowBackup) for more privacy
* Enable on demand restricting for new applications only ([issue](https://github.com/M66B/XPrivacy/issues/1208))
* Updated Dutch translation
* Updated Lithuanian translation

**Version 1.99.23 BETA**

* Prevent bootloops with not yet migrated restrictions
* Fixed clearing restrictions / applying template for application groups ([issue](https://github.com/M66B/XPrivacy/issues/1202))
* Added restriction for MCC/MNC found in configuration ([issue](https://github.com/M66B/XPrivacy/issues/1201))
* Updated Lithuanian translation

**Version 1.99.22 BETA**

* Fixed crash on saving main settings
* Fixed support info popup for method not found

**Version 1.99.21 BETA**

* Restricting on demand
* Fixed erased settings staying cached
* Faster sorting, thanks @[jpeg729](https://github.com/jpeg729)
* Change state to gray when unrestricting ([issue](https://github.com/M66B/XPrivacy/issues/1190))
* Updated Dutch translation
* Updated French translation
* Updated Lithuanian translation
* Updated Slovak translation

**Version 1.99.20 BETA**

* Fixed restricting accounts in some situations
* Added application sort, thanks @[jpeg729](https://github.com/jpeg729)
* Saving tab state (category, filters), thanks @[jpeg729](https://github.com/jpeg729) ([issue](https://github.com/M66B/XPrivacy/issues/1035))
* Restrict access to Gmail information ([issue](https://github.com/M66B/XPrivacy/issues/1080))
* Added XPrivacy version and device name to export file name ([issue](https://github.com/M66B/XPrivacy/issues/1116))
* Using zero altitude for fake locations ([issue](https://github.com/M66B/XPrivacy/issues/1179))
* Updated Dutch translation
* Updated French translation
* Updated Lithuanian translation
* Updated Polish translation

**Version 1.99.19 BETA**

* Fixed always randomizing application settings when global randomization enabled

**Version 1.99.18 BETA**

* Further simplify application specific settings: no check mark or empty value means use global value
* Migration will no longer migrate empty values; existing empty values will be erased from the database
* Display message *XPrivacy is incompatible with ...* (currently LBE Security Master only)

**Version 1.99.17 BETA**

* Fixed opening wrong application view from notification ([issue](https://github.com/M66B/XPrivacy/issues/717))
* Run upgrade after migrate ([issue](https://github.com/M66B/XPrivacy/issues/1165))
* Close application details view on removing the application
* Added checks for LBE Security Master and for accessibility of the database

**Version 1.99.16 BETA**

* Fixed empty settings values ([issue](https://github.com/M66B/XPrivacy/issues/1164))
* Added restriction for *NetworkInfo.getExtraInfo* ([issue](https://github.com/M66B/XPrivacy/issues/1175))
* Updated Czech translation
* Updated German translation

**Version 1.99.15 BETA**

* Fixed global application setting not sticking ([issue](https://github.com/M66B/XPrivacy/issues/1164))
* Always allow randomization ([issue](https://github.com/M66B/XPrivacy/issues/1159))
* Display application switch state as enabled/disabled in main list
* Updated Chinese translation

**Version 1.99.14 TEST**

* Fixed randomize on boot setting being restored ([issue](https://github.com/M66B/XPrivacy/issues/1164))
* Fixed crash on start in some situations ([issue](https://github.com/M66B/XPrivacy/issues/1168))
* Fixed usage data vertical alignment
* Updated Slovak translation

**Version 1.99.13 TEST**

* Updated German translation
* Moved database back again due to permission problems ([issue](https://github.com/M66B/XPrivacy/issues/1160))

**Version 1.99.12 TEST**

* Show restrictions of system applications, even when disabled
* Moved database back due to permission problems ([issue](https://github.com/M66B/XPrivacy/issues/1129))

**Version 1.99.11 TEST**

* Added a switch to enable/disable all application restrictions
* Improved device registration procedure
* Prevent pollution of usage data ([issue](https://github.com/M66B/XPrivacy/issues/1140))
* Improved randomization of subscriber ID ([issue](https://github.com/M66B/XPrivacy/issues/1158))

**Version 1.99.10 TEST**

* Fixed usage data display for restriction categories
* Fixed a bug in randomizing the GSF ID
* Rewrote migration process, should solve continues migration and should be faster
* Added setting to enable restriction of system components (Android), default **disabled**
* Added setting to disable usage data collection, default enabled
* Marked IPC functions as not dangerous
* Updated Dutch translation
* Updated Lithuanian translation
* Updated Norwegian translation
* Updated traditional Chinese translation

**Version 1.99.9 TEST**

* Fixed crash on rotation change
* Added checks for all hooks

**Version 1.99.8 TEST**

* Batch set restrictions on fetch (performance)
* Added service restriction cache again (performance)
* Updated German translation
* Updated Lithuanian translation

**Version 1.99.7 TEST**

* Attempt to fix update service dying
* Fixed toggling category restrictions from application list
* Fixed toggling function restrictions from application details view
* Changed memory class for service settings cache to heap size >= 32MB

**Version 1.99.6 TEST**

* Fixed export progress, thanks @[jpeg729](https://github.com/jpeg729)
* Fixed some settings not migrated correctly ([issue](https://github.com/M66B/XPrivacy/issues/1127))
* Fixed allowing applications for *queryIntentActivities* ([issue](https://github.com/M66B/XPrivacy/issues/1147))
* Corrected application name and version in update notifications, thanks @[jpeg729](https://github.com/jpeg729) ([issue](https://github.com/M66B/XPrivacy/issues/1112))
* Added update service with progress notifications for migration, randomization and upgrade
* Allowed secondary users to set restrictions
* Kill application is not experimental anymore and can kill applications only
* Moved privacy database to application data folder ([issue](https://github.com/M66B/XPrivacy/issues/1129))
* Added dialog for export, import, submit, fetch and toggle, big thanks @[jpeg729](https://github.com/jpeg729)
* Added caching before restriction database (will only be used if the heap size >= 64 MiB)
* Show half check box for restricted categories with only dangerous functions
* Updated Arabic translation
* Updated Dutch translation
* Updated French translation
* Updated German translation
* Updated Lithuanian translation
* Updated Slovak translation

**Version 1.99.5 EXPERIMENTAL**

* Fixed deleting all usage data
* Replaced Play service library by local interface definition (smaller application size)
* Removed obsolete restriction GMS.connect
* Guarantee migrate, upgrade, randomize threads keep running

**Version 1.99.4 EXPERIMENTAL**

* Fixed usage data in application list
* Fixed setting *IPC*, *Storage* and *View* restrictions

**Version 1.99.3 EXPERIMENTAL**

* Fixed security exception reported with support info
* Fixed restart required notification
* Option to register device on custom e-mail address
* Restored restrictions *getNetwork...* and *getSim...* ([issue](https://github.com/M66B/XPrivacy/issues/1125))
* User defined dangerous functions, long press a function name to toggle ([issue](https://github.com/M66B/XPrivacy/issues/869))

**Version 1.99.2 EXPERIMENTAL**

* Removed restriction *getPackagesForUid*
* Migrate on boot completed event and display notification migration completed
* Attempt to fix security exception (based on received support info)
* Moved meta.xml to Meta class
* Cache permissions for better performance
* Optimized usage data getting (should solve most of the performance issue)
* Half check box for restricted IPC category (special case)
* Use inbox style notifications ([issue](https://github.com/M66B/XPrivacy/issues/1112))

**Version 1.99.1 EXPERIMENTAL**

* **Redesigned restriction/settings engine/database**
	* Existing settings will be migrated automatically
	* No more missing and always up-to-date usage data
	* Less processor and memory usage
	* New features possible
* Better method for hooking the bluetooth manager
* Better method for hooking the package manager
* Added restriction for *getPackagesForUid*, *getPackagesHoldingPermissions* (JellyBean MR2) and *queryIntentContentProviders* (Kitkat)
* Lower case randomized Android ID
* Simple registration procedure for submitting restrictions
* Moved IPC to separate category
* Replaced *getNetwork...* and *getSim...* by system property restrictions
* Upgrade settings in separate thread
* Updated Arabic translation
* Updated Chinese translation
* Updated Lithuanian translation
* Updated Slovak translation
* Updated Vietnamese translation

**Version 1.11.13 EXPERIMENTAL**

* Experimental: restriction for direct inter-process communication (System/IPC)
* Experimental: new, faster way of getting restrictions/settings

**Version 1.11.12 BETA**

* Use secure connection for [submitting and fetching restrictions](https://crowd.xprivacy.eu/)
* Skip upgrade for dangerous functions
* Restored original location and telephony restrictions

**Version 1.11.11 TEST**

* Fixed restricting locations for some Android versions ([issue](https://github.com/M66B/XPrivacy/issues/1102))
* Fixed restricting incoming phone number for some Android versions
* Fixed removing location and phone state listener
* Fixed location client always restricting locations
* Fixed fake value for SIM (ICC) operator name

**Version 1.11.10 TEST**

* Fixed fake values for get network operator/sim info
* Fixed phone state listener cast errors
* Updated Lithuanian translation

**Version 1.11.9 TEST**

* Fixed usage data with a dot in the name
* Added an Easter egg (don't ask)
* Added cell location restrictions within the phone process
* Added phone/network type restrictions within the phone process
* Added phone property restrictions (static phone info)
* Added phone registry restrictions (phone state listener)
* Added phone sub info restrictions (volatile phone info)
* Write warnings and errors to a new private log file
* Suppressing com.google.android.gms.* method errors
* Increase retry count for reading settings files
* Updated Tagalog translation

Replacing the location and phone restrictions is ready for testing now.

**Version 1.11.8 BETA**

* Fixed restricting locations acquired using Google Play services
* Fixed maximum number count message for submit and increased maximum to ten
* Fixed location listener casting ([issue](https://github.com/M66B/XPrivacy/issues/1094))
* Fixed reading settings file in some situations ([issue](https://github.com/M66B/XPrivacy/issues/1094))
* Fixed force close from update notification for removed application ([issue](https://github.com/M66B/XPrivacy/issues/51))
* Added restriction for Motorola's contact provider *blur*, thanks @[liudongmiao](https://github.com/liudongmiao)
* Updated minimum API version numbers, thanks @[liudongmiao](https://github.com/liudongmiao)
* Updated XML utils to KitKat version
* Restore application state color from imported settings ([issue](https://github.com/M66B/XPrivacy/issues/1065))
* Added import from the application view ([issue](https://github.com/M66B/XPrivacy/issues/1096))
* Enable Android usage data by default
* Added restriction for [getDefaultAdapter](http://developer.android.com/reference/android/nfc/NfcAdapter.html#getDefaultAdapter\(android.content.Context\)), thanks @[liudongmiao](https://github.com/liudongmiao)
* Disable XPrivacy view restrictions on XPrivacy view actions
* Improved debug logging (read settings file, build application list)
* Updated German translation
* Updated Polish translation
* Updated simplified Chinese translation
* Updated Slovak translation

[Open issues](https://github.com/M66B/XPrivacy/issues?state=open)

**Version 1.11.7 TEST**

* Sharper check marks, thanks @[jpeg729](https://github.com/jpeg729)
* Select/clear all visible/invisible action, thanks @[jpeg729](https://github.com/jpeg729)
* Toggle (clear/set) selected applications ([issue](https://github.com/M66B/XPrivacy/issues/1031))
* Fetch/submit selected applications ([issue](https://github.com/M66B/XPrivacy/issues/1031))
* Import selected applications ([issue](https://github.com/M66B/XPrivacy/issues/1031))
* Better location restrictions
* Updated simplified Chinese translation
* Updated traditional Chinese translation
* Updated French translation
* Updated Lithuanian translation

**Version 1.11.6 BETA**

* Fixed disabling application settings when using global settings ([issue](https://github.com/M66B/XPrivacy/issues/1050))
* Fixed display issues and scroll lag ([issue](https://github.com/M66B/XPrivacy/issues/1049)) ([issue](https://github.com/M66B/XPrivacy/issues/1059))
* Fixed check marks not visible with some themes ([issue](https://github.com/M66B/XPrivacy/issues/1057))
* Updated Chinese translation
* Updated Dutch translation
* Updated Slovak translation
* Updated Vietnamese translation

**Version 1.11.5 BETA**

* Fixed batch fetching system applications ([issue](https://github.com/M66B/XPrivacy/issues/1048))
* Added grayed usage data icon to help
* Updated Chinese translation
* Updated Tagalog translation

**Version 1.11.4 BETA**

* Fixed scroll lag on slower devices
* Fixed displaying wrong information in scroll lists sometimes
* Fixed settings not sticking in first three minutes ([issue](https://github.com/M66B/XPrivacy/issues/1042))
* Updated Polish translation
* Updated Spanish translation

**Version 1.11.3 TEST**

* Reverted "Prevent applications from bypassing Android APIs" ([issue](https://github.com/M66B/XPrivacy/issues/1039))

**Version 1.11.2 TEST**

* Fixed disarranged categories ([issue](https://github.com/M66B/XPrivacy/issues/1033))
* Fixed race conditions in list views (resulting in strange glitches sometimes)
* Fixed reloading when navigating up ([issue](https://github.com/M66B/XPrivacy/issues/1034))
* Fixed multiple filters running simultaneously, thanks @[jpeg729](https://github.com/jpeg729) ([issue](https://github.com/M66B/XPrivacy/issues/1036))
* Notify action for application settings ([issue](https://github.com/M66B/XPrivacy/issues/955))
* Display grayed usage data icon for methods with no usage data ([issue](https://github.com/M66B/XPrivacy/issues/878))
* Prevent applications from bypassing Android APIs by directly calling the binder

**Version 1.11.1 TEST**

* Fixed getting restrictions for multi-user environments ([issue](https://github.com/M66B/XPrivacy/issues/357))
* Fixed restricting internet and storage for multi-user environments ([issue](https://github.com/M66B/XPrivacy/issues/357))
* Fixed clearing existing restrictions on import, thanks @[jpeg729](https://github.com/jpeg729)
* Experimental functions enabled by default
* Sorted localized restriction categories, thanks @[jpeg729](https://github.com/jpeg729)
* Themed tri-state check boxes, thanks @[jpeg729](https://github.com/jpeg729)
* Added location restrictions for Google Play services ([issue](https://github.com/M66B/XPrivacy/issues/1011))
* Added Tagalog translation
* Updated Chinese translations
* Updated German translation
* Updated Vietnamese translation

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

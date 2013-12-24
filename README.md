XPrivacy
========

The ultimate, yet easy to use, privacy manager for Android

Index
-----

* [Description](https://github.com/M66B/XPrivacy#description)
* [Features](https://github.com/M66B/XPrivacy#features)
* [Restrictions](https://github.com/M66B/XPrivacy#restrictions)
* [Limitations](https://github.com/M66B/XPrivacy#limitations)
* [Compatibility](https://github.com/M66B/XPrivacy#compatibility)
* [Installation](https://github.com/M66B/XPrivacy#installation)
* [Upgrading](https://github.com/M66B/XPrivacy#upgrading)
* [Usage](https://github.com/M66B/XPrivacy#usage)
* [Permissions](https://github.com/M66B/XPrivacy#permissions)
* [Frequently asked questions](https://github.com/M66B/XPrivacy#frequently-asked-questions)
* [Support](https://github.com/M66B/XPrivacy#support)
* [Changelog](https://github.com/M66B/XPrivacy/blob/master/CHANGELOG.md)
* [Similar solutions](https://github.com/M66B/XPrivacy#similar-solutions)
* [News](https://github.com/M66B/XPrivacy#news)
* [Contributing](https://github.com/M66B/XPrivacy#contributing)
* [License](https://github.com/M66B/XPrivacy#license)

Description
-----------

XPrivacy can prevent applications from leaking privacy sensitive data.
XPrivacy can restrict the categories of data an application can access.
This is done by feeding an application with no or fake data.
There are several data categories which can be restricted, for example *contacts* or *location*.
For example, if you restrict access to contacts for an application,
this will result in sending an empty contact list to the application.
Similarly, restricting an application's access to your location
will result in a fake location being sent to the application.

XPrivacy doesn't revoke (i.e. block) permissions from an application,
which means that most applications will continue to work as before and won't force close.
There are two exceptions to this, access to the internet and to external storage (typically an SD card)
is restricted by denying access (revoking permissions).
There is no other way to implement this, since these permissions are handled by Android in a special way.
Android delegates handling of these permission to the underlying Linux network/file system.
XPrivacy will fake offline (internet) and/or unmounted (storage) state,
but some applications try to access the internet/storage nevertheless,
potentially resulting in crashes or error messages.

If restricting a category of data for an application results in problems for the application,
it is possible to allow access to the data category again to solve the issue.

By default, all newly installed applications will have no access to any data category at all,
to prevent a new application from leaking sensitive data right after installation.
Shortly after installing a new application,
XPrivacy will ask which data categories you want the new application to have access to.
XPrivacy comes with an application browser,
which allows you to quickly enable or disable applications' access to a particular data category
for example to view your calendar.
It is also possible to edit all data categories for one application.

To help you identify potential data leaks,
XPrivacy will monitor attempts made by all applications to access sensitive data.
XPrivacy will display an orange warning triangle icon as soon as data of a data category has been used.
If an application has requested Android permissions to access data in a data category,
this will be displayed with a green key icon.
XPrivacy will also display if an application has internet access,
indicating that the application poses a risk of sharing the data it obtains with an external server.

XPrivacy is built using the [Xposed framework](http://forum.xda-developers.com/showthread.php?t=1574401).
XPrivacy taps into a vast number of carefully selected functions of Android through the Xposed framework.
Depending on the function, XPrivacy conditionally skips execution of the original function
(for example when an application tries to set a proximity alert)
or alters the result of the original function (for example to return an empty message list).

XPrivacy has been tested with Android version 4.0.3 - 4.4.2 (ICS, JellyBean, KitKat),
and is reported to work with most Android variants, including stock ROMs.
Root access is needed to install the Xposed framework.


**XPrivacy was a lot of work, so please support this project**

Donate a few dollars for the [pro version](http://www.xprivacy.eu/)

OR

buy [the pro enabler](https://play.google.com/store/apps/details?id=biz.bokhorst.xprivacy.pro) from Google play

OR

[![Flattr](http://api.flattr.com/button/flattr-badge-large.png "Flattr This!")](http://flattr.com/thing/1491130/ "The ultimate, yet easy to use, privacy manager")


**Using XPrivacy is entirely at your own risk**

![Applications](https://raw.github.com/M66B/XPrivacy/master/screenshots/applications.png)
![Application](https://raw.github.com/M66B/XPrivacy/master/screenshots/application.png)
![Expert](https://raw.github.com/M66B/XPrivacy/master/screenshots/expert.png)
![Help](https://raw.github.com/M66B/XPrivacy/master/screenshots/help.png)
![Settings](https://raw.github.com/M66B/XPrivacy/master/screenshots/settings.png)
![Usage data](https://raw.github.com/M66B/XPrivacy/master/screenshots/usagedata.png)

Features
--------

* Simple to use
* No need to patch anything (no source, no [smali](https://code.google.com/p/smali/) or anything else)
* For any (stock) variant of Android version 4.0.3 - 4.4.2 (ICS, JellyBean, KitKat)
* Newly installed applications are restricted by default
* Displays data actually used by an application
* Free and open source

Restrictions
------------

For easy usage, data is restricted by category:

<a name="accounts"></a>
* Accounts
	* return an empty account list
	* return fake account info
	* return empty authorization tokens
<a name="browser"></a>
* Browser
	* return an empty bookmark list
	* return empty search history
<a name="calendar"></a>
* Calendar
	* return an empty calendar
<a name="calling"></a>
* Calling
	* prevent calls from being placed
	* prevent SMS messages from being sent
	* prevent MMS messages from being sent
	* prevent data messages from being sent
<a name="clipboard"></a>
* Clipboard
	* prevent paste from clipboard (both manual and from an application)
<a name="contacts"></a>
* Contacts
	* return an empty contact list
		* content://com.android.contacts/data
		* content://com.android.contacts/raw_contacts
		* content://com.android.contacts/phone_lookup
		* content://com.android.contacts/profile
<a name="dictionary"></a>
* Dictionary
	* return an empty user dictionary
<a name="email"></a>
* E-mail
	* return an empty list of accounts, e-mails, etc (provider)
<a name="identification"></a>
* Identification
	* return a fake Android ID
	* return a fake device serial number
	* return a fake host name
	* return a fake Google services framework ID
	* return file not found for folder /proc
	* return a fake Google advertising ID
	* return a fake system property cid (Card Identification Register)
	* return file not found for /sys/block/.../cid
	* return file not found for /sys/class/.../cid
	* return fake input device descriptor
<a name="internet"></a>
* Internet
	* revoke access to the internet
	* return fake disconnected state
	* return fake supplicant disconnected state
<a name="location"></a>
* Location
	* return a random or set location
	* return empty cell location
	* return an empty list of (neighboring) cell info
	* prevents geofences from being set
	* prevents proximity alerts from being set
	* prevents sending NMEA data to an application
	* prevent phone state from being sent to an application
		* Cell info changed
		* Cell location changed
	* prevent sending extra commands (aGPS data)
	* return an empty list of Wi-Fi scan results
	* prevents connecting to Google Play services
<a name="media"></a>
* Media
	* prevent recording audio (including from the microphone)
	* prevent taking pictures
	* prevent recording video
	* you will be notified if an application tries to perform any of these actions
<a name="messages"></a>
* Messages
	* return an empty SMS/MMS message list
	* return an empty list of SMS messages stored on the SIM (ICC SMS)
	* return an empty list of voicemail messages
<a name="network"></a>
* Network
	* return fake IP's
	* return fake MAC's (network, Wi-Fi, bluetooth)
	* return fake BSSID/SSID
	* return an empty list of Wi-Fi scan results
	* return an empty list of configured Wi-Fi networks
	* return an empty list of bluetooth adapters/devices
<a name="nfc"></a>
* NFC
	* prevent receiving NFC adapter state changes
	* prevent receiving NDEF discovered
	* prevent receiving TAG discovered
	* prevent receiving TECH discovered
<a name="notifications"></a>
* Notifications
	* prevent receiving statusbar notifications (Android 4.3+)
	* prevent [C2DM](https://developers.google.com/android/c2dm/) messages
<a name="overlay"></a>
* Overlay
	* prevent draw over / on top
<a name="phone"></a>
* Phone
	* return a fake own/in/outgoing/voicemail number
	* return a fake subscriber ID (IMSI for a GSM phone)
	* return a fake phone device ID (IMEI): 000000000000000
	* return a fake phone type: GSM (matching IMEI)
	* return a fake network type: unknown
	* return an empty ISIM/ISIM domain
	* return an empty IMPI/IMPU
	* return a fake MSISDN
	* return fake mobile network info
		* Country: XX
		* Operator: 00101 (test network)
		* Operator name: fake
	* return fake SIM info
		* Country: XX
		* Operator: 00101
		* Operator name: fake
		* Serial number (ICCID): fake
	* return empty [APN](http://en.wikipedia.org/wiki/Access_Point_Name) list
	* return no currently used APN
	* return an empty call log
	* prevent phone state from being sent to an application
		* Call forwarding indication
		* Call state changed (ringing, off-hook)
		* Mobile data connection state change / being used
		* Message waiting indication
		* Service state changed (service/no service)
		* Signal level changed
	* return an empty group identifier level 1
<a name="sensors"></a>
* Sensors
	* return an empty default sensor
	* return an empty list of sensors
<a name="shell"></a>
* Shell
	* return I/O exception for Linux shell
	* return I/O exception for Superuser shell
	* return unsatisfied link error for load/loadLibrary
<a name="storage"></a>
* Storage
	* revoke permission to the [media storage](http://www.chainfire.eu/articles/113/Is_Google_blocking_apps_writing_to_SD_cards_/)
	* revoke permission to the external storage (SD card)
	* return fake unmounted state
<a name="system"></a>
* System
	* return an empty list of installed applications
	* return an empty list of recent tasks
	* return an empty list of running processes
	* return an empty list of running services
	* return an empty list of running tasks
	* return an empty list of widgets
	* return an empty list of applications (provider)
	* prevent package add, replace, restart and remove notifications
<a name="view"></a>
* View
	* prevent links from opening in the browser
	* return fake browser user agent string
		* *Mozilla/5.0 (Linux; U; Android; en-us) AppleWebKit/999+ (KHTML, like Gecko) Safari/999.9*

Limitations
-----------

* /proc and system properties cannot be restricted for Android (serial number, IMEI, MAC address, etc)
* Phone number cannot be restricted for the standard phone application
* Internet and storage can only be restricted for applications/providers/services started by the Android package manager
* Due to its static nature [Build.SERIAL](http://developer.android.com/reference/android/os/Build.html#SERIAL) can only be randomized on application start and there is no usage data
* Due to a bug in Chromium the user agent cannot be restricted in all cases ([issue](https://github.com/M66B/XPrivacy/issues/825))
* Due to a custom implementation the clipboard cannot be restricted on some Samsung stock ROMs ([issue](https://github.com/M66B/XPrivacy/issues/857))
* You cannot restrict the Android ID used for submitting restrictions (only [pro version](http://www.xprivacy.eu/))
* There is no usage data for isolated processes (a new concept in Android 4.4)

Compatibility
-------------

XPrivacy has been tested with Android version 4.0.3 - 4.4.2 (ICS, JellyBean, KitKat),
and is reported to work with most Android variants, including stock ROMs.

Installation
------------

**Instead of following the steps below, you can use the [XPrivacy Installer](https://play.google.com/store/apps/details?id=biz.bokhorst.xprivacy.installer).**

It seems like a lot of steps, but it is done in no time:

1. Requirements:
	* Android version 4.0.3 - 4.4.2 (ICS, JellyBean, KitKat); check with *System Settings* > *About phone* > *Android version*
	* Custom recovery ([CWM](http://forum.xda-developers.com/wiki/ClockworkMod_Recovery), [TWRP](http://teamw.in/project/twrp2) or similar)
1. **Make a backup**
1. If not done already: root your device; the procedure depends on the brand and model of your device
	* You can find a guide [here](http://www.androidcentral.com/root) for most devices
1. Enable *System settings* > *Security* > *Unknown sources*
1. Install the [Xposed framework](http://forum.xda-developers.com/showthread.php?t=1574401)
	* Be sure to install [the latest version](http://dl.xposed.info/latest.apk)
	* The Xposed fix is not needed anymore
1. Download and install XPrivacy from [here](http://repo.xposed.info/module/biz.bokhorst.xprivacy)
	* Alternatively download from [here](http://d-h.st/users/M66B/?fld_id=19078#files)
1. Enable XPrivacy from the Xposed installer
1. Reboot

I do not recommend using XPrivacy in combination with any of the
[similar solutions](https://github.com/M66B/XPrivacy#similar-solutions),
because it could result in conflicts (with as possible consequence data leakage).

If you want to uninstall XPrivacy, you have two options:

1. Disable XPrivacy in the Xposed installer
1. Uninstall Xposed using the Xposed installer

Upgrading
---------

* **Make a backup**
* **Do not remove the previous version** (else your settings will get lost)
* Download the new version
* Install the new version over the previous version
* Reboot your device

When following this procedure your data will not leak, because the Xposed part of XPrivacy keeps running.

Usage
-----

The application starts in the main view, where at the top a data category can be selected.
By ticking one or more check boxes in the list below, the selected data category can be restricted for the chosen applications.
The default category is *All*, meaning that all data categories will be restricted.

Tapping on an application icon shows the detailed view, where all the data categories for the selected application can be managed.
This view will also appear by tapping on the notification that appears after updating or installing an application.
By default all data categories will be restricted for new installed applications to prevent leaking privacy sensitive data from the beginning.
You can change which data categories will be restricted by changing the *Template* available from the main menu.

Data categories exist to make it easier to manage restrictions.
The data categories in the detailed view can be drilled down to individual functions.
If the category is restricted, individual functions can be allowed by clearing the function check boxes.

To see it in action: try restricting the category *Identification* for
[Android Id Info](https://play.google.com/store/apps/details?id=com.bzgames.androidid)
or try restriction the category *Contacts* for the Contacts application.

**Applying some restrictions require restarting applications and/or your device**

If an application requested Android permissions for a data category,
the category will be marked with a green key icon.
If an application used/tried to use data, the data category will be marked with an orange warning triangle icon.
If an application has internet permissions a world globe icon will be shown.
These icons are just a guideline, because an application can access some privacy sensitive data without Android permissions,
for example the serial number of your device
and because is not possible to monitor data usage in each and every situation,
for example not for access to the internet or the external storage.
Be aware that an application could access the internet through other (sister) applications.

Enabling the internet or storage restriction means blocking access to the internet
or to the external storage (typically the SD card).
This may result in error messages and even in forced closes of the application.

Some category and function restrictions are considered dangerous.
These categories and functions are marked with a redish background color.
Some applications will crash if you restrict these categories and/or functions.

There are global settings and application specific settings,
respectively accessible from the menu of the application list and the menu of the application details view.
The global settings, like a randomized or set latitude/longitude, apply to all applications,
unless you set any application specific setings.
In that case all global settings are overriden by the application specific settings.
There is one special case: saving empty specific application settings (you can use the clear button)
will erase all application specific settings, so that the global settings will be used again.

The template, available from the main menu is applied to newly installed applications
or if you use the menu apply template from the application details view.

**Using XPrivacy is entirely at your own risk**

Permissions
-----------

XPrivacy asks for the following Android permissions:

* Accounts: to select accounts to allow for applications
* Contacts: to select contacts to allow for applications
* Boot: to check if XPrivacy is enabled
* Internet: to submit/fetch [crowd sourced restrictions](http://crowd.xprivacy.eu/)
* Storage: to export settings to the SD card (only [pro version](http://www.xprivacy.eu/))

If you don't like this, you can always restrict XPrivacy itself ...

Frequently asked questions
--------------------------

<a name="FAQ1"></a>
**(1) Will XPrivacy make my device slower?**

Maybe a little bit, but it will probably not be noticeable.

<a name="FAQ2"></a>
**(2) Does XPrivacy use a lot of memory or battery?**

Almost nothing.

<a name="FAQ3"></a>
**(3) Can you help me with rooting my device?**

There are already enough [guides](http://www.androidcentral.com/root) to help you to root your device.
Use your favorite search engine to find one.

<a name="FAQ5"></a>
**(5) How can I reset all XPrivacy settings?**

*Manage apps* > *XPrivacy* > *Clear data*

Reboot.

<a name="FAQ6"></a>
**(6) Can I backup XPrivacy and settings?**

Yes, you can, for example with [Titanium backup](https://play.google.com/store/apps/details?id=com.keramidas.TitaniumBackup),
but you can only restore into the same environment (device/ROM).
Exporting/importing settings will work across devices/ROMs.
To export/import settings you will need the [pro version](http://www.xprivacy.eu/).

<a name="FAQ10"></a>
**(10) Which functions are exactly restricted?**

Many, see [here](https://github.com/M66B/XPrivacy/blob/master/src/biz/bokhorst/xprivacy/XPrivacy.java) for all details.

<a name="FAQ12"></a>
**(12) How safe is XPrivacy?**

Great care has been taken to develop XPrivacy, nevertheless data could leak and applications can crash,
although this is fortunately rare.

<a name="FAQ14"></a>
**(14) I get 'Incompatible ...' !**

An internal check of XPrivacy failed, resulting in potential data leakage.
Please press *OK* to send me the support information, so I can look into it.

<a name="FAQ15"></a>
**(15) What is the procedure for a ROM update?**

The right order for ROM updates is:

1. Export XPrivacy settings
1. Enable flight mode
1. Reboot to recovery
1. Flash ROM
1. Flash Google apps (optional)
1. Re-activate Xposed using [Xposed toggle](http://forum.xda-developers.com/showpost.php?p=45188739)
1. Reboot to Android
1. Restore the android ID (when needed; with for example [Titanium backup](https://play.google.com/store/apps/details?id=com.keramidas.TitaniumBackup))
1. Clear XPrivacy data (please note that this will erase the imported pro license file if any)
1. Import XPrivacy settings
1. Disable flight mode
1. Fake network type (Wi-Fi, mobile)

(this assumes no data wipe and Xposed and XPrivacy installed before updating the ROM)

If you skip the XPrivacy import/clear/export steps some system applications can have the wrong restrictions,
because the uid's of these applications might have been changed.

For export/importing XPrivacy data you need [pro version](http://www.xprivacy.eu/).

<a name="FAQ16"></a>
**(16) Can I restrict an application with root access?**

Yes, you can by restricting su shell access.

<a name="FAQ17"></a>
**(17) Will restrictions be applied immediately?**

It can take up to 15 seconds before changes in restrictions will be effective, because of caching.
Changing the internet and storage restriction requires an application restart.
Please note that in many cases pressing *back*, only moves the application to the background.

<a name="FAQ18"></a>
**(18) Can XPrivacy ask for restrictions on demand / display a message on data usage?**

It cannot always, since it works deep within Android,
and therefore it is IMHO not a good idea to ask for restrictions when it could,
because this will probably result into confusion only.

There is a lot of privacy sensitive data processed within Android,
especially if there are a lot of applications installed.
It would slow down your device significantly if XPrivacy would notify data usage.

Newly installed applications are by default fully restricted.
Restricting an application should not result into any force closes (crashes),
please create an issue if this happens (see the support section below),
it only means that an application cannot see the restricted data.
If an application should see the data, you can remove the associated restriction at any time.

<a name="FAQ19"></a>
**(19) Does XPrivacy have a firewall?**

Yes, you can restrict internet access for any application.
If you want to partly enable internet, for example for Wi-Fi only,
you will have to use a firewall application, like [AFWall+](http://forum.xda-developers.com/showthread.php?t=1957231).
The reason is that XPrivacy works within Android
and detailed firewall rules can only be applied within the Linux kernel.

<a name="FAQ21"></a>
**(21) I get 'Unable to parse package'**

This means the downloaded apk is corrupt.
Try disabling your popup blocker or download using another computer.

<a name="FAQ22"></a>
**(22) How can I make a logcat?**

Enable logging using the settings menu and see [here](http://forum.xda-developers.com/showthread.php?t=1726238).

<a name="FAQ23"></a>
**(23) Where are the settings of XPrivacy stored?**

The restriction settings of XPrivacy are stored as private application data.
It is possible to backup the application and the data,
but you can restore it onto the same environment (device/ROM) only.
This is because Android assigns different uid's to the same applications on different devices.
Exporting settings on one device and importing settings onto another device will work,
but this requires the [pro version](http://www.xprivacy.eu/).
If you want to backup the exported settings, they are in the folder *.xprivacy* on the SD card.

<a name="FAQ25"></a>
**(25) Why doesn't undoing a data category restriction disable the function exceptions too?**

If you accidentally undo a data category restriction all the function exception would be lost.
The function exceptions only apply when the data category is restricted.

<a name="FAQ26"></a>
**(26) How can I export/import my settings?**

For this you need the [pro version](http://www.xprivacy.eu/).
Exported settings are stored in the folder *.xprivacy* in the file *XPrivacy.xml*.
You can copy this file to the same place on a second device.
When importing, settings are only applied to applications that exist on the second device.
This also applies to system applications.

Note that allowed accounts and allowed contacts (not the accounts and contacts itself)
can only be imported when the android ID is the same.
See question 15 about what to do when updating your ROM.

<a name="FAQ28"></a>
**(28) I have restricted locations but my GPS status icon still appears**

That is correct, XPrivacy only replaces the real location by a fake location.
It even uses the real location to randomize the fake location.
The idea is that everything should appear as normal as possible to an application.

<a name="FAQ29"></a>
**(29) How about multi-user support?**

Each application is identified by a *uid* and all restrictions are based on this unique identifier.
Each user has his/her own set of applications, which are identified by separate uids.
So, each user can manage the restrictions for the applications available to him/her.

<a name="FAQ30"></a>
**(30) Why is the location search in the settings disabled?**

Because some Google components are not installed.

<a name="FAQ31"></a>
**(31) Do I still need root after installing Xposed?**

No.

<a name="FAQ32"></a>
**(32) Why is XPrivacy not available in the Play store anymore?**

Read [here](http://forum.xda-developers.com/showpost.php?p=44427089&postcount=2433) why.

<a name="FAQ33"></a>
**(33) What is 'Template' used for?**

The template is used to apply restrictions to newly installed applications
and when you use the menu *Apply template*.

<a name="FAQ34"></a>
**(34) Will there be a iOS / Window phone version?**

No, because these OS'es are to closed to implement something like XPrivacy.

<a name="FAQ35"></a>
**(35) Will you restrict ...?**

* The device brand/manufacturer
* The device model/product name
* The device (phone) type
* The network type (mobile, Wi-Fi, etc.)
* Synchronization state
* Screen locking
* Display settings
* Wi-Fi settings
* Bluetooth settings
* Shortcuts
* Android version
* Vibration

No, because I don't consider this as privacy sensitive data (=able to identify you and collect data about you).
I am happy to add new restrictions for data that is really privacy sensitive.

<a name="FAQ36"></a>
**(36) What does expert mode?**

Expert mode disables the dangerous restrictions warning and enables some advanced settings.

<a name="FAQ37"></a>
**(37) Does XPrivacy work with SELinux (Fort Knox) ?**

Yes.

<a name="FAQ38"></a>
**(38) What is 'Android usage data' and 'Extra usage data' ?**

The setting *Android usage data* enables sending more usage data for Android itself (uid=1000).

The setting *Extra usage data* enables sending more usage data for all applications.

Enabling these settings can cause instability and/or decrease performance on some devices.

Usage data = orange triangles.

<a name="FAQ39"></a>
**(39) How does the tri-state check box work?**

The tri-state check box follows this pattern:

* Unchecked = no retrictions
* Solid square = some restrictions
* Check mark = all restrictions

Some/all is determined as follows:

* All category: some/all other categories
* Other categories: some/all functions

Be aware that by default categories and functions are filtered by permission,
so you may not see all of them. The check box state is independent of this.

<a name="FAQ40"></a>
**(40) How can I get usage data for all applications?**

Try enabling the setting *Extra usage data* from the main menu,
but be aware that this might cause instability on some ROMs.

<a name="FAQ41"></a>
**(41) Why are not all pro features available with the pro enabler?**

The [pro enabler](https://play.google.com/store/apps/details?id=biz.bokhorst.xprivacy.pro)
is in the Play store on request of some early XPrivacy users.
In the beginning there was just one pro feature: export/import of all restrictions and settings.
In a later stage fetching [crowd sourced restrictions](http://crowd.xprivacy.eu/)
were added as a pro feature.
Processing of the crowd sourced restrictions requires a big server which has to be paid for.
The low price of the pro enabler, don't forget Google takes already 30%,
didn't allow to give this feature for free to existing users.
Looking back I would never have put the pro enabler into the Play store,
but I cannot remove it anymore now, because of the existing users.

<a name="FAQ42"></a>
**(42) What should I do if an application force closes (crashes)?**

Take a look into the usage view of the application,
available through the menu in the application details view,
to see which functions the application uses.
Disable category and/or function restrictions one by one
until you have found the one causing the force close.
Help others by submitting your working restrictions.

<a name="FAQ43"></a>
**(43) Can XPrivacy handle non-Java applications?**

In general, due to the architecture of Android (isolated virtual machines),
any call to native libraries and binaries is through Java
and can thus be restricted by XPrivacy.
As far known any route to a native library or binary is covered by XPrivacy.

XPrivacy cannot hook into native libraries, but can prevent native libraries from loading.
This could break applications, like Facebook, but can prevent malware from doing its work.

XPrivacy can also restrict access to the Linux shell (including superuser),
to prevent native binaries from running.

You can find the restrictions in the *Shell* category.

Support
-------

If you encounter a bug please [create an issue](https://github.com/M66B/XPrivacy/issues).

Include a [logcat](#FAQ22) when relevant
(use [pastebin](http://pastebin.com/) or a similar service).

**Do not forget to enable XPrivacy logging using the settings menu!**

Please describe the exact steps to reproduce the issue
and include information about your device type and Android version.

If you have a feature request, please [create an issue](https://github.com/M66B/XPrivacy/issues).

If you have any question, you can leave a message in the [XDA XPrivacy forum thread](http://forum.xda-developers.com/showthread.php?p=42488236).

**Before submitting any issue please make sure you are running the latest version of XPrivacy.**

**Before submitting any issue please make sure XPrivacy is causing the problem by disabling XPrivacy.**

**One bug report / feature request per issue please!**

**Do not use my personal or XDA e-mail for bug reports and feature requests.**

Changelog
---------

The changelog has been moved [here](https://github.com/M66B/XPrivacy/blob/master/CHANGELOG.md).

Similar solutions
-----------------

* [PDroid](http://forum.xda-developers.com/showthread.php?t=1357056)
* [PDroid 2.0](http://forum.xda-developers.com/showthread.php?t=1923576)
* [OpenPDroid](http://forum.xda-developers.com/showthread.php?t=2098156)
* [LBE Privacy Guard](https://play.google.com/store/apps/details?id=com.lbe.security.lite)
* [CyanogenMod Incognito Mode](https://plus.google.com/100275307499530023476/posts/6jzWcRR6hyu)
* [Per App Settings Module](http://forum.xda-developers.com/showthread.php?t=2072081)
* [Android 4.3+ Permission Manager](http://www.androidpolice.com/2013/07/25/app-ops-android-4-3s-hidden-app-permission-manager-control-permissions-for-individual-apps/)

The *PDroid* family provides fake or no data, more or less in the same way as XPrivacy does.
A difference is that you need to patch Android and that there is (therefore) only limited stock ROM support.
The PDroid family is open source.

*LBE Privacy Guard* revokes permissions, which will make some applications unusable.
LBE Privacy Guard also features malware protecting and data traffic control.
Some consider the closed source code of Chinese origin as a problem.

The members of the PDroid family and XPrivacy hardly use memory, but LBE Privacy Guard does.

The CyanogenMod Incognito Mode seems not to be fine grained and provides only privacy for personal data,
if the associated content provider chooses to do so.

The *Per App Settings Module* revokes permissions like LBE Privacy Guard does.
This modules offers a lot of other, interesting features.

Android 4.3+ Permission Manager is like CyanogenMod Incognito Mode.

XPrivacy can restrict more data than any of the above solutions,
also for closed source applications and libraries, like Google Play services.

News
----

* [Manage Individual App Permissions with XPrivacy](http://www.xda-developers.com/android/manage-individual-app-permissions-with-xprivacy/) (June 20, 2013)
* [XPrivacy Gives You Massive Control Over What Your Installed Apps Are Allowed To Do](http://www.androidpolice.com/2013/06/23/xprivacy-gives-you-massive-control-over-what-your-installed-apps-are-allowed-to-do/) (June 23, 2013)
* [Protect Your Privacy with XPrivacy - XDA Developer TV](http://www.xda-developers.com/android/protect-your-privacy-with-xprivacy-xda-developer-tv/) (July 17, 2013)
* [XPrivacy Android - Schutz gegen Datensammler](http://www.kuketz-blog.de/xprivacy-android-schutz-gegen-datensammler/) (August 1, 2013)

Contributing
------------

Translations:

* Translate the strings in [this file](https://github.com/M66B/XPrivacy/blob/master/res/values/strings.xml)
* Omit lines with *translatable="false"*
* If you know how to, please create a [pull request](https://help.github.com/articles/using-pull-requests)
* Else send me the translated file [via XDA PM](http://forum.xda-developers.com/member.php?u=2799345)

Current translations:

1. Bulgarian (bg)
1. Catalan (ca)
1. Czech (cs)
1. Danish (da)
1. Dutch/Flemish (nl)
1. English
1. Estonian (ee)
1. Farsi (fa)
1. Finnish (fi)
1. French (fr)
1. German (de)
1. Greek (el)
1. Hebrew (he/iw)
1. Hindi (hi)
1. Hungarian (hu)
1. Irish (ga)
1. Italian (it)
1. Japanese (ja)
1. Lithuanian (lt)
1. Norwegian (nb-rNO, nn-rNO, no-rNO)
1. Polish (pl)
1. Portuguese (pt)
1. Romanian (ro)
1. Rusian (ru)
1. Serbian (sr)
1. Simplified Chinese (zh-rCN)
1. Slovak (sk)
1. Slovenian (sl)
1. Spanish (es)
1. Swedish (sv)
1. Traditional Chinese (zh-rTW)
1. Turkish (tr)
1. Ukrainian (ua)
1. Vietnamese (vi)

Restrict new data:

* Find the package/class/method that exposes the data (look into the Android documentation/sources)
* Figure out a way to get a context (see existing code for examples)
* Create a class that extends [XHook](https://github.com/M66B/XPrivacy/blob/master/src/biz/bokhorst/xprivacy/XHook.java)
* Hook the method in [XPrivacy](https://github.com/M66B/XPrivacy/blob/master/src/biz/bokhorst/xprivacy/XPrivacy.java)
* Write a before and/or after method to restrict the data
* Do a [pull request](https://help.github.com/articles/using-pull-requests) if you want to contribute

Using Eclipse:

* Clone the GitHub project to a temporary location
* Import the GitHub project into Eclipse, copy the files
* Close Eclipse and copy the project from the temporary location over the imported project
	* Make sure you copy all hidden files
* Add the Xposed library to the build path as described [here](https://github.com/rovo89/XposedBridge/wiki/Development-tutorial#making-the-project-an-xposed-module)

Testing:

* [Android Id Info](https://play.google.com/store/apps/details?id=com.bzgames.androidid)
* [Network Info II](https://play.google.com/store/apps/details?id=aws.apps.networkInfoIi)
* [SIM Card](https://play.google.com/store/apps/details?id=com.gsmdev.simcard)

Serious contributors do not have to donate for the [pro version](http://www.xprivacy.eu/).
New translations are considered as a serious contribution, but translating a few lines of text is not.

License
-------

[GNU General Public License version 3](http://www.gnu.org/licenses/gpl.txt)

Copyright (c) 2013-2014 [Marcel Bokhorst](http://blog.bokhorst.biz/about/)
([M66B](http://forum.xda-developers.com/member.php?u=2799345))

This file is part of XPrivacy.

XPrivacy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

XPrivacy is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XPrivacy.  If not, see [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).

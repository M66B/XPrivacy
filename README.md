XPrivacy
========

The ultimate, yet easy to use, privacy manager

Index
-----

* [Description](https://github.com/M66B/XPrivacy#description)
* [Features](https://github.com/M66B/XPrivacy#features)
* [Restricted data](https://github.com/M66B/XPrivacy#restricted-data)
* [Restricted actions](https://github.com/M66B/XPrivacy#restricted-actions)
* [Limitations](https://github.com/M66B/XPrivacy#limitations)
* [Installation](https://github.com/M66B/XPrivacy#installation)
* [Upgrading](https://github.com/M66B/XPrivacy#upgrading)
* [Usage](https://github.com/M66B/XPrivacy#usage)
* [Frequently asked questions](https://github.com/M66B/XPrivacy#frequently-asked-questions)
* [Support](https://github.com/M66B/XPrivacy#support)
* [Changelog](https://github.com/M66B/XPrivacy#changelog)
* [Similar solutions](https://github.com/M66B/XPrivacy#similar-solutions)
* [Contributing](https://github.com/M66B/XPrivacy#contributing)
* [License](https://github.com/M66B/XPrivacy#license)

Description
-----------

XPrivacy can prevent applications (including associated background services and content providers)
from leaking privacy sensitive data.
XPrivacy can restrict the categories of data an application can access.
This is done by feeding an application with no or fake data.
There are several data categories which can be restricted, for example *contacts* or *location*.
For example, if you restrict access to contacts for an application,
this will result in sending an empty contact list to the application if it requests access to your contacts.
Similarly, restricting an application's access to your location
will result in a random location being sent to the application.

XPrivacy doesn't revoke (i.e. block) permissions from an application,
which means that most applications will continue to work as before and won't force close.
There are two exceptions to this, access to the internet and to external storage (typically an SD card)
is restricted by denying access (revoking permissions).
There is no other way to realize this, since these permissions are handled by Android in a special way.
Android delegates handling of these permission to the underlying Linux network/file system.

If restricting a category of data for an application results in problems for that application,
it is possible to allow access to the data category again.

By default, all newly-installed applications will have no access to any data category at all,
to prevent a new application from leaking sensitive data after installation.
Shortly after installing a new application,
XPrivacy will ask which data categories you want the new application to have access to.
XPrivacy comes with a category browser,
which allows you to quickly enable or disable applications' access to a particular data category
(i.e. to view and control all access to the camera, for example).

To help you identify potential data leaks,
XPrivacy will monitor attempts made by all applications to access sensitive data.
XPrivacy will highlight (with a yellow triangle) a data category for an application
(or an application name in the category browser)
as soon as data of the data category has been used.
XPrivacy will also display if an application has internet access,
indicating that the application poses a risk of sharing the data it obtains with an external server.
If an application has requested Android permissions to access data in a data category,
it will also be displayed (with a green tick),
but this will only be shown when looking at an individual application,
since checking permissions for all applications is quite slow.

XPrivacy is built using the [Xposed framework](http://forum.xda-developers.com/showthread.php?t=1574401).
XPrivacy taps into a number of selected functions of Android through the Xposed framework.
Depending on the function, XPrivacy conditionally skips execution of the original function
(for example when an application tries to set a proximity alert)
or alters the result of the original function (for example to return empty calendar data).

XPrivacy has been tested with CyanogenMod 10 and 10.1 (Android 4.1 and 4.2),
and will likely work with any Android version 4.1 or 4.2 variant, including stock ROMs.
Root access is needed to install the Xposed framework.
Because of a bug in the Xposed framework, XPrivacy currently needs a fixed Xposed binary,
which is provided as download for both Android version 4.1 and 4.2.

**Using XPrivacy is entirely at your own risk**

![Xposed](https://raw.github.com/M66B/XPrivacy/master/screenshots/xposed.png) ![Applications](https://raw.github.com/M66B/XPrivacy/master/screenshots/applications.png)
![Categories](https://raw.github.com/M66B/XPrivacy/master/screenshots/categories.png) ![Category](https://raw.github.com/M66B/XPrivacy/master/screenshots/category.png)
![Application](https://raw.github.com/M66B/XPrivacy/master/screenshots/application.png) ![Help](https://raw.github.com/M66B/XPrivacy/master/screenshots/help.png)

Features
--------

* Simple to use
* No need to patch anything (no source, no [smali](https://code.google.com/p/smali/) or anything else)
* For any (stock) variant of Android version 4.1 or 4.2 (JellyBean)
* Newli installed applications are restricted by default
* Displays data actually used by an application
* Open source

Restricted data
---------------

* Accounts (including auth token)
* [APN](http://en.wikipedia.org/wiki/Access_Point_Name) data
* Application data (installed apps)
* Browser (bookmarks, searches)
* Calendar
* Contacts
* Identification (Android ID, serial number)
* Location (coarse/fine, cell location/info)
* Messages (SMS/MMS, including ICC SMS stored on SIM)
* Network (IPs, MACs, BSSID, SSID)
* Opening links
* Phone (call log, in/outgoing/voicemail number, phone ID/number, subscriber ID, SIM info, ISIM, IMPI, IMPU, MSISDN, network details)
* Recording audio (including microphone)
* Recording video
* Taking photos
* Voicemail **untested**

Restricted actions
------------------

* Calling **untested**
* Sending SMS
* Sending MMS **untested**
* Internet (revoke permission: no usage data)
* External storage (SD card) (revoke permission: no usage data)

Limitations
-----------

* Android can be restricted, but there will be no usage data available (orange triangle).
* The Google Services Framework (com.google.android.gsf) retrieves the Wi-Fi MAC address in an unusual way
(within a thread, without context), it can be restricted by XPrivacy, but there will be no usage data available.

Installation
------------

0. Requirement: Android 4.1+ (JellyBean), check with *System Settings* > *About phone* > *Android version*
1. Root your device, the procedure varies depending on the brand and model of your device
2. **Make a backup**
3. Install the [Xposed framework](http://forum.xda-developers.com/showthread.php?t=1574401), *including the disabler*
4. Install XPrivacy from [here](http://goo.im/devs/M66B/xprivacy)
5. Enable XPrivacy from the Xposed Installer app
6. Reboot into recovery
7. Flash the Xposed fix for your Android version from [here](http://goo.im/devs/M66B/xprivacy)
8. Reboot

Upgrading
---------

* Install the new version (replacing the previous version)
* Reboot your device

Usage
-----

* Start XPrivacy
* Select an application
* Set restrictions

To see it in action: try disabling *Identification* for [Android Id Info](https://play.google.com/store/apps/details?id=com.bzgames.androidid)
or try disabling *Contacts* for the Contacts application
(the Contacts application will continue to show a spinner, which is actually a bug in the Contacts application).

**Applying some restrictions requires an app restart**

If an application requested Android permissions for an data category,
the category will be marked with a green check mark icon.
If an application tried to use data, the data category will be marked with an orange triangle icon.
These icons are only a guideline, because an application can access privacy sensitive data without Android permissions,
for example the serial number of your device
and because is not possible to monitor data usage in each and every situation,
for example not for access to the internet or the external storage.

Enabling internet or storage restriction means blocking access to the internet or to external storage (typically the SD card).
This may result in error messages and even in force closes of the application.

Tricks:

* Click the application icon in the category browser to go to the app restriction settings
* Click the orange triangle to see the actual Android functions that were used by the app

**Using XPrivacy is entirely at your own risk**

Frequently asked questions
--------------------------

*Will XPrivacy make my device slower?*

Maybe a little bit, but it won't probably be not noticeable.

*Does XPrivacy use a lot of memory?*

Almost nothing.

*Can you help me with rooting my device?*

There are already enough guides to help you to root your device.
Use your favorite search engine to find one.

*Do I really need to install the Xposed fix?*

If you like to have all data restricted, yes.

*How can I reset all XPrivacy settings?*

*Manage apps* > *XPrivacy* > *Clear data*

*Can I backup XPrivacy and settings?*

Yes, you can, but you can only restore onto the same device.
Exporting/importing settings will work across devices.

*What is expert mode?*

In expert mode you can block applications from starting at device boot time (except content providers)
and you will be able to restrict system applications, including Android itself. **Be careful!**

*Will you block the iptables command or force online state?*

Maybe in a later stage.

*Will you make it possible to enter fake data?*

Maybe in a later stage.
For now I like to keep things as simple as possible for maximum stability.

*Which functions are exactly restricted?*

A lot, see [here](https://github.com/M66B/XPrivacy/blob/master/src/biz/bokhorst/xprivacy/XPrivacy.java) for all details.

*What did you fix in the Xposed framework?*

See [here](https://github.com/M66B/Xposed/commit/8a46f91bfd1381f78d1deb575041f51bae5d3dda).

Support
-------

If you encounter any bug or data leakage please [report an issue](https://github.com/M66B/XPrivacy/issues),
preferably including a [logcat](http://developer.android.com/tools/help/logcat.html)
(use [pastebin](http://pastebin.com/) or a similar service).

If you have any question or want to request a new feature,
you can leave a message in the [XDA XPrivacy forum thread](http://forum.xda-developers.com/showthread.php?p=42488236).

Changelog
---------

**Version 0.17**

* User interface improvements
* Display system apps in expert mode only
* Restrict opening links ([issue](https://github.com/M66B/XPrivacy/issues/15))
* Restrict serial number (system/build properties)
* Export/import ([issue](https://github.com/M66B/XPrivacy/issues/18)) (Pro version only)

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
* Fix setting restrictions in the app list

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

Similar solutions
-----------------

* [PDroid](http://forum.xda-developers.com/showthread.php?t=1357056)
* [PDroid 2.0](http://forum.xda-developers.com/showthread.php?t=1923576)
* [OpenPDroid](http://forum.xda-developers.com/showthread.php?t=2098156)

Contributing
------------

Translations:

* Translate the strings in [this file](https://github.com/M66B/XPrivacy/blob/master/res/values/strings.xml)
* Send the translated file to me [via an XDA PM](http://forum.xda-developers.com/member.php?u=2799345)

Restrict new data:

* Find the package/class/method that exposes the data (look into the Android documentation/sources)
* Figure out a way to get a context (see existing code for examples)
* Create a class that extends [XHook](https://github.com/M66B/XPrivacy/blob/master/src/biz/bokhorst/xprivacy/XHook.java)
* Hook the method in [XPrivacy](https://github.com/M66B/XPrivacy/blob/master/src/biz/bokhorst/xprivacy/XPrivacy.java)
* Write a before and/or after method to restrict the data
* Do a [pull request](https://help.github.com/articles/using-pull-requests) if you want to contribute

License
-------

[GNU General Public License version 3](http://www.gnu.org/licenses/gpl.txt)

Copyright (c) 2013 [Marcel Bokhorst](http://blog.bokhorst.biz/about/)
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

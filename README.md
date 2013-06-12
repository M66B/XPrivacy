XPrivacy
========

Privacy manager using the [Xposed framework](http://forum.xda-developers.com/showthread.php?t=1574401)

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
There is one exception to this, access to external storage (typically an SD card) is restricted by denying access.
There is no other way to realize this, since this permission is handled by Android in a special way.
Android delegates handling of this permission to the underlying Linux file system.

If restricting a category of data for an application results in problems for that application,
it is possible to allow access to the data category again.

By default, all newly-installed applications will have no access to any data category at all,
to prevent a new application from leaking sensitive information after installation.
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
indicating that the application poses a risk of sharing the information it obtains with an external server.
If an application has requested Android permissions to access data in a data category,
it will also be displayed (with a green tick),
but this will only be shown when looking at an individual application,
since checking permissions for all applications is quite slow.

XPrivacy is accessible for each application from the Android *Manage apps* menu.
The category browser is accessible from the application list or application drawer.

XPrivacy is built using the Xposed framework.
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

![Manage apps](https://raw.github.com/M66B/XPrivacy/master/screenshots/xposed.png) ![Manage apps](https://raw.github.com/M66B/XPrivacy/master/screenshots/manage_apps.png)
![App details](https://raw.github.com/M66B/XPrivacy/master/screenshots/app_details.png) ![Batch edit](https://raw.github.com/M66B/XPrivacy/master/screenshots/batch_edit_select.png)
![Batch edit](https://raw.github.com/M66B/XPrivacy/master/screenshots/batch_edit_apps.png) ![Batch edit](https://raw.github.com/M66B/XPrivacy/master/screenshots/help.png)

Features
--------

* Simple to use
* No need to patch anything (source, [smali](https://code.google.com/p/smali/))
* For any (stock) variant of Android version 4.1 or 4.2 (JellyBean)
* New apps are restricted by default
* Displays data actually used by an app
* Open source

Restricted data
---------------

* Accounts (including auth token)
* [APN](http://en.wikipedia.org/wiki/Access_Point_Name) data
* Application data (installed apps)
* Browser (bookmarks, searches)
* Calendar
* Contacts
* Identification (Android ID, Wi-Fi MAC address)
* Location (coarse/fine, cell location/info)
* Messages (SMS/MMS, voicemail: **untested**, ICC SMS)
* Phone (call log, in/outgoing/voicemail number, phone ID/number, subscriber ID, SIM info, ISIM, IMPI, IMPU, MSISDN, network details)
* Recording audio (including microphone)
* Recording video
* Reading sdcard (revoke permission)
* Taking photos

Installation
------------

1. Root your device
2. **Make a backup**
3. Install the [Xposed framework](http://forum.xda-developers.com/showthread.php?t=1574401), *including the disabler*
4. Install XPrivacy from [here](http://goo.im/devs/M66B/xprivacy)
5. Enable XPrivacy from the Xposed Installer app
6. Reboot into recovery
7. Flash the correct Xposed fix from [here](http://goo.im/devs/M66B/xprivacy)
8. Reboot

Upgrading
---------

* Uninstall the previous version
* Reboot your device
* Follow the installation instructions

Usage
-----

* Select *Manage apps* from the main menu
* Select an app
* Press the XPrivacy button

To see it in action: try disabling *Identification* for [Android Id Info](https://play.google.com/store/apps/details?id=com.bzgames.androidid)
or try disabling *Contacts* for the Contacts app.

**Applying some restrictions requires an app restart**

Enabling storage restriction means blocking access to external storage (typically the SD card).
Because of the nature of this restriction, there is no usage data for this restriction.
All other restrictions have usage data and send no or fake data.

Tricks:

* Click the application icon in the category browser to go to the app restriction settings
* Click the orange triangle to see the actual Android functions that were used by the app

Frequently asked questions
--------------------------

* Will you restrict internet access? No, you can use a firewall app, like [AFWall+](https://play.google.com/store/apps/details?id=dev.ukanth.ufirewall).
* Will you block outgoing SMS/MMS, the iptables command or force online state? No, XPrivacy is about restricting data, not about blocking actions.
* Will you make it possible to enter fake data? No, I want to keep things as simple as possible for maximum stability.
* Which functions are exactly restricted? See [here](https://github.com/M66B/XPrivacy/blob/master/src/biz/bokhorst/xprivacy/XPrivacy.java).
* What did you fix in the Xposed framework? See [here](https://github.com/M66B/Xposed/commit/8a46f91bfd1381f78d1deb575041f51bae5d3dda).
* Does XPrivacy use the Android permissions for something? No, Android permissions are just displayed as a guideline.
* How can I reset all XPrivacy settings? *Manage apps* > *XPrivacy* > *Clear data*
* Will XPrivacy make my device slower? Not noticeable.

Support
-------

If you encounter any bug or data leakage please [report an issue](https://github.com/M66B/XPrivacy/issues),
preferably including a [logcat](http://developer.android.com/tools/help/logcat.html)
(use [pastebin](http://pastebin.com/) or a similar service).

If you have any question or want to request a new feature,
you can leave a message in the [XDA XPrivacy forum thread](http://forum.xda-developers.com/showthread.php?p=42488236).

Changelog
---------

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

* [Strings](https://github.com/M66B/XPrivacy/blob/master/res/values/strings.xml)

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

<h2>XPrivacy Menus</h2>

<h3>Menu - App list</h3>
  * Tutorial - this will open the tutorial
  * Usage data - this will show the usage data for all apps for all categories, or the category select in the category selection dropdown
    * Menu - Usage data
      * Toggle filter - this will toggle between all and only denied restrictions
      * Refresh - this will refresh the current view
      * Clear - this will clear the entire usage data list
  * Toggle restrictions - this will allow you to apply a template to selected apps for either one or all categories
    * Category drop down - here you can select which category to apply the change to
    * Clear - this will clear restrictions for the selected category
    * Restrict (categories) - this will restrict the select selected category
    * Aplly template (categories) - this will apply the selected template to the selected category
    * Apply template (categories+functions) - this will apply the selected template to selected category and subfunctions
    * Enable on demand restriction - this will enable on demand for the selected applications
    * Disable on demand restrictions - this will disable on demand for the selected applications
  * Clear all XPrivacy data - this will delete all restrictions, settings, and whitelists. (Use with caution)
  * Export - this will create a restriction and setting backup of all or the currently selected apps *1
  * Import - this will import restrictions and settings from a backup for all or the selected apps *1
  * Submit restrictions - this will submit restrictions for the selected apps to the crowd server
  * Fetch restrictions - this will fetch crowd restrictions for the selected apps *1
  * Report issue - this will open a broswer to submit a new issue on GitHub
  * Switch theme - this will switch the UI between the light and dark theme
  * Tmeplate - this will allow you set restrictions for the default and alternate templates
    * Template selector drop down - here you can select which template you want to adjust
      * Note - functions marked as dangerous are indicated with a red background (predifined) or an orange background (user-defined). Longpressing on a function will change its dangerous status
  * Settings - here you can set global settings as well as global fake values
    * Update notifications - this will enable or disable update noticification for all app updates
    * Restrict on demand - this will enable or disable on demand for all apps
    * Show application usage data - this will turn usage data logging on or off for all apps
    * Debug log (requires restart) - this will turn on additional logging for troubleshooting
    * Expert mode
      * Restrict system components (Android) - this will turn restrictions for core android components (UID less that 10000) on or off
      * Use secure connections - this will enable or disable secure communicaitons with the XPrivacy server
    * FAKE DATA
      * Randomize on boot - this will randomize all global fake values on boot
      * Randomize now - this will randomize all global fake values
      * Clear - this will clear all global fake values and all 'Randomize on access' check marks
      * Flush cache - this will clear the server side restrictions cache
      * Randomize on access - here you can check which values should be randomized each time it access by an app
  * About - this will show information about the current XPrivacy version as well as license status

<h3>Menu - App detail view</h3>
  * Tutorial - this will open the tutorial
  * Usage data - this will show the usage data for the currently selected app
    * Menu - Usage data
      * Toggle filter - this will toggle between all and only denied restrictions
      * Refresh - this will refresh the current view
      * Clear - this will clear the usage data list for the currently selected app
  * Apply template - this will allow you to apply a template to the selected app for either one or all categories
    * Category drop down - here you can select which category to apply the change to
    * Clear - this will clear restrictions for the selected category
    * Restrict (categories) - this will restrict the select selected category
    * Aplly template (categories) - this will apply the selected template to the selected category
    * Apply template (categories+functions) - this will apply the selected template to selected category and subfunctions
    * Enable on demand restriction - this will enable on demand for the currently selected application
    * Disable on demand restrictions - this will disable on demand for the currently selected application
  * Clear - this will allow you to clear the restrictions, settings and whitelists of the currently selected app
  * Export - this will create a restrictions and setting backup of the currently seletect app *1
  * Import - this will import restrictions and settings for the currently selected app *1
  * Submit restrictions - this will submit restrictions for the currently selected app to the crowd server
  * Fetch restrictions - this will fetch crowd restrictions for the currently seleted app *1
  * Select accounts to allow - this can be used to allow certain accounts access to the account category while restricting it for others *1
  * Select applications to allow - this will allow the app to see allowed apps while restricting others *1 
  * Select contacts to allow - this will allow the apps to see certain contacts while restricting others *1
  * Manage whitelists - here previously defined white/blacklist items can be allowed/denied or removed from the list *1
    * Category drop down - Here you can select which white list category you want to manange
  * Settings - here you can application specific setting and fake values
    * Update notifications - this will enable or disable update noticification for app updates to the currently selected app
    * Restrict on demand - this will enable or disable on demand for the currently selected app
    * FAKE DATA
      * Randomize on boot - this will randomize app specific fake values on boot
      * Randomize now - this will randomize all app specific fake values
      * Clear - this will clear all app specific fake values and all 'Randomize on access' check marks
      * Randomize on access - here you can check which values should be randomized each time it access by the selected app
  
<h3>Application list buttons</h3>
* Select all - this will select or unselect all currently filtered applications
* Sort by - here you can chose how the application should be sorted
  * By name - this will sort by name A-Z
  * By uid- this will sort by UID number
  * By date installed - this will sort by installation date
  * By date updated - this will sort by app update date
  * By date modified (XPrivacy) - this will sort by restriciton modification time
  * Invert sort order - this will invert the above selected order. (I.E. By name will be shown Z-A)
* Filter - here you can filter the application list
  * Filter on data usage - this will filter on global data usage or data usage for the selected category (category drop down)
  * Filter on internet access - this will filter on apps which have requested internet access in android
  * Filter on permission - this will filter on apps that have requested permissions, either globally or for the selected category (category drop down)
  * Filter on restriction - this will filter on apps that have restrictions set, either globally or for the selected category (category drop down)
    * Negate - this will show apps without restrictions
  * Filter by on-demand - this will filter on apps that have on demand enabled, either globally or for the selected category (category drop down)
    * Negate - this will show apps where on demand is disabled
  * Filter on user applications
  * Filter on system appications
  * Clear all - this will clear all filters
* ? - this will show the help screen
* Category drop down - this will show category restrictions in the app list for the selected category
* Info button - this will open the XPrivacy GitHub page in a browser
* Search box - here you can search for a specific application by name or UID
* Restriction check mark - this will restrict or unrestict all or the currently selected category (category drop down) for an app
* On demand check mark - this will enable or disable on demand for an app or apply on demand to currently selected category (category drop down)

<h3>Application detail view buttons</h3>

* ? - this will show the help screen
* Info button - this will open the the crowd source restrictions for the currently selected app in a browser
* On/Off toogle- this will turn restrictions for the currently selected app on or off without deleting the restriction settings
* On demand ? - this will toggle on demand restrictions for the currently selected app
* Category drill down - this will show or hide the individual functions of a category
* Category info icon - this will open the category restrcions info on GitHub in a browser
* Function book icon - this will show additional information about the function, possibly with a link to the google documentaion
* Category restriction check mark - this will restrict or unrestrict the category for the currently selected app
* Category on demand check mark - this will enable or disalbe on demand popups to the functions in the cateogry for the currently selected app
* Function restrction check mark - this will restrict or unrestrict the indiviual function
* Functions on demand check mark - this will enable or disalbe on demand popups for the individual function
* Manage white list icon - this will be shown if a function has white/blacklist entries, pressing it will open the white list manager to the specific list *1

*1 - Pro feature (see [Xprivacy.eu](http://xprivacy.eu) for more info)

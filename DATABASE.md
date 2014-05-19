<h1>WARNING: MODIFYING THE DATABASES DIRECTLY MAY CAUSE ISSUES (INCLUDING BOOT LOOPS) WITH YOUR DEVICE. PROCEED AT YOUR OWN RISK AND ALWAYS PERFORM A BACKUP BEFORE CHANGING ANYTHING!</h1>

<h2>Introduction</h2>

<p>XPrivacy utilizes 2 databases (<em>xprivacy.db</em> and <em>usage.db</em>), both are located in <em>/data/system/xprivacy</em>. Making a file backup of the database cannot safely be done in a running system and should be done from recovery!</p>
<p>XPrivacy checks both the xprivacy database and usage database at system boot for integrity (using 'PRAGMA integrity_check'). If a database is found to be corrupt, the database is deleted, because repairing an sqlite database is mostly not possible (and Android doesn't have the tools for it installed). This can happen to the database of any application, but for XPrivacy it is of course a greater concern.Given the support info I receive, this fortunately happens rarely to the xprivacy database, but more to the usage database. The cause for this difference is that usage database is set to asynchronous mode for speed reasons (using 'PRAGMA synchronous=OFF').</p>
<p>The usage database is just an aid and not critical for the operation of XPrivacy. Both the xprivacy and usage database are compacted at boot (using 'VACUUM'). This saves space and is good for performance, but the disadvantage is that twice the size of the database on disk space is needed.</p>
<p>A full disk (/data/system is mounted on internal memory) is fatal for XPrivacy, because the database will become corrupt in this situation. Again looking at the support info, this also rarely happens.</p>
<p>All mentioned sqlite commands are properly documented on the <a href="http://www.sqlite.org">SQLite Website</a></p>

<h2>Accessing the databases:</h2>

<h3>From a PC:</h3>

<p><code>adb shell</code></p>
<p><code>sqlite3 /data/system/xprivacy/xprivacy.db</code></p>

<h3>From a Terminal Emulator within Android:</h3>

<p><code>su</code></p>
</p><code>sqlite3 /data/system/xprivacy/xprivacy.db</code></p>


<p>*Note: You may need to install sqlite3 binaries</p>

<h2><em>xprivacy.db</em> consists of two relevant TABLES</h2>

<h3>TABLE:restriction</h3>

| FIELD       | TYPE    | NULLABLE |
|-------------|---------|----------|
| uid         | INTEGER | NOTNULL  |
| restriction | TEXT    | NOTNULL  |
| restricted  | INTEGER |          |

<p>The restriction table holds information pertaining to the restriction and onDemand settings on a per UID basis.</p>
<p>The 'restriction' field always lists the restriction category (Accounts, Browser, Calendar, etc.).</p>
<p>The method field lists the restriction methods (addOnAccountsUpdateListener, blockingGetAuthToken, getAccounts, etc.)</p>
<p>Entries where the method field is blank always pertain to the restriction category.</p>
<p>If a restriction category is set to block or allow the entire category, the individual methods will not be listed (with the exception of dangerous methods).</p>
<p>The 'restricted' field lists the status of the restrictions settings, the possible values are 0-3.</p>
<p>The meaning of the 'value' field depends on whether the restriction pertains to a category or a method:</p>

<h4>For category:</h4>

| Value | as seen in XPrivay | Meaning               |
|-------|--------------------|-----------------------|
| 0     | [ ] [?]            | not restricted, ask   |
| 1     | [x] [?]            | restricted, ask       |
| 2     | [ ] [ ]            | not restricted, asked |
| 3     | [x] [ ]            | retricted, asked      |

<h4>For method:</h4>

| Value | as seen in XPrivay | Meaning               |
|-------|--------------------|-----------------------|
| 0     | [x] [?]            | restricted, ask       |
| 1     | [ ] [?]            | not restricted, ask   |
| 2     | [x] [ ]            | restricted, asked     |
| 3     | [ ] [ ]            | not restricted, asked |


<p>*NOTE: Although the 'method' field doesn't always contain data, it is still NOT NULL. To query empty entries: <code>WHERE method=''</code></p>

<h3>TABLE:setting</h3>

<p>The setting table holds information pertaining to settings on a global (uid='0') and per UID basis, as well as the white/blacklists.</p>

| Field | Type | NULLABLE |
|-------|------|----------|
| name  | TEXT | NOTNULL  |
| value | TEXT |          |
| type  | TEXT |          |

<p>*Note: <code>WHERE name='state' and value='0'</code> means ‘restrictions need attention’ (orange), <code>WHERE name='state' and value='1'</code> means ‘restrictions are changed’ (grey), <code>WHERE name='state' and value='2'</code> means ‘restrictions are submitted’ (green)</p>
<p>*Note: <code>WHERE uid='0' and type='template'</code> contains the values in the designated in the restrictions template</p>
<p>*Note: <code>WHERE type IN (Command, Filename, IPAddress, Library, Proc, Url)</code> pertain to the white/blacklist entries</p>

<h2><em>usage.db</em> consists of one relevent table</h2>

<h3>TABLE:usage</h3>

<p>The usage database holds information regarding used restrictions by apps.</p>
<p>The 'restriction' field lists the restriction category (Accounts, Browser, Calendar, etc.).</p>
<p>The 'method' field lists the restriction methods (addOnAccountsUpdateListener, blockingGetAuthToken, getAccounts, etc.)</p>
<p>The 'extra' field holds the parameter information (when applicable)</p>
<p>The 'restricted' field indicates whether the restriction was allowed '0' or denied '1'</p>
<p>The 'time' field holds a UNIX timestamp indicating when the restriction was last accessed.</p>

| Field       | Type    | NULLABLE |
|-------------|---------|----------|
| uid         | INTEGER | NOTNULL  |
| restriction | TEXT    | NOTNULL  |
| method      | TEXT    | NOTNULL  |
| extra       | TEXT    | NOTNULL  |
| restricted  | INTEGER | NOTNULL  |
| time        | INTEGER | NOTNULL  |

<p>NOTE: Although the 'extra' field doesn't always contain data, it is still NOT NULL. To query empty entries: <code>WHERE extra=''</code></p>

<h2>QUERY EXAMPLES:</h2>

<h3><em>xprivacy.db</em></h3>

<code>SELECT * FROM setting WHERE uid='0';</code>

<p>//This will show all global XPrivacy settings (including those not visible within the app)</p>

<code>SELECT * FROM restriction WHERE uid='1000';</code>

<p>//This will show all restriction settings for UID 1000</p>

<code>SELECT * from setting WHERE name='OnDemand' and value='false';</code>

<p>//This will list all apps where onDemand is not active</p>

<code>SELECT * from restriction WHERE restriction='internet' and method='' and restricted='2' ORDER BY uid;</code>

<p>//This will list all apps that have unrestricted access to the Internet category, ordered by UID</p>

<code>SELECT * FROM restriction WHERE method='inet' and restricted='3';</code>

<p>//This will list all apps that have unrestricted Internet/Inet access</p>

<code>UPDATE setting SET value='true' where name='OnDemand' and uid IN (10001,10002,10003,);</code>

<p>//This will enable onDemand for apps listed in the IN ()</p>

<code>UPDATE restriction SET restricted='0' WHERE uid IN (10001,10002,10003) and method='connect';</code>

<p>//This will turn on onDemand for Internet/Connect with a time out default to deny for apps listed in the IN ()</p>

<code>UPDATE restriction SET restricted='3' WHERE uid IN (10001,10002,10003) and method='open';</code>

<p>//This will allow unrestricted access to Storage/Open for the listed apps</p>

<h3><em>usage.db</em></h3>

<code>SELECT * FROM usage ORDER BY time DESC;</code>

<p>//This will show all usage data ordered by TIME (newest entries first)</p>

<code>DELETE FROM usage where uid='1000';</code>

<p>//This will delete all usage entries for UID 1000</p>

<code>DELETE FROM usage;</code>

<p>//THIS WILL DELETE ALL USAGE DATA</p>

WARNING: MODIFYING THE DATABASES DIRECTLY MAY CAUSE ISSUES (INCLUDING BOOT LOOPS) WITH YOUR DEVICE. PROCEED AT YOUR OWN RISK AND ALWAYS PERFORM A BACKUP BEFORE CHANGING ANYTHING!

XPrivacy utilizes 2 databases (xprivacy.db and usage.db), both are located in /data/system/xprivacy.
Making a file backup of the database cannot safely be done in a running system and should be done from recovery!
XPrivacy checks both the xprivacy database and usage database at system boot for integrity (using 'PRAGMA integrity_check'). 
If a database is found to be corrupt, the database is deleted, because repairing an sqlite database is mostly not possible (and Android doesn't have the tools for it installed).
This can happen to the database of any application, but for XPrivacy it is of course a greater concern.
Given the support info I receive, this fortunately happens rarely to the xprivacy database, but more to the usage database. 
The cause for this difference is that usage database is set to asynchronous mode for speed reasons (using 'PRAGMA synchronous=OFF').
The usage database is just an aid and not critical for the operation of XPrivacy. Both the xprivacy and usage database are compacted at boot (using 'VACUUM').
This saves space and is good for performance, but the disadvantage is that twice the size of the database on disk space is needed. 
A full disk (/data/system is mounted on internal memory) is fatal for XPrivacy, because the database will become corrupt in this situation. Again looking at the support info, this also rarely happens. 
All mentioned sqlite commands are properly documented on the [SQLite website](http://www.sqlite.org/)

Accessing the databases:
From a PC:

adb shell
sqlite3 /data/system/xprivacy/xprivacy.db

From a Terminal Emulator within Android:

su
sqlite3 /data/system/xprivacy/xprivacy.db

*Note: You may need to install sqlite3 binaries

xprivacy.db consists of 2 relevant TABLES

TABLE:restriction

uid         INTEGER   	NOTNULL
restriction TEXT      	NOTNULL
method      TEXT
restricted  INTEGER   	NOTNULL

The restriction table holds information pertaining to the restriction and onDemand settings on a per UID basis.
The 'restriction' field always lists the restriction category (Accounts, Browser, Calendar, etc.).
The method field lists the restriction methods (addOnAccountsUpdateListener, blockingGetAuthToken, getAccounts, etc.) Entries where the method field is blank always pertain to the restriction category.
If a restriction category is set to block or allow the entire category, the individual methods will not be listed (with the exception of dangerous methods).
The 'restricted' field lists the status of the restrictions settings, the possible values are 0-3.
The meaning of the 'value' field depends on whether the restriction pertains to a category or a method:

For category:

0	[   ][ ? ]	(not restricted, ask)
1	[ x ][ ? ]	(restricted, ask)
2	[   ][   ]	(not restricted, asked)
3	[ x ][   ]	(restricted, asked)

For method:

0	[ x ][ ? ]	(restricted, ask)
1	[   ][ ? ]	(not restricted, ask)
2	[ x ][   ]	(restricted, asked)
3	[   ][   ]	(not restricted, asked)

*NOTE: Although the 'method' field doesn't always contain data, it is still NOT NULL. To query empty entries: WHERE method=''

TABLE:setting

The setting table holds information pertaining to settings on a global (uid='0') and per UID basis as well as the white/blacklists. 

uid         INTEGER		NOTNULL
name        TEXT		NOTNULL
value       TEXT
type        TEXT

*Note: For field:name='state' value of 0 means ‘restrictions need attention’ (orange), 1 means ‘restrictions are changed’ (grey), 2 means ‘restrictions are submitted’ (green)
*Note: WHERE uid='0' and type='template' contains the values in the designated in the restrictions template
*Note: WHERE type IN (Command, Filename, IPAddress, Library, Proc, Url) pertain to the white/blacklist entries

usage.db

TABLE:usage


The usage database holds information regarding used restrictions by apps. 
The 'restriction' field lists the restriction category (Accounts, Browser, Calendar, etc.).
The 'method' field lists the restriction methods (addOnAccountsUpdateListener, blockingGetAuthToken, getAccounts, etc.)
The 'extra' field holds the parameter information (when applicable)
The 'restricted' field indicates whether the restriction was allowed '0' or denied '1'
The 'time' field holds a UNIX timestamp indicating when the restriction was last accessed.


uid		INTEGER		NOTNULL
restriction	TEXT		NOTNULL
method		TEXT		NOTNULL
extra		TEXT		NOTNULL
restricted	INTEGER		NOTNULL
time		INTEGER		NOTNULL

NOTE: Although the 'extra' field doesn't always contain data, it is still NOT NULL. To query empty entries: WHERE extra=''

QUERY EXAMPLES:

xprivacy.db

SELECT * FROM setting WHERE uid='0';
//This will show all global XPrivacy settings (including those not visible within the app)

SELECT * FROM restriction WHERE uid='1000';
//This will show all restriction settings for UID 1000

SELECT * from setting WHERE name='OnDemand' and value='false';
//This will list all apps where onDemand is not active

SELECT * from restriction WHERE restriction='internet' and method='' and restricted='2' ORDER BY uid;
//This will list all apps that have unrestricted access to the Internet category, ordered by UID

SELECT * FROM restriction WHERE method='inet' and restricted='3';
//This will list all apps that have unrestricted Internet/Inet access

UPDATE setting SET value='true' where name='OnDemand' and uid IN (10001,10002,10003,);
//This will enable onDemand for apps listed in the IN ()

UPDATE restriction SET restricted='0' WHERE uid IN (10001,10002,10003) and method='connect';
//This will turn on onDemand for Internet/Connect with a time out default to deny for apps listed in the IN ()

UPDATE restriction SET restricted='3' WHERE uid IN (10001,10002,10003) and method='open';
//This will allow unrestricted access to Storage/Open for the listed apps

usage.db

SELECT * FROM usage ORDER BY time DESC;
//This will show all usage data ordered by TIME (newest entries first)

DELETE FROM usage where uid='1000';
//This will delete all usage entries for UID 1000

DELETE FROM usage;
//THIS WILL DELETE ALL USAGE DATA

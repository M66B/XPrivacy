#!/bin/bash
grep -RIl "\<string name=\"menu_tutorial" . | xargs sed -i -e '/string name="menu_tutorial/a \
    <string name="menu_exclude">Exclude</string>
    <string name="menu_close">Close</string>
    <string name="menu_choose">Choose</string>
    <string name="msg_abort">Aborting</string>
    <string name="msg_export">Backup all settings to %s</string>
    <string name="msg_import">Import settings from %s</string>
\ \ \ \ <string name="menu_exclude">Exclude</string>\
\ \ \ \ <string name="menu_close">Close</string>\
\ \ \ \ <string name="menu_choose">Choose</string>\
\ \ \ \ <string name="msg_abort">Aborting</string>\
\ \ \ \ <string name="msg_export">Backup all settings to %s</string>\
\ \ \ \ <string name="msg_import">Import settings from %s</string>'

#grep -RIl "\<string name=\"settings_usage" . | xargs sed -i -e '/settings_usage/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

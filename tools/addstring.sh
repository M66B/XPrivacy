#!/bin/bash
grep -RIl "\<string name=\"msg_restart" . | xargs sed -i -e '/string name="msg_restart/a \
\ \ \ \ <string name="msg_upgrading" formatted="false">Upgrading %s</string>'
grep -RIl "\<string name=\"msg_restart" . | xargs sed -i -e '/string name="msg_restart/a \
\ \ \ \ <string name="msg_randomizing" formatted="false">Randomizing %s</string>'
grep -RIl "\<string name=\"msg_restart" . | xargs sed -i -e '/string name="msg_restart/a \
\ \ \ \ <string name="msg_migrating" formatted="false">Migrating %s</string>'
grep -RIl "\<string name=\"msg_restart" . | xargs sed -i -e '/string name="msg_restart/a \
\ \ \ \ <string name="msg_registered">Check your e-mail to activate your device</string>'
grep -RIl "\<string name=\"msg_restart" . | xargs sed -i -e '/string name="msg_restart/a \
\ \ \ \ <string name="msg_register">Please register your device</string>'

#grep -RIl "\<string name=\"settings_usage" . | xargs sed -i -e '/settings_usage/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

#!/bin/bash
grep -RIl "\<string name=\"msg_upgrading" . | xargs sed -i -e '/string name="msg_upgrading/a \
\ \ \ \ <string name="msg_migrated">Migration complete</string>'

#grep -RIl "\<string name=\"settings_usage" . | xargs sed -i -e '/settings_usage/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

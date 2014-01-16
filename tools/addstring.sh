#!/bin/bash
grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e '/string name="settings_experimental/a \
\ \ \ \ <string name="settings_https">Use secure connections</string>'

#grep -RIl "\<string name=\"msg_usage" . | xargs sed -i -e '/msg_usage/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

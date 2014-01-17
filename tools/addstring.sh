#!/bin/bash
grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e '/string name="settings_experimental/a \
\ \ \ \ <string name="settings_https">Use secure connections</string>'

#grep -RIl "\<string name=\"settings_usage" . | xargs sed -i -e '/settings_usage/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

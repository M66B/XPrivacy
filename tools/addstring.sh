#!/bin/bash
grep -RIl "\<string name=\"app_version" . | xargs sed -i -e '/string name="app_version/a \
\ \ \ \ <string name="app_description">The ultimate privacy manager</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

#!/bin/bash
grep -RIl "\<string name=\"help_used" . | xargs sed -i -e '/string name="help_used/a \
\ \ \ \ <string name="help_used_grayed">no usage data available</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

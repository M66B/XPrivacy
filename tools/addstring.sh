#!/bin/bash
grep -RIl "\<string name=\"settings_https" res | xargs sed -i -e '/string name="settings_https/a \
\ \ \ \ <string name="settings_syscomponents">Restrict system components (Android)</string>'

#grep -RIl "\<string name=\"msg_migrated" res | xargs sed -i -e '/msg_migrated/d'
#grep -RIl "\<string name=\"settings_experimental" res | xargs sed -i -e 's/Experimental features/Experimental functions/g'

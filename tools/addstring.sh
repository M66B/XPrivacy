#!/bin/bash
grep -RIl "\<string name=\"msg_registered" res | xargs sed -i -e '/string name="msg_registered/a \
\ \ \ \ <string name="msg_service">Update service running</string>'

#grep -RIl "\<string name=\"msg_migrated" res | xargs sed -i -e '/msg_migrated/d'
#grep -RIl "\<string name=\"settings_experimental" res | xargs sed -i -e 's/Experimental features/Experimental functions/g'

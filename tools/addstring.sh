#!/bin/bash
grep -RIl "\<string name=\"msg_migrated" res | xargs sed -i -e '/string name="msg_migrated/a \
\ \ \ \ <string name="msg_support_info">An internal check failed, do you want to send the support info?</string>'

#grep -RIl "\<string name=\"menu_choose" res | xargs sed -i -e '/menu_choose/d'
#grep -RIl "\<string name=\"settings_experimental" res | xargs sed -i -e 's/Experimental features/Experimental functions/g'

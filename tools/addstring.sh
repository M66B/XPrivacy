#!/bin/bash
grep -RIl "\<string name=\"msg_registered" res | xargs sed -i -e '/string name="msg_registered/a \
\ \ \ \ <string name="msg_service">Update service running</string>'

#grep -RIl "\<string name=\"menu_choose" res | xargs sed -i -e '/menu_choose/d'
#grep -RIl "\<string name=\"settings_experimental" res | xargs sed -i -e 's/Experimental features/Experimental functions/g'

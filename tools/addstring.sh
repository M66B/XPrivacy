#!/bin/bash
grep -RIl "\<string name=\"msg_no_restrictions" res | xargs sed -i -e '/string name="msg_no_restrictions/a \
\ \ \ \ <string name="msg_technical_error">Technical error</string>'

#grep -RIl "\<string name=\"settings_usage" res | xargs sed -i -e '/settings_usage/d'
#grep -RIl "\<string name=\"settings_experimental" res | xargs sed -i -e 's/Experimental features/Experimental functions/g'

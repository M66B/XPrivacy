#!/bin/bash
grep -RIl "\<string name=\"msg_abort" res | xargs sed -i -e '/string name="msg_abort/a \
\ \ \ \ <string name="msg_aborted">Aborted</string>'

#grep -RIl "\<string name=\"msg_import" res | xargs sed -i -e '/msg_import/d'
#grep -RIl "\<string name=\"settings_experimental" res | xargs sed -i -e 's/Experimental features/Experimental functions/g'

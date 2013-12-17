#!/bin/bash
grep -RIl "\<string name=\"menu_app_store" . | xargs sed -i -e '/string name="menu_app_store/a \
\ \ \ \ <string name="menu_app_kill">Kill</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

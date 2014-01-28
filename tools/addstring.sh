#!/bin/bash
grep -RIl "\<string name=\"app_wrongandroid" res | xargs sed -i -e '/string name="app_wrongandroid/a \
\ \ \ \ <string name="app_incompatible" formatted="false">XPrivacy is incompatible with %s</string>'

#grep -RIl "\<string name=\"settings_global" res | xargs sed -i -e '/settings_global/d'
#grep -RIl "\<string name=\"settings_experimental" res | xargs sed -i -e 's/Experimental features/Experimental functions/g'

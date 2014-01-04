#!/bin/bash
grep -RIl "\<string name=\"tutorial_detailslist" . | xargs sed -i -e '/string name="tutorial_detailslist/a \
\ \ \ \ <string name="permission_manage">Manage applications, including killing</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

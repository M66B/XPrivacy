#!/bin/bash
grep -RIl "\<string name=\"menu_restrict_all" . | xargs sed -i -e '/string name="menu_restrict_all/a \
\ \ \ \ <string name="menu_clear_usage">Clear</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'

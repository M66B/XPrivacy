#!/bin/bash
grep -RIl "\<string name=\"tutorial_detailslist" . | xargs sed -i -e '/string name="menu_help/a \
\ \ \ \ <string name="menu_select_all">Select all</string>'

grep -RIl "\<string name=\"tutorial_detailslist" . | xargs sed -i -e '/string name="msg_sure/a \
\ \ \ \ <string name="msg_select">You must select some applications first</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"settings_experimental" . | xargs sed -i -e 's/Experimental features/Experimental functions/g'

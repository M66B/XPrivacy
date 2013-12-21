#!/bin/bash
#grep -RIl "\<string name=\"menu_app_store" . | xargs sed -i -e '/string name="title_fnot/a \
#\ \ \ \ <string name="title_sort_by">Sort by</string>\'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'
grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/By name/name/g'
grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/By uid/uid/g'
grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/Date installed/date installed/g'
grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/Date last updated/date updated/g'

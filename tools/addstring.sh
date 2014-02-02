#!/bin/bash
grep -RIl "\<string name=\"menu_close" res | xargs sed -i -e '/menu_close/a \
\ \ \ \ <string name="menu_category">Act on the entire category</string>'

#grep -RIl "\<string name=\"menu_clear_ondemand" res | xargs sed -i -e '/menu_clear_ondemand/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

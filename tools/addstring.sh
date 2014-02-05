#!/bin/bash
grep -RIl "\<string name=\"menu_sort" res | xargs sed -i -e '/menu_sort/a \
\ \ \ \ <string name="menu_filter">Filter</string>'

#grep -RIl "\<string name=\"menu_clear_ondemand" res | xargs sed -i -e '/menu_clear_ondemand/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

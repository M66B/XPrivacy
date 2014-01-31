#!/bin/bash
grep -RIl "\<string name=\"menu_clear_usage" res | xargs sed -i -e '/menu_clear_usage/a \
\ \ \ \ <string name="menu_clear_db">Clear all data</string>'

#grep -RIl "\<string name=\"msg_ondemand" res | xargs sed -i -e '/msg_ondemand/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

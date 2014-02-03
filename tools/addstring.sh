#!/bin/bash
grep -RIl "\<string name=\"settings_lon" res | xargs sed -i -e '/settings_lon/a \
\ \ \ \ <string name="settings_alt">Altitude</string>'

#grep -RIl "\<string name=\"menu_clear_ondemand" res | xargs sed -i -e '/menu_clear_ondemand/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

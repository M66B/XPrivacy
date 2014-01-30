#!/bin/bash
grep -RIl "\<string name=\"menu_clear_all" res | xargs sed -i -e '/menu_clear_all/a \
\ \ \ \ <string name="menu_clear_ondemand">Clear on demand choices</string>'

#grep -RIl "\<string name=\"msg_edit" res | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/title_sattention">Apps requiring attention first/title_sinvert">Invert sort order/g'

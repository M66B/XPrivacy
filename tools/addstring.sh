#!/bin/bash
grep -RIl "\<string name=\"msg_ondemand" res | xargs sed -i -e '/msg_ondemand/a \
\ \ \ \ <string name="msg_restrictedby">Restricted by XPrivacy</string>'

#grep -RIl "\<string name=\"msg_edit" res | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/title_sattention">Apps requiring attention first/title_sinvert">Invert sort order/g'

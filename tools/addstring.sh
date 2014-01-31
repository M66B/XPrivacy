#!/bin/bash
grep -RIl "\<string name=\"msg_support_info" res | xargs sed -i -e '/msg_support_info/a \
\ \ \ \ <string name="msg_ondemand" formatted="false">%1$s is trying to use %$3s in the %2$s category</string>'

#grep -RIl "\<string name=\"msg_ondemand" res | xargs sed -i -e '/msg_ondemand/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/title_sattention">Apps requiring attention first/title_sinvert">Invert sort order/g'

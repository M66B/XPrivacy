#!/bin/bash
grep -RIl "\<string name=\"title_deny" res | xargs sed -i -e '/title_deny/a \
\ \ \ \ <string name="title_denyonce">Deny once</string>'

#grep -RIl "\<string name=\"msg_edit" res | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/title_sattention">Apps requiring attention first/title_sinvert">Invert sort order/g'

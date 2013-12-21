#!/bin/bash
grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e '/item >date updated/a \
\ \ \ \ \ \ \ \ <item >date modified</item>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'
grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/title_sattention">Apps requiring attention first/title_sinvert">Invert sort order/g'

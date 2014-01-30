#!/bin/bash
grep -RIl "\<string name=\"msg_support_info" res | xargs sed -i -e '/msg_support_info/a \
\ \ \ \ <string name="msg_ondemand" formatted="false">%1$s is trying to use a function in the %2$s category, restrict this category?</string>'

grep -RIl "\<string name=\"title_sinvert" res | xargs sed -i -e '/title_sinvert/a \
\ \ \ \ <string name="title_allow">Allow</string>\
    <string name="title_deny">Deny</string>'

grep -RIl "\<string name=\"settings_notify" res | xargs sed -i -e '/settings_notify/a \
\ \ \ \ <string name="settings_ondemand">Restricting on demand</string>'

#grep -RIl "\<string name=\"title_smodified" res | xargs sed -i -e '/By date modified/By date modified (XPrivacy)/g'
#grep -RIl "\<string name=\"msg_edit" res | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/title_sattention">Apps requiring attention first/title_sinvert">Invert sort order/g'

#!/bin/bash
grep -RIl "\<string name=\"menu_select_all" res | xargs sed -i -e '/menu_select_all/a \
\ \ \ \ <string name="menu_sort">Sort</string>'

grep -RIl "\<string name=\"title_fnot" res | xargs sed -i -e '/title_fnot/a \
\ \ \ \ <string name="title_sname">By name</string>\
    <string name="title_suid">By uid</string>\
    <string name="title_sinstalled">By date installed</string>\
    <string name="title_supdated">By date updated</string>\
    <string name="title_smodified">By date modified</string>\
    <string name="title_sinvert">Invert sort order</string>'

#grep -RIl "\<string name=\"title_smodified" res | xargs sed -i -e '/By date modified/By date modified (XPrivacy)/g'
#grep -RIl "\<string name=\"msg_edit" res | xargs sed -i -e '/msg_edit/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/title_sattention">Apps requiring attention first/title_sinvert">Invert sort order/g'

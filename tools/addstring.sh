#!/bin/bash
grep -RIl "\<string name=\"menu_close" res | xargs sed -i -e "/menu_close/a \
\ \ \ \ <string name=\"menu_flush\">Flush cache</string>"

#grep -RIl "\<string name=\"menu_apply" res | xargs sed -i -e '/menu_apply/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

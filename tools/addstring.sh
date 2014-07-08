#!/bin/bash
grep -RIl "\<string name=\"whitelist_permission" res | xargs sed -i -e '/whitelist_permission/d'
grep -RIl "\<string name=\"whitelist_transaction" res | xargs sed -i -e "/whitelist_transaction/a \
\ \ \ \ <string name=\"whitelist_permission\">Permissions</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

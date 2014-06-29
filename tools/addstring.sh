#!/bin/bash
grep -RIl "\<string name=\"whitelist_address" res | xargs sed -i -e '/whitelist_address/d'
grep -RIl "\<string name=\"restrict_view" res | xargs sed -i -e "/restrict_view/a \
\ \ \ \ <string name=\"whitelist_address\">Addresses</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

#!/bin/bash
grep -RIl "\<string name=\"whitelist_library" res | xargs sed -i -e "/whitelist_library/a \
\ \ \ \ <string name=\"whitelist_method\">Method names</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

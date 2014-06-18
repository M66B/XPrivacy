#!/bin/bash
grep -RIl "\<string name=\"title_deny" res | xargs sed -i -e "/title_deny/a \
\ \ \ \ <string name=\"title_dontknow\">Don\\\'t know</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

#!/bin/bash
grep -RIl "\<string name=\"title_ondemand_reset" res | xargs sed -i -e '/title_ondemand_reset/d'
grep -RIl "\<string name=\"title_ondemand_default" res | xargs sed -i -e "/title_ondemand_default/a \
\ \ \ \ <string name=\"title_ondemand_reset\">Reset</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

#!/bin/bash
grep -RIl "\<string name=\"title_set_restrict" res | xargs sed -i -e "/title_set_restrict/a \
\ \ \ \ <string name=\"title_enable_ondemand\">Enable on demand restricting</string>"

#grep -RIl "\<string name=\"title_categories" res | xargs sed -i -e '/title_categories/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

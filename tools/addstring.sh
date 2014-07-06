#!/bin/bash
grep -RIl "\<string name=\"title_function_help" res | xargs sed -i -e '/title_function_help/d'
grep -RIl "\<string name=\"title_ondemand_category" res | xargs sed -i -e "/title_ondemand_category/a \
\ \ \ \ <string name=\"title_function_help\">Function documentation</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

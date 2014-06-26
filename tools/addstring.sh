#!/bin/bash
grep -RIl "\<string name=\"title_ondemand_category" res | xargs sed -i -e '/title_ondemand_category/d'
grep -RIl "\<string name=\"title_toggle_dangerous" res | xargs sed -i -e "/title_toggle_dangerous/a \
\ \ \ \ <string name=\"title_ondemand_category\">Applying to the category will reset the function exceptions to default values</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

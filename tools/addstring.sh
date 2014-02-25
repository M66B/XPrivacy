#!/bin/bash
grep -RIl "\<string name=\"settings_usagedata" res | xargs sed -i -e "/settings_usagedata/a \
\ \ \ \ <string name=\"settings_parameters\">Show parameters of usage data</string>"

#grep -RIl "\<string name=\"title_categories" res | xargs sed -i -e '/title_categories/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

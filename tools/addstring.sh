#!/bin/bash
grep -RIl "\<string name=\"restrict_accounts" res | xargs sed -i -e "/restrict_accounts/a \
\ \ \ \ <string name=\"restrict_analytics\">Analytics</string>"

#grep -RIl "\<string name=\"menu_apply" res | xargs sed -i -e '/menu_apply/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

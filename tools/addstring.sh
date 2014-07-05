#!/bin/bash
grep -RIl "\<string name=\"whitelist_transaction" res | xargs sed -i -e '/whitelist_transaction/d'
grep -RIl "\<string name=\"whitelist_url" res | xargs sed -i -e "/whitelist_url/a \
\ \ \ \ <string name=\"whitelist_transaction\">Transactions</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

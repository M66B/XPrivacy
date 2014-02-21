#!/bin/bash
#grep -RIl "\<string name=\"msg_no_restrictions" res | xargs sed -i -e "/msg_no_restrictions/a \
#\ \ \ \ <string name=\"whitelist_proc\">/proc</string>"

grep -RIl "\<string name=\"title_categories" res | xargs sed -i -e '/title_categories/d'
#grep -RIl "\<string name=\"title_whitelist" res | xargs sed -i -e 's/">Whitelist /">Remember /g'

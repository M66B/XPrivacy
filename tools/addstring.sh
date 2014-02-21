#!/bin/bash
#grep -RIl "\<string name=\"msg_no_restrictions" res | xargs sed -i -e "/msg_no_restrictions/a \
#\ \ \ \ <string name=\"msg_no_whitelists\">No whitelists found for this application</string>"

grep -RIl "\<string name=\"msg_no_whitelists" res | xargs sed -i -e '/msg_no_whitelists/d'
#grep -RIl "\<string name=\"title_whitelist" res | xargs sed -i -e 's/">Whitelist /">Remember /g'

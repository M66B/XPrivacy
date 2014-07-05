#!/bin/bash
grep -RIl "\<string name=\"settings_blacklist" res | xargs sed -i -e '/settings_blacklist/d'
grep -RIl "\<string name=\"settings_ondemand" res | xargs sed -i -e "/settings_ondemand/a \
\ \ \ \ <string name=\"settings_blacklist\">Blacklist accounts, applications and contacts</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

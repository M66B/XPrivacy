#!/bin/bash
grep -RIl "\<string name=\"title_susage" res | xargs sed -i -e '/title_susage/d'
#grep -RIl "\<string name=\"title_sstate" res | xargs sed -i -e "/title_sstate/a \
#\ \ \ \ <string name=\"title_susage\">By last usage (XPrivacy)</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

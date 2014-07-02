#!/bin/bash
grep -RIl "\<string name=\"title_sstate" res | xargs sed -i -e '/title_sstate/d'
grep -RIl "\<string name=\"title_smodified" res | xargs sed -i -e "/title_smodified/a \
\ \ \ \ <string name=\"title_sstate\">By state (XPrivacy)</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

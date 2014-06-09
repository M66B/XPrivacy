#!/bin/bash
grep -RIl "\<string name=\"title_template_full" res | xargs sed -i -e "/title_template_full/a \
\ \ \ \ <string name=\"title_template_merge\">Apply template (merge)</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

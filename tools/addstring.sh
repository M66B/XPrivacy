#!/bin/bash
grep -RIl "\<string name=\"title_template_full" res | xargs sed -i -e "/title_template_full/a \
\ \ \ \ <string name=\"title_template_merge\">Apply template (merge)</string>"

#grep -RIl "\<string name=\"menu_apply" res | xargs sed -i -e '/menu_apply/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

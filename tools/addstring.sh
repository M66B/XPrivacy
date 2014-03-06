#!/bin/bash
grep -RIl "\<string name=\"title_set_restrict" res | xargs sed -i -e "/title_set_restrict/a \
\ \ \ \ <string name=\"title_template_full\">Apply template (categories+functions)</string>"
grep -RIl "\<string name=\"title_set_restrict" res | xargs sed -i -e "/title_set_restrict/a \
\ \ \ \ <string name=\"title_template_category\">Apply template (categories)</string>"

#grep -RIl "\<string name=\"menu_apply" res | xargs sed -i -e '/menu_apply/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

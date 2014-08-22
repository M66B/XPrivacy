#!/bin/bash
grep -RIl "\<string name=\"title_template_merge_reset" res | xargs sed -i -e '/title_template_merge_reset/d'

grep -RIl "\<string name=\"title_template_merge" res | xargs sed -i -e '/title_template_merge/a \
\ \ \ \ <string name=\"title_template_merge_reset\">Apply template (merge reset)</string>'

grep -RIl "\<string name=\"title_template_merge" res | xargs sed -i -e 's/Apply template (merge)/Apply template (merge set)/g'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'
#grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e 's/requires restart/requires reboot/g'

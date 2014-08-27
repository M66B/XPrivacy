#!/bin/bash
grep -RIl "\<string name=\"menu_legend" res | xargs sed -i -e '/menu_legend/d'

grep -RIl "\<string name=\"menu_batch" res | xargs sed -i -e '/menu_batch/a \
\ \ \ \ <string name=\"menu_legend\">Legend</string>'

#grep -RIl "\<string name=\"title_template_merge" res | xargs sed -i -e 's/Apply template (merge)/Apply template (merge set)/g'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'
#grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e 's/requires restart/requires reboot/g'

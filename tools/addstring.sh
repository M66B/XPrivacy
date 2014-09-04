#!/bin/bash
grep -RIl "\<string name=\"msg_method_expert" res | xargs sed -i -e '/msg_method_expert/d'

grep -RIl "\<string name=\"msg_expert" res | xargs sed -i -e '/msg_expert/a \
\ \ \ \ <string name=\"msg_method_expert\">Function restrictions are meant for expert users. Are you an expert user?</string>'

#grep -RIl "\<string name=\"title_template_merge" res | xargs sed -i -e 's/Apply template (merge)/Apply template (merge set)/g'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'
#grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e 's/requires restart/requires reboot/g'

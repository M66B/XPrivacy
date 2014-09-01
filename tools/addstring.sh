#!/bin/bash
grep -RIl "\<string name=\"msg_expert" res | xargs sed -i -e '/msg_expert/d'

grep -RIl "\<string name=\"msg_service_updating" res | xargs sed -i -e '/msg_service_updating/a \
\ \ \ \ <string name=\"msg_expert\">Expert users are supposed to solve their own problems where possible and to follow the XDA XPrivacy forum and help others where possible</string>'

#grep -RIl "\<string name=\"title_template_merge" res | xargs sed -i -e 's/Apply template (merge)/Apply template (merge set)/g'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'
#grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e 's/requires restart/requires reboot/g'

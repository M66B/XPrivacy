#!/bin/bash
grep -RIl "\<string name=\"msg_settings_specific" res | xargs sed -i -e '/msg_settings_specific/d'

grep -RIl "\<string name=\"msg_longpress_whitelist" res | xargs sed -i -e '/msg_longpress_whitelist/a \
\ \ \ \ <string name=\"msg_settings_specific\">Check marks and values take precedence over global check marks and values; empty check boxes and values do not take precedence</string>'

#grep -RIl "\<string name=\"title_template_merge" res | xargs sed -i -e 's/Apply template (merge)/Apply template (merge set)/g'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'
#grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e 's/requires restart/requires reboot/g'

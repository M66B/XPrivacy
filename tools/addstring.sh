#!/bin/bash
grep -RIl "\<string name=\"help_check_restrict" res | xargs sed -i -e '/help_check_restrict/d'
grep -RIl "\<string name=\"help_check_ondemand" res | xargs sed -i -e '/help_check_ondemand/d'
grep -RIl "\<string name=\"help_switch" res | xargs sed -i -e '/help_switch/d'

grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e '/settings_aosp/a \
\ \ \ \ <string name=\"help_check_restrict\">First check box: restrict category or function</string>\
\ \ \ \ <string name=\"help_check_ondemand\">Second check box: restrict on demand</string>\
\ \ \ \ <string name=\"help_switch\">On/off switch: enable or disable all restrictions</string>'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'
#grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e 's/requires restart/requires reboot/g'

#!/bin/bash
grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e '/settings_aosp/d'

grep -RIl "\<string name=\"settings_quirks" res | xargs sed -i -e '/settings_quirks/a \
\ \ \ \ <string name=\"settings_aosp\">AOSP mode (requires restart)</string>'

grep -RIl "\<string name=\"settings_aosp" res | xargs sed -i -e 's/requires restart/requires reboot/g'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'

#!/bin/bash
grep -RIl "\<string name=\"title_usage_footer" res | xargs sed -i -e "/title_usage_footer/a \
\ \ \ \ <string name=\"title_toggle_dangerous\">Long press on a title to toggle dangerous functions</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

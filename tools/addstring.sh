#!/bin/bash
grep -RIl "\<string name=\"title_update_checking" res | xargs sed -i -e '/title_update_checking/d'
grep -RIl "\<string name=\"title_update_downloading" res | xargs sed -i -e '/title_update_downloading/d'
grep -RIl "\<string name=\"title_update_install" res | xargs sed -i -e '/title_update_install/d'
grep -RIl "\<string name=\"title_update_none" res | xargs sed -i -e '/title_update_none/d'

grep -RIl "\<string name=\"title_unsafe" res | xargs sed -i -e '/title_unsafe/a \
\ \ \ \ <string name=\"title_update_checking\">Checking for update</string>\
\ \ \ \ <string name=\"title_update_downloading\">Downloading update</string>\
\ \ \ \ <string name=\"title_update_install\">Tap to install update</string>\
\ \ \ \ <string name=\"title_update_none\">No updates available</string>'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'

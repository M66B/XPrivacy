#!/bin/bash
grep -RIl "\<string name=\"title_update_legacy" res | xargs sed -i -e '/title_update_legacy/d'

grep -RIl "\<string name=\"title_update_none" res | xargs sed -i -e '/title_update_none/a \
\ \ \ \ <string name=\"title_update_legacy\">XPrivacy 2 is not supported anymore, please consider upgrading to XPrivacy 3</string>'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'

#!/bin/bash
grep -RIl "\<string name=\"help_settings" res | xargs sed -i -e '/help_settings/d'

grep -RIl "\<string name=\"help_shared" res | xargs sed -i -e '/help_shared/a \
\ \ \ \ <string name=\"help_settings\">has application specific fake values</string>'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'

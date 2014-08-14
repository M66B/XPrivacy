#!/bin/bash
grep -RIl "\<string name=\"title_once" res | xargs sed -i -e '/title_once/d'

grep -RIl "\<string name=\"title_applycat" res | xargs sed -i -e '/title_applycat/a \
\ \ \ \ <string name=\"title_once\">Once for</string>'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'

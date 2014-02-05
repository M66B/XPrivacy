#!/bin/bash
grep -RIl "\<string name=\"help_shared" res | xargs sed -i -e '/help_shared/a \
\ \ \ \ <string name="tutorial_mainheader">Select the category to restrict</string>'

#grep -RIl "\<string name=\"tutorial_mainheader" res | xargs sed -i -e '/tutorial_mainheader/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

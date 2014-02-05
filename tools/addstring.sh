#!/bin/bash
grep -RIl "\<string name=\"settings_usagedata" res | xargs sed -i -e '/settings_usagedata/a \
\ \ \ \ <string name="help_longpress">Long press name to select</string>'

#grep -RIl "\<string name=\"tutorial_mainheader" res | xargs sed -i -e '/tutorial_mainheader/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

#!/bin/bash
grep -RIl "\<string name=\"title_deny" res | xargs sed -i -e '/title_deny/a \
\ \ \ \ <string name="title_pleasesubmit">Please submit your restrictions to help others</string>'

#grep -RIl "\<string name=\"tutorial_mainheader" res | xargs sed -i -e '/tutorial_mainheader/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

#!/bin/bash
grep -RIl "\<string name=\"title_sinvert" res | xargs sed -i -e '/title_sinvert/a \
\ \ \ \ <string name="title_attempt">Usage attempt of:</string>'

#grep -RIl "\<string name=\"msg_ondemand" res | xargs sed -i -e '/msg_ondemand/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

#!/bin/bash
grep -RIl "\<string name=\"msg_register\"" res | xargs sed -i -e '/msg_register"/a \
\ \ \ \ <string name="msg_email">Please enter your e-mail address:</string>'

#grep -RIl "\<string name=\"msg_ondemand" res | xargs sed -i -e '/msg_ondemand/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/%$3s/%3$s/g'

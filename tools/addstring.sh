#!/bin/bash
grep -RIl "\<string name=\"app_copyright" res | xargs sed -i -e '/app_copyright/a \
\ \ \ \ <string name="app_licensed">Licensed, thanks for your support!</string>'

#grep -RIl "\<string name=\"app_licensed" res | xargs sed -i -e '/app_licensed/d'
#grep -RIl "\<string name=\"menu_app_store" res | xargs sed -i -e 's/GSM Location area code/GSM LAC/g'

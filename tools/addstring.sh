#!/bin/bash
#grep -RIl "\<string name=\"menu_contacts" . | xargs sed -i -e '/string name="menu_contacts/a \
#\ \ \ \ <string name="menu_tutorial">Tutorial</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'

grep -RIl "\<string name=\"app_name" res | xargs -L1 sed -i -e 's/Filter visible applications/Choose which apps to see with the filters/'

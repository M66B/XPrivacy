#!/bin/bash
grep -RIl "\<string name=\"menu_contacts" . | xargs sed -i -e '/string name="menu_contacts/a \
\ \ \ \ <string name="menu_tutorial">Tutorial</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'
#!/bin/bash
grep -RIl "\<string name=\"menu_contacts" . | xargs sed -i -e '/string name="menu_contacts/a \
\ \ \ \ <string name="menu_tutorial">Tutorial</string>'
#!/bin/bash
#grep -RIl "\<string name=\"menu_contacts" . | xargs sed -i -e '/string name="menu_contacts/a \
#\ \ \ \ <string name="menu_tutorial">Tutorial</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'

grep -RIl "\<string name=\"help_shared" . | xargs sed -i -e '/string name="help_shared/a \
    <string name="tutorial_mainheader">Select the category to restrict; choose visible apps with the filters</string>\
    <string name="tutorial_mainlist">Tap a check box to restrict the selected category for an app\\n\
\\nApps will be fed empty or fake data for restricted categories\\n\
\\nTap the app icon for detailed restrictions and settings</string>\
    <string name="tutorial_detailsheader">Tap the app icon for more actions</string>\
    <string name="tutorial_detailslist">Tap the down arrow to see the function exceptions of a restriction category</string>'


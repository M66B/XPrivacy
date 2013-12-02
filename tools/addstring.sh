#!/bin/bash
grep -Rl "\<string name=\"help_dangerous" . | xargs sed -i -e '/string name="menu_restriction_all/a \
\ \ \ \ <string name="menu_clear_all">Clear all</string>\
    <string name="menu_restrict_all">Restrict all</string>'


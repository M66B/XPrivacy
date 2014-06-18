#!/bin/bash
grep -RIl "\<string name=\"title_alternate" res | xargs sed -i -e "/title_alternate/a \
\ \ \ \ <string name=\"title_usage_footer\">Use common sense when restricting, don\\\'t expect internet access if you restricted the internet category, etc.</string>"
grep -RIl "\<string name=\"title_alternate" res | xargs sed -i -e "/title_alternate/a \
\ \ \ \ <string name=\"title_usage_text_3\">3. Tap the first check box of any category you want to restrict</string>"
grep -RIl "\<string name=\"title_alternate" res | xargs sed -i -e "/title_alternate/a \
\ \ \ \ <string name=\"title_usage_text_2\">2. Tap on the application icon</string>"
grep -RIl "\<string name=\"title_alternate" res | xargs sed -i -e "/title_alternate/a \
\ \ \ \ <string name=\"title_usage_text_1\">1. Find the application to restrict in the main application list</string>"
grep -RIl "\<string name=\"title_alternate" res | xargs sed -i -e "/title_alternate/a \
\ \ \ \ <string name=\"title_usage_header\">Usage</string>"

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

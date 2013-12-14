#!/bin/bash
grep -RIl "\<string name=\"settings_dangerous" . | xargs sed -i -e '/string name="settings_dangerous/a \
\ \ \ \ <string name="settings_confidence">Maximum fetch confidence interval</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'

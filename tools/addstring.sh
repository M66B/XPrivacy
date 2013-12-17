#!/bin/bash
grep -RIl "\<string name=\"settings_confidence" . | xargs sed -i -e '/string name="settings_confidence/a \
\ \ \ \ <string name="settings_experimental">Experimental features</string>'

#grep -RIl "\<string name=\"msg_edit" . | xargs sed -i -e '/msg_edit/d'

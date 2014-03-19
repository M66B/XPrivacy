#!/bin/bash
grep -RIl "\<string name=\"msg_restrictedby" res | xargs sed -i -e "/msg_restrictedby/a \
\ \ \ \ <string name=\"msg_service_version\">Privacy service version mismatch; did you reboot your device after upgrading XPrivacy?</string>"
grep -RIl "\<string name=\"msg_restrictedby" res | xargs sed -i -e "/msg_restrictedby/a \
\ \ \ \ <string name=\"msg_service_missing\">Privacy service not found; did you enable XPrivacy in the Xposed installer and reboot your device?</string>"

#grep -RIl "\<string name=\"menu_apply" res | xargs sed -i -e '/menu_apply/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

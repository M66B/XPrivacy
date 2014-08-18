#!/bin/bash
grep -RIl "\<string name=\"msg_service_updating" res | xargs sed -i -e '/msg_service_updating/d'

grep -RIl "\<string name=\"msg_service_version" res | xargs sed -i -e '/msg_service_version/a \
\ \ \ \ <string name=\"msg_service_updating\">Please wait until the update service has been completed</string>'

#grep -RIl "\<string name=\"app_notxposed" res | xargs sed -i -e '/app_notxposed/d'
#grep -RIl "\<string name=\"restrict_help_internet" res | xargs sed -i -e 's/internet</Internet</g'

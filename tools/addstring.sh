#!/bin/bash
grep -RIl "\<string name=\"restrict_notifications" . | xargs sed -i -e '/string name="restrict_notifications/a \
\ \ \ \ <string name="restrict_overlay">Overlay</string>'
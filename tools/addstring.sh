#!/bin/bash
grep -RIl "\<string name=\"msg_applying" . | xargs sed -i -e '/string name="msg_applying/a \
\ \ \ \ <string name="msg_applied">Template applied</string>'
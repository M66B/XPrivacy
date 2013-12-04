#!/bin/bash
grep -RIl "\<string name=\"msg_usage" . | xargs sed -i -e '/string name="msg_usage/a \
\ \ \ \ <string name="msg_restart">Application restart required</string>'
#!/bin/bash
grep -Rl "\<string name=\"help_dangerous" . | xargs sed -i -e '/string name="msg_loading/a \
    <string name="msg_filtering">Applying filters</string>'

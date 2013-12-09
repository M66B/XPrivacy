#!/bin/bash
grep -RIl "\<string name=\"help_half" . | xargs sed -i -e '/string name="help_half/a \
\ \ \ \ <string name="help_attention">restrictions need attention</string>'
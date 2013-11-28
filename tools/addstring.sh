#!/bin/bash
grep -Rl "\<string name=\"help_dangerous" | xargs sed -i "/string name=\"help_dangerous/a \
\ \ \ \ <string name=\"help_half\">partly restricted</string>"

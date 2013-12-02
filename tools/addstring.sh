#!/bin/bash
<<<<<<< HEAD
grep -Rl "\<string name=\"help_dangerous" . | xargs sed -i -e '/string name="msg_random_boot/a \
\ \ \ \ <string name="msg_usage">Updating usage data</string>'
=======
grep -Rl "\<string name=\"help_dangerous" . | xargs sed -i -e '/string name="msg_filtering/a \
\ \ \ \ <string name="msg_applying">Applying</string>'
>>>>>>> ToggleTask for toggle operations


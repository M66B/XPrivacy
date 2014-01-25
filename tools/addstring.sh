#!/bin/bash
grep -RIl "\<string name=\"settings_https" res | xargs sed -i -e '/string name="settings_https/a \
\ \ \ \ <string name="settings_syscomponents">Restrict system components (Android) (requires restart)</string>'

#grep -RIl "\<string name=\"settings_syscomponents" res | xargs sed -i -e '/settings_syscomponents/d'
#grep -RIl "\<string name=\"settings_experimental" res | xargs sed -i -e 's/Experimental features/Experimental functions/g'

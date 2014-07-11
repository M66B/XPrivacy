#!/bin/bash
grep -RIl "\<string name=\"settings_quirks" res | xargs sed -i -e '/settings_quirks/d'
grep -RIl "\<string name=\"settings_lac" res | xargs sed -i -e '/settings_lac/a \
\ \ \ \ <string name=\"settings_quirks\">Quirks</string>

#grep -RIl "\<string name=\"restrict_view" res | xargs sed -i -e '/restrict_view/a \
#\ \ \ \ <string name=\"restrict_help_view\">Will restrict access to view actions and information</string>\
#\ \ \ \ <string name=\"restrict_help_system\">Will restrict access to system information, like installed applications</string>\
#\ \ \ \ <string name=\"restrict_help_shell\">Will restrict access to shell commands, including superuser commands</string>\
#\ \ \ \ <string name=\"restrict_help_storage\">Will restrict access to the storage, like files on your SD card or the internal storage</string>\
#\ \ \ \ <string name=\"restrict_help_sensors\">Will restrict access to the sensors, like the motion sensor</string>\
#\ \ \ \ <string name=\"restrict_help_phone\">Will restrict access to phone information, like your phone number</string>\
#\ \ \ \ <string name=\"restrict_help_overlay\">Will restrict overlay windows to prevent phishing</string>\
#\ \ \ \ <string name=\"restrict_help_notifications\">Will restrict access to system/application notifications and Google cloud messaging</string>\
#\ \ \ \ <string name=\"restrict_help_nfc\">Will restrict access to near field communication</string>\
#\ \ \ \ <string name=\"restrict_help_network\">Will restrict access to network information, like IP and MAC addresses and Wi-Fi network information</string>\
#\ \ \ \ <string name=\"restrict_help_messages\">Will restrict access to stored and received messages (SMS/MMS) and voicemail</string>\
#\ \ \ \ <string name=\"restrict_help_media\">Will restrict access to the camera (photos and videos) and microphone</string>\
#\ \ \ \ <string name=\"restrict_help_location\">Will restrict access to your location</string>\
#\ \ \ \ <string name=\"restrict_help_ipc\">Will restrict inter-process communication, when used to circumvent other restrictions</string>\
#\ \ \ \ <string name=\"restrict_help_internet\">Will restrict access to the internet</string>\
#\ \ \ \ <string name=\"restrict_help_identification\">Will restrict information which can identify you, like the serial number of your device</string>\
#\ \ \ \ <string name=\"restrict_help_email\">Will restrict access to e-mail information, like addresses and messages (standard and G-mail application only)</string>\
#\ \ \ \ <string name=\"restrict_help_dictionary\">Will restrict access to the user dictionary</string>\
#\ \ \ \ <string name=\"restrict_help_contacts\">Will restrict access to information about your contacts</string>\
#\ \ \ \ <string name=\"restrict_help_clipboard\">Will restrict access to information on the clipboard</string>\
#\ \ \ \ <string name=\"restrict_help_calling\">Will restrict calling phone numbers, sending messages (SMS/MMS) and access to your calling history</string>\
#\ \ \ \ <string name=\"restrict_help_calendar\">Will restrict access to calendar information, like your appointments</string>\
#\ \ \ \ <string name=\"restrict_help_browser\">Will restrict access to browser information, like your bookmarks and the download history</string>\
#\ \ \ \ <string name=\"restrict_help_accounts\">Will restrict access to account information, like your Google account, which includes your G-mail address</string>'

#grep -RIl "\<string name=\"settings_dangerous" res | xargs sed -i -e '/settings_dangerous/d'
#grep -RIl "\<string name=\"title_check_whitelist" res | xargs sed -i -e 's/Check to allow/Check to allow:/g'

module Chat.Page.Icons
  ( linkIcon
  , bellIcon
  , sendIcon
  , replyIcon
  , imageIcon
  ) where

import UI.Icon as Icon

linkIcon :: Icon.Icon
linkIcon = Icon.icon [ "M10 13a5 5 0 0 0 7.5.5l3-3a5 5 0 0 0-7-7l-1.7 1.7", "M14 11a5 5 0 0 0-7.5-.5l-3 3a5 5 0 0 0 7 7l1.7-1.7" ]

bellIcon :: Icon.Icon
bellIcon = Icon.icon [ "M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9", "M10.3 21a1.94 1.94 0 0 0 3.4 0" ]

sendIcon :: Icon.Icon
sendIcon = Icon.icon [ "M12 19V5", "m5 12 7-7 7 7" ]

replyIcon :: Icon.Icon
replyIcon = Icon.icon [ "M9 17 4 12l5-5", "M20 18v-2a4 4 0 0 0-4-4H4" ]

imageIcon :: Icon.Icon
imageIcon = Icon.icon [ "M3 5h18v14H3z", "m3 15 5-5 4 4 3-3 6 6", "M16 8h.01" ]

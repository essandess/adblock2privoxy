module Templates where

blockCss, ab2pPrefix :: String
blockCss = "{display:none,visibility:hidden}"
ab2pPrefix = "ab2p-"

terminalActionSwitch :: Bool -> String
terminalActionSwitch True = 
 "+block{ adblock rules } \\\n\
 \+server-header-tagger{ab2p-block-s}"
terminalActionSwitch False = 
 "-block \\\n\
 \-server-header-tagger{ab2p-block-s} \\\n\
 \+server-header-tagger{ab2p-unblock-d} \\\n\
 \+server-header-tagger{ab2p-unblock-s} \\\n\
 \+client-header-tagger{ab2b-unblock-u}"
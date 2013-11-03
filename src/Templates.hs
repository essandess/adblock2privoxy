module Templates where
import  {-# SOURCE #-}  UrlBlocker

blockCss, ab2pPrefix :: String
blockCss = "{display:none,visibility:hidden}"
ab2pPrefix = "ab2p-"

terminalActionSwitch :: Bool -> BlockMethod -> String
terminalActionSwitch True Request = 
 "+block{ adblock rules } \\\n\
 \+server-header-tagger{ab2p-block-s}"
terminalActionSwitch False Request = 
 "-block \\\n\
 \-server-header-tagger{ab2p-block-s} \\\n\
 \+server-header-tagger{ab2p-unblock-d} \\\n\
 \+server-header-tagger{ab2p-unblock-s} \\\n\
 \+client-header-tagger{ab2b-unblock-u}"
terminalActionSwitch True Xframe = "+xframe-filter" 
terminalActionSwitch False Xframe = "-xframe-filter" 
terminalActionSwitch False Elem = "-elem-hide-filter" 
terminalActionSwitch _ _ = "" 
 
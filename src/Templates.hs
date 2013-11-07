module Templates where
import  {-# SOURCE #-}  UrlBlocker

blockCss, ab2pPrefix, actionsFilePrefix, filtersFilePrefix :: String
blockCss = "{display:none,visibility:hidden}"
ab2pPrefix = "ab2p-"
actionsFilePrefix = "#AbBlock generated actions -- don't edit --"
filtersFilePrefix = "#AbBlock generated filters -- don't edit --"

terminalActionSwitch :: Bool -> BlockMethod -> String
terminalActionSwitch True Request = 
 "+block{ adblock rules } \\\n\
 \+server-header-tagger{ab2p-block-s} \\\n\
 \+handle-as-image \\\n\
 \+client-header-tagger{ab2p-handle-as-document-c} \\\n\ 
 \+server-header-tagger{ab2p-handle-as-document-s}"
terminalActionSwitch False Request = 
 "-block \\\n\
 \-server-header-tagger{ab2p-block-s} \\\n\
 \+server-header-tagger{ab2p-unblock-d} \\\n\
 \+server-header-tagger{ab2p-unblock-s} \\\n\
 \+client-header-tagger{ab2b-unblock-u}"
terminalActionSwitch True Xframe = "+server-header-filter{ab2p-xframe-filter}" 
terminalActionSwitch False Xframe = "-server-header-filter{ab2p-xframe-filter}" 
terminalActionSwitch False Elem = "-filter{ab2p-elemhide-filter}" 
terminalActionSwitch True Dnt = "+add-header{DNT: 1}"
terminalActionSwitch _ _ = "" 
 
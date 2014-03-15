module Templates where
import  {-# SOURCE #-}  UrlBlocker
import Paths_adblock2privoxy
import System.Directory (copyFile)
import System.FilePath ((</>))

blockCss, ab2pPrefix, actionsFilePrefix, filtersFilePrefix :: String
blockCss = "{display:none!important;visibility:hidden!important}"
ab2pPrefix = "ab2p-"
actionsFilePrefix = "#AbBlock generated actions -- don't edit --"
filtersFilePrefix = "#AbBlock generated filters -- don't edit --"

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
terminalActionSwitch True Xframe = "+server-header-filter{ab2p-xframe-filter}" 
terminalActionSwitch False Xframe = "-server-header-filter{ab2p-xframe-filter}" 
terminalActionSwitch False Elem = "-filter{ab2p-elemhide-filter}" 
terminalActionSwitch True Xpopup = "+filter{ab2p-popup-filter}" 
terminalActionSwitch False Xpopup = "-filter{ab2p-popup-filter}" 
terminalActionSwitch True Dnt = "+add-header{DNT: 1}"
terminalActionSwitch _ _ = "" 

writeTemplateFiles :: String -> IO ()
writeTemplateFiles outDir = do
        copySystem "ab2p.system.action"
        copySystem "ab2p.system.filter"
        where 
        copySystem file = do
                dataDir <- getDataDir
                copyFile (dataDir  </> "templates" </> file) (outDir </> file)
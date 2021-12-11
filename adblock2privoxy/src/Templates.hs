module Templates where
import  {-# SOURCE #-}  UrlBlocker
import Paths_adblock2privoxy
import System.FilePath ((</>))
import Data.String.Utils (replace, startswith)

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

cssProtocol :: Bool -> String
cssProtocol useHTTP
        | useHTTP == True = "http"
        | otherwise       = "https"

writeTemplateFiles :: String -> String -> Bool -> IO ()
writeTemplateFiles outDir cssDomain useHTTP = do
        copySystem "ab2p.system.action"
        copySystem "ab2p.system.filter"
        where
        filterDomain content = unlines . filter (not . null) $ filterLine <$> lns
                where
                lns = lines content
                replace' line (from, to) = replace from to line
                filterLine line
                        | null cssDomain && (startswith "[?CSS_DOMAIN]" line || startswith "[?CSS_DOMAIN_DEBUG]" line) = ""
                        | otherwise = foldl replace' line [("[?CSS_DOMAIN]", ""), ("[?CSS_DOMAIN_DEBUG]", "# "), ("[CSS_DOMAIN]", cssDomain), ("[CSS_PROTOCOL]", cssProtocol useHTTP)]

        copySystem file = do
                dataDir <- getDataDir
                content <- readFile $ dataDir  </> "templates" </> file
                writeFile (outDir </> file) $ filterDomain content

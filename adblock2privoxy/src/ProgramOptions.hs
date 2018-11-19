module ProgramOptions
(
Options(..),
fillFromLog,
parseOptions,
logOptions,
writeError,
versionText
) where
import Control.Monad.State
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>),State,Line)
import System.Console.GetOpt
import System.FilePath
import Paths_adblock2privoxy (version)
import Data.Version (showVersion)


data Options = Options
     { _showVersion :: Bool
     , _privoxyDir  :: FilePath
     , _webDir      :: FilePath
     , _taskFile    :: FilePath
     , _cssDomain   :: String
     , _forced      :: Bool
     }

options :: [OptDescr (Options -> Options)]
options =
     [ Option "v" ["version"]
         (NoArg (\ opts -> opts { _showVersion = True }))
         "Show version number"
     , Option "p"   ["privoxyDir"]
         (ReqArg (\ f opts -> opts { _privoxyDir = f })
                 "PATH")
         "Privoxy config output path"
     , Option "w"   ["webDir"]
         (ReqArg (\ f opts -> opts { _webDir = f })
                 "PATH")
         "Css files output path (optional, privoxyDir is used by default)"
     , Option "d"   ["domainCSS"]
         (ReqArg (\ d opts -> opts { _cssDomain = d })
                 "DOMAIN")
         "Domain of CSS web server (required for Element Hide functionality)"
     , Option "t"   ["taskFile"]
         (ReqArg (\ f opts -> opts { _taskFile = f })
                 "PATH")
         "Path to task file containing urls to process and options. privoxyDir, webDir and domainCSS values are taken from this file if not specified explicitly"
     , Option "f" ["forced"]
         (NoArg (\ opts -> opts { _forced = True }))
         "Run even if no sources are expired"
     ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
   case getOpt Permute options argv of
      (opts,nonOpts,[]  ) ->
                case foldl (flip id) emptyOptions opts of
                        Options False "" _ "" _ _ -> writeError "Privoxy dir or task file should be specified.\n"
                        opts'@Options{_showVersion = True} -> return (opts', nonOpts)
                        opts' -> return (setDefaults opts', nonOpts)
      (_,_,errs) -> writeError $ concat errs
   where
        setDefaults opts@(Options _ (privoxyDir@(_:_)) "" _ _ _) = setDefaults opts{ _webDir = privoxyDir }
        setDefaults opts@(Options _ privoxyDir _ "" _ _) = setDefaults opts{ _taskFile = privoxyDir </> "ab2p.task" }
        setDefaults opts = opts

versionText :: String
versionText = "adblock2privoxy version " ++ showVersion version

writeError :: String -> IO a
writeError msg = ioError $ userError $ msg ++ "\n" ++ usageInfo header options
        where
        header = versionText ++
                "\nSee home page for more details and updates: https://github.com/essandess/adblock2privoxy\n" ++
                "Usage: adblock2privoxy [OPTION...] [URL...]"


logOptions :: Options -> [String]
logOptions options' = [
        startMark,
        "Privoxy path: " ++ _privoxyDir options',
        "Web path: " ++ _webDir options',
        "CSS web server domain: " ++ _cssDomain options',
        endMark,
        ""]

startMark :: String
startMark = "----- options -----"

endMark :: String
endMark = "------- end ------"

emptyOptions :: Options
emptyOptions = Options False "" "" "" "" False


fillFromLog :: Options -> [String] -> Options
fillFromLog existing lns = execState (sequence $ parseLogOptions <$> lns') existing
   where
   lns' = filter (not.null) $ takeWhile (/= endMark).dropWhile (/= startMark) $ lns

parseLogOptions :: String -> State Options ()
parseLogOptions text = do
    info <- get
    let
        ifEmpty getter x =
                let oldValue = getter info in
                if null oldValue then x else oldValue
        privoxyPathParser = (\x -> info{_privoxyDir = ifEmpty _privoxyDir x}) <$> (string "Privoxy path: " *> many1 anyChar)
        webPathParser = (\x -> info{_webDir = ifEmpty _webDir x}) <$> (string "Web path: " *> many1 anyChar)
        cssDomainParser = (\x -> info{_cssDomain = ifEmpty _cssDomain x}) <$> (string "CSS web server domain: " *> many1 anyChar)
        stringParser = skipMany (char ' ') *>
            (try privoxyPathParser <|> try webPathParser <|> cssDomainParser)
    case parse stringParser "" text of
        Left _ -> return ()
        Right info' -> put info'

module ManPage(
        createManPage
)
where
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Char (toUpper)
import Control.Monad
import System.FilePath
import System.Directory
import Distribution.PackageDescription
import Text.Pandoc.Builder
import Distribution.Package
import Data.Time.Clock
import Data.Time
import Distribution.Version (versionBranch)
import Data.List (intercalate)
import RootPath

createManPage:: Bool -> PackageDescription -> IO ()
createManPage verbose cabalMeta = do
    pandocResult <- liftM (readRST def) $ UTF8.readFile $ rootPath ++ "README.rst"
    now <- getCurrentTime
    case pandocResult of
      Left pandocError -> print pandocError
      Right pandoc -> do
        let PackageIdentifier (PackageName name) version = package cabalMeta
        let versionText = intercalate "." $ map show $ versionBranch version
        let pandoc' = setTitle (text $ map toUpper name) .
                      setAuthors [text $ author cabalMeta] .
                      setDate (text $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) now) .
                      setMeta "section" (text "1") .
                      setMeta "header" (text "General Commands Manual") .
                      setMeta "footer" (text $ name ++ " " ++ versionText)
                      $ pandoc
        createDirectoryIfMissing True ("man" </> "man1")
        writeManPage verbose ("man" </> "man1" </> "adblock2privoxy.1") pandoc'

writeManPage :: Bool -> FilePath -> Pandoc -> IO ()
writeManPage verbose page pandoc = do
  template <- getDefaultTemplate Nothing "man"
  case template of
        Left ex -> print ex
        Right template' -> do
          let opts = def{ writerStandalone = True,
                          writerTemplate = template'}
          let manPage = writeMan opts $
                            bottomUp capitalizeHeaders pandoc
          UTF8.writeFile page manPage
          when verbose $ putStrLn $ page ++ " file created"

capitalizeHeaders :: Block -> Block
capitalizeHeaders (Header 1 attr inline) = Header 1 attr $ bottomUp capitalize inline
        where
        capitalize (Str s) = Str $ map toUpper s
        capitalize x = x
capitalizeHeaders x = x

module Main where
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Char (toUpper)
import Control.Monad
import System.FilePath
import System.Environment (getArgs)
import System.Directory
import Distribution.PackageDescription.Parse
import Distribution.Verbosity (normal)
import Distribution.PackageDescription
import Text.Pandoc.Builder
import Distribution.Package
import Data.Time.Clock 
import Data.Time
import System.Locale
import Distribution.Version (versionBranch)
import Data.List (intercalate)

-- It is helper executable updating documentation
-- and distribution packages with 
-- latest metadata from .cabal file

main :: IO ()
main = do
    verbose <- liftM (elem "--verbose") getArgs
    cabalMeta <- liftM packageDescription $ readPackageDescription normal "adblock2privoxy.cabal"
    createManPage verbose cabalMeta                                     

createManPage:: Bool -> PackageDescription -> IO ()
createManPage verbose cabalMeta = do 
    pandoc <- liftM (readRST def) $ UTF8.readFile "README.rst"
    now <- getCurrentTime
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
          when verbose $ putStrLn $ "Created " ++ page

capitalizeHeaders :: Block -> Block
capitalizeHeaders (Header 1 attr xs) = Header 1 attr $ bottomUp capitalize xs
capitalizeHeaders x = x

capitalize :: Inline -> Inline
capitalize (Str xs) = Str $ map toUpper xs
capitalize x = x
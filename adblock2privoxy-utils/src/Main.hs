module Main where
import System.Environment (getArgs)
import Distribution.PackageDescription.Parse
import Distribution.Verbosity (normal)
import Control.Monad
import Distribution.PackageDescription
import ManPage
import RpmSpec
import DebControl
import RootPath

-- It is helper executable updating documentation
-- and distribution packages with
-- latest metadata from .cabal file

main :: IO ()
main = do
    verbose <- liftM (elem "--verbose") getArgs
    cabalMeta <- liftM packageDescription $ readPackageDescription normal $ rootPath ++ "adblock2privoxy.cabal"
    createManPage verbose cabalMeta
    createRpmSpec verbose cabalMeta
    createDebControl verbose cabalMeta

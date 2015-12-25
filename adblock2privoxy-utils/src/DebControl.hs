
module DebControl (
        createDebControl
)
where
import Control.Monad
import Distribution.PackageDescription
import Distribution.Package
import CabalTemplate
import Data.Char
import RootPath


createDebControl:: Bool -> PackageDescription -> IO ()
createDebControl verbose cabalMeta = do
        let control = expandTemplate template cabalMeta
        writeFile resultFile control
        when verbose $ putStrLn $ resultFile ++ " file created"

resultFile :: String
resultFile = rootPath ++ "distribution/debbuild/DEBIAN/control"

prependSpace :: String -> String
prependSpace = (' ':)

emptyToDot :: String -> String
emptyToDot [] = "."
emptyToDot s = s

description' :: PackageDescription -> String
description' =  unlines.liftM (prependSpace.emptyToDot).lines.description

template :: [[CabalValue]]
template = []
    ## "Package:         " # text.pkgName.package
    ## "Version:         " # text.pkgVersion.package
    ## "Depends:         libgmp10"
    ## "Architecture:    #ARCH#"
    ## "Maintainer:      " # maintainer
    ## "Homepage:        " # homepage
    ## "Section:         " # map toLower <$> category
    ## "Priority:        extra"
    ## "Recommends:      privoxy, nginx"
    ## "Description:     " # synopsis # "\n" # description'


module DebControl (
        createDebControl
) 
where
import Control.Applicative 
import Control.Monad
import Distribution.PackageDescription
import Distribution.Package
import CabalTemplate
import Data.Char


createDebControl:: Bool -> PackageDescription -> IO ()
createDebControl verbose cabalMeta = do
        let control = expandTemplate template cabalMeta
        writeFile resultFile control
        when verbose $ putStrLn $ resultFile ++ " file created"
        
resultFile :: String
resultFile = "distribution/debbuild/DEBIAN/control"

template :: [[CabalValue]]
template = [] 
    ## "Package:         " # text.pkgName.package
    ## "Version:         " # text.pkgVersion.package
    ## "Depends:         libgmp10"
    ## "Architecture:    any"
    ## "Maintainer:      " # maintainer
    ## "Homepage:        " # homepage
    ## "Section:         " # map toLower <$> category
    ## "Priority:        extra"
    ## "Recommends:      privoxy, nginx"
    ## "Description:     " # synopsis # "\n" # description





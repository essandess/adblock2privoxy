import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.PackageDescription (PackageDescription(..), Executable(..))
import System.Process ( rawSystem )
import System.FilePath ( (</>) )
import System.Directory ( findExecutable )
import Distribution.Simple.Utils (info)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { 
    hookedPreProcessors = []
      -- ensure that adblock2privoxy-distrib doesn't get installed to bindir
    , copyHook = \pkgdescr ->
         (copyHook simpleUserHooks) pkgdescr{ executables =
            [x | x <- executables pkgdescr, exeName x /= "adblock2privoxy-distrib"] }
    , instHook = \pkgdescr ->
         (instHook simpleUserHooks) pkgdescr{ executables =
            [x | x <- executables pkgdescr, exeName x /= "adblock2privoxy-distrib"] }
    }
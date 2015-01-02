
module RpmSpec (
        createRpmSpec
) 
where
import Control.Applicative 
import Data.String.Utils 
import Control.Monad
import Distribution.PackageDescription
import Distribution.Package
import CabalTemplate
import Data.Time.Clock
import Data.Time.Format
import System.Locale


createRpmSpec:: Bool -> PackageDescription -> IO ()
createRpmSpec verbose cabalMeta = do
        now <- getCurrentTime
        let rpm = expandTemplate (template now) cabalMeta
        writeFile resultFile rpm
        when verbose $ putStrLn $ resultFile ++ " file created"
        
resultFile :: String
resultFile = "distribution/rpmbuild/SPECS/adblock2privoxy.spec"

template :: UTCTime -> [[CabalValue]]
template now = [] 
        ## "Name:    " # text.pkgName.package
        ## "Version: " # text.pkgVersion.package
        ## "Release: 1%{?dist}"
        ## "Summary: " # synopsis
        ## ""
        ## "License: " # text.license
        ## "URL:     " # homepage
        ## "Source0: http://hackage.haskell.org/package/" 
                # text.pkgName.package # "-" # text.pkgVersion.package # "/" 
                # text.pkgName.package # "-" # text.pkgVersion.package # ".tar.gz"
        ## "Vendor:  " # maintainer
        ## "Group:   " # category
        ## ""
        ## "BuildRequires:  ghc-Cabal-devel"
        ## "BuildRequires:  ghc-rpm-macros"
        ## "BuildRequires:  cabal-install"
        ## "BuildRequires:  zlib-devel"
        ## ""
        ## "%description"
        ## description
        ## ""
        ## ""
        ## "%prep"
        ## "%setup -q -T -D -n root"
        ## "cabal update"
        ## "cabal install --user --only-dependencies --enable-optimization=2"
        ## ""
        ## ""
        ## "%build"
        ## "%global cabal_configure_options --user"
        ## "%global ghc_user_conf 1"
        ## "%global ghc_without_dynamic 1"
        ## "%ghc_bin_build"
        ## ""
        ## ""
        ## "%install"
        ## "%ghc_bin_install"
        ## ""
        ## ""
        ## "%files"
        ## "%doc " # (unwords <$> licenseFiles) # " " 
                   # (unwords <$> filter (not.startswith "distribution") . extraSrcFiles)
        ## "%{_bindir}/%{name}"
        ## "%{_datadir}/%{name}-%{version}"
        ## ""
        ## ""
        ## "%changelog" 
        ## "* " # formatTime defaultTimeLocale "%a %b %d %Y" now 
                # " " # maintainer # " - " # text.pkgVersion.package
        ## "- Rpm release for new version (generated from cabal file)"




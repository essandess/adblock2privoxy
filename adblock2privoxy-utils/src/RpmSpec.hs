
module RpmSpec (
        createRpmSpec
)
where
import Data.String.Utils
import Control.Monad
import Distribution.PackageDescription
import Distribution.Package
import CabalTemplate
import Data.Time.Clock
import Data.Time.Format
import RootPath


createRpmSpec:: Bool -> PackageDescription -> IO ()
createRpmSpec verbose cabalMeta = do
        now <- getCurrentTime
        let rpm = expandTemplate (template now) cabalMeta
        writeFile resultFile rpm
        when verbose $ putStrLn $ resultFile ++ " file created"

resultFile :: String
resultFile = rootPath ++ "distribution/rpmbuild/SPECS/adblock2privoxy.spec"

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
        ## "BuildRequires:  stack"
        ## "BuildRequires:  zlib-devel"
        -- "BuildRequires:  ghc-rpm-macros"
        -- ## "BuildRequires:  cabal-install"
        -- ## "BuildRequires:  zlib-devel"
        ## ""
        ## "%description"
        ## description
        ## ""
        ## "%define debug_package %{nil}"
        ## ""
        ## "%prep"
        ## "%setup -q -T -D -n root"
        ## "stack setup"
        ## "stack install cabal-install"
        ## ""
        ## ""
        ## "%build"
        ## "stack build --only-dependencies"
        ## "stack exec --no-ghc-package-path runhaskell -- Setup.hs configure --user " #
           "--package-db=clear --package-db=global --package-db=\"$(stack path --snapshot-pkg-db)\" --package-db=\"$(stack path --local-pkg-db)\" " #
           "--prefix=%{_prefix} --libdir=%{_libdir} --docdir=%{?_pkgdocdir}%{!?_pkgdocdir:%{_docdir}/%{name}-%{version}} --libsubdir='$compiler/$pkgid' --datasubdir='$pkgid'"
        ## "stack exec --no-ghc-package-path runhaskell -- Setup.hs build"
        ## ""
        ## ""
        ## "%install"
        ## "stack exec --no-ghc-package-path runhaskell -- Setup.hs copy --destdir=%{buildroot} -v"
        ## "cp -r man %{buildroot}%{_mandir}"
        ## ""
        ## ""
        ## "%files"
        ## "%doc %{_mandir}"
        ## "%doc " # (unwords <$> licenseFiles) # " "
                   # (unwords <$> filter (not.startswith "man")
                                 .filter (not.startswith "distribution")
                                 .filter (not.startswith "stack")
                                 . extraSrcFiles)
        ## "%{_bindir}/%{name}"
        ## "%{_datadir}/%{name}-%{version}"
        ## ""
        ## ""
        ## "%changelog"
        ## "* " # formatTime defaultTimeLocale "%a %b %d %Y" now
                # " " # maintainer # " - " # text.pkgVersion.package
        ## "- Rpm release for version " # text.pkgVersion.package #" (generated from cabal file)"

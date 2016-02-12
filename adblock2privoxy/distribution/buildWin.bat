@ECHO OFF
ECHO   This script compiles adblock2privoxy to windows binary.

ECHO   Make sure you have Haskell Stack installed before running this script.
ECHO   See http://docs.haskellstack.org/en/stable/install_and_upgrade.html#windows
ECHO   for installation details.

SET "startpath=%~dp0\.."
SET "prefix=%~dp0\binary\adblock2privoxy"
SET "stack=%appdata%\local\bin\stack"
ECHO Install GHC and Cabal
CD %USERPROFILE%
"%stack%" setup
"%stack%" build cabal-install

ECHO Build package dependencies with stack first
CD "%startpath%"

"%stack%" build --only-dependencies

FOR /F "tokens=* USEBACKQ" %%F IN (`"%stack%" path --snapshot-pkg-db`) DO (
SET snapshotdb=%%F
)

FOR /F "tokens=* USEBACKQ" %%F IN (`"%stack%" path --local-pkg-db`) DO (
SET localdb=%%F
)

ECHO snapshots package DB = %snapshotdb%
ECHO local package DB = %localdb%

MKDIR "%prefix%"
"%stack%" exec --no-ghc-package-path runhaskell -- Setup.hs configure --user --prefix="%prefix%" --package-db=clear --package-db=global --package-db="%snapshotdb%" --package-db="%localdb%" 
"%stack%" exec --no-ghc-package-path runhaskell -- Setup.hs build
"%stack%" exec --no-ghc-package-path runhaskell -- Setup.hs install

cd "%prefix%\doc\*windows*\adblock2privoxy*"
copy "%startpath%\README.rst" .
copy "%startpath%\INSTALL.rst" .
CD "%prefix%"

ECHO Build is done. The result is in current folder
ECHO You can copy it to the final destination and run the executable from bin folder

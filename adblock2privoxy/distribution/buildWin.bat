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

ECHO Build package dependencies with stack first
CD "%startpath%"

"%stack%" build --only-dependencies

MKDIR "%prefix%"
"%stack%" build
"%stack%" install --local-bin-path="%prefix%\bin"

cd "%prefix%\doc\*windows*\adblock2privoxy*"
copy "%startpath%\README.rst" .
copy "%startpath%\INSTALL.rst" .
CD "%prefix%"

ECHO Build is done. The result is in current folder
ECHO You can copy it to the final destination and run the executable from bin folder

@ECHO OFF
ECHO   This script compiles adblock2privoxy to windows binary.

ECHO   Make sure you have Haskell comiler and Cabal installed before run of this script.
ECHO   You can download MinGHC installer (including GHC 7.8.3 compiler and Cabal) 
ECHO   from https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.8.3.exe.

MKDIR binary\adblock2privoxy
CD ..
cabal update
runhaskell Setup.hs configure --user --prefix=%cd%\distribution\binary\adblock2privoxy
runhaskell Setup.hs build
runhaskell Setup.hs install
CD distribution\binary

ECHO Build is done. The result is in current folder
ECHO You can copy it to the final destination and run the executable from bin folder
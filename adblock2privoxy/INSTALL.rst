============================
Adblock2Privoxy installation 
============================

From binary package
-------------------
There are packages for various systems available at `downloads page <http://projects.zubr.me/wiki/adblock2privoxyDownloads>`_

	* For linux: you can try RPM or DEB package (depending on your package manager).
	* For windows: Just unzip the file provided. You'll find adblock2privoxy executable is in bin folder.

From sources
------------
You can install adblock2privoxy from sources if there is no binary package for your system.

1. Ensure you have Haskell compiler and Cabal 

	* For Windows: you can download `MinGHC installer <https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.8.3.exe>`_ (It includes GHC 7.8.3 compiler and Cabal)
	* From Linux: Install Haskell platform from your distributive repository or follow `the guide <http://www.stackage.org/install>`_
2. Obtain sources. You can 

	* Either download and extract sources from `Hackage <http://hackage.haskell.org/package/adblock2privoxy>`_
	* Or clone git repository with `git clone http://projects.zubr.me/adblock2privoxy.git`

3. Open console and go to the sources folder  
4. Run:: 

	runhaskell Setup.hs configure --user
	runhaskell Setup.hs build
	runhaskell Setup.hs install

Packaging
---------
You can create your own binary package for adblock2privoxy. There are two ways: 

	* Use scripts from `distribution` folder
	* Or do the same actions as for installing from sources, but use `--prefix=[package directory]` option on configure step. After that you put content of [package directory] to package or archive. 


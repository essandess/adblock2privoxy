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

	* Install `Stack <http://docs.haskellstack.org/en/stable/install_and_upgrade.html>`_ for your platform
2. Run::

	stack setup
	stack install adblock2privoxy

Packaging
---------
You can create your own binary package for adblock2privoxy.

	* Use scripts from `distribution` folder for your platform.

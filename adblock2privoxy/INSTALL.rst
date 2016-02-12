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

You can build and run adblock2privoxy from sources if there is no binary package for your system.

#. Ensure you have Haskell Stack environment

	* Install `Stack <http://docs.haskellstack.org/en/stable/install_and_upgrade.html>`_ for your platform

#. Build the app::

	stack unpack adblock2privoxy
	cd adblock2privoxy-*
	stack setup	
	stack build

#. Run the app::

	stack exec adblock2privoxy -- [YOUR ARGS]
	#for example: stack exec adblock2privoxy -- -p /etc/privoxy -d example.com https://easylist-downloads.adblockplus.org/easylist.txt

Packaging
---------

You can create your own binary package for adblock2privoxy.

#. Use scripts from `distribution` folder for your platform.

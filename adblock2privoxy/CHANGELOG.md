2.3.0
	* hlint mods
	* Add debug code with new DebugLevel option

2.2.0
	* Remove unused debug code

2.1.1
	* Fix compilation issues for ghc 9.6.3

2.1.0
	* Use TLS for CSS server to avoid mixed content errors
	* Add --useHTTP option
        * Update to ghc 8.10.7, lts-18.18 resolver

2.0.2
        * Update to ghc 8.10.4, lts-17.11 resolver

2.0.1
	* Update to ghc 8.6.5, lts-13.25 resolver

2.0.0
	* Update to ghc 8.4.3
	* Regex wildcard optimizations
	* Fix several parsing issues
	* Fix several privoxy action syntax issues
	* Migrate to GitHub fork

1.4.2
        * Fixed "commitBuffer: invalid argument (invalid character)" problem
        * Fixed system.action file generation in case when no webserver is used
        * Improved documentation

1.4.0
        * Stack build envirionment and GHC 7.10
        * Problem with network connections should be fixed because of new HTTPS library

1.3.0
	* Build and packaging process refactoring
	* Build script for windows
	* Installation documentation
	* CSS server domain changed from hardcoded 'privoxy.zubr.me' to configurable one (ticket #1)
	* Element hiding CSS generation is skipped if the domain is not specified
	* Task files contains output paths, so it's possible to run ab2p with single parameter: task file name
	* Bugfixes for tickets #7, #8, #9
1.2.4
        * Documentation and bugfixes

1.1.0
        * Tasks support to update privoxy config from same sources
        * HTTP(s) supported to get adblock files
        * Redirect comment support
        * Sources expiration check 

1.0.0
        * First public version
        * Supports filenames in command-line
        * Page elements blocking
        * URLs blocking
        * Popups blocking
        * Adblock options: script, image, stylesheet, object, xmlhttprequest, object-subrequest, subdocument,document, elemhide, other, popup, third-party, domain=..., match-case, donottrack    

===============
Adblock2Privoxy 
===============

**Convert adblock config files to privoxy format**

Synopsis
--------

    adblock2privoxy [OPTION...] [URL...]

Objectives
----------

AdBlock Plus browser plugin has great block lists provided by big community,
but it is client software and cannot work on a server as a proxy.

Privoxy proxy has good potential to block ads at server side, 
but it experiences acute shortage of updated block lists.

This software converts adblock lists to privoxy config files format.   

Almost all adblock features are supported including

* block/unblock requests (on privoxy)

  * all syntax features are supported except for regex templates matching host name

* hide/unhide page elements (via CSS)

  * all syntax features are supported

* all block request options except for outdated ones:

  * Supported: script, image, stylesheet, object, xmlhttprequest, object-subrequest, subdocument,document, elemhide, other, popup, third-party, domain=..., match-case, donottrack
  * Unsupported: collapse, background, xbl, ping and dtd
  
Tested with privoxy version 3.0.21.
Element blocking feature requires a webserver to serve CSS files. See Nginx and Apache config examples provided.

Description
-----------

Adblock files specified by [URL]... are converted to privoxy config files and auxiliarly elemHide CSS files. Local file names and http(s) addresses are accepted as URLs. 

If no source URLs are specified, task file is used to determine sources: previously processed sources are processed again if any of them is expired. Nothing is done if all sources in the task file are up to date. 

Options
-------

    -v, --version          show version number
    -p PATH, --privoxyDir=PATH  privoxy config output path (required)
    -w PATH, --webDir=PATH      css files output path (optional, privoxyDir is used by default)
    -t PATH, --taskFile=PATH    path to task file containing urls to process
    -f, --forced           run even if no sources are expired

Usage
-----

Example of first run::

    adblock2privoxy -p /etc/privoxy -w /var/www/privoxy -t my_ab2b.task https://easylist-downloads.adblockplus.org/easylist.txt https://easylist-downloads.adblockplus.org/advblock.txt my_custom.txt

Example of subsequent runs::

    adblock2privoxy -p /etc/privoxy -w /var/www/privoxy -t my_ab2b.task

The app generates following files

	* privoxyDir: 

		* ab2p.system.action
		* ab2p.action
		* ab2p.system.filter
		* ab2p.filter

	* webDir: 

		* ab2p.common.css
		* ab2p.css
		* [lot of directories for first level domain names] 

	* taskFile:

    * special file containing execution details. It can be reused to update privoxy config from same sources. 

How to apply results
--------------------

1. Install privoxy. Optionally setup it as transparent proxy

2. Change privoxy config file: Add following lines

    actionsfile ab2p.system.action
    actionsfile ab2p.action
    filterfile ab2p.system.filter
    filterfile ab2p.filter

3. Install nginx or apache webserver

   Nginx config example::

    server {
            listen 80;
            #ab2p css domain name (optional)
            server_name privoxy.zubr.me;

            #root = webDir parameter value 
            root /var/www/privoxy; 

            location ~ ^/[^/.]+\..+/ab2p.css$ {
              	# first reverse domain names order
    			rewrite ^/([^/]*?)\.([^/.]+)(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?/ab2p.css$ /$9/$8/$7/$6/$5/$4/$3/$2/$1/ab2p.css last;
            }

            location ~ (^.*/+)[^/]+/+ab2p.css {
              	# then try to get CSS for current domain
              	# if it is unavailable - get CSS for parent domain
              	try_files $uri $1ab2p.css;
            } 
    }


   Apache config example::

    <VirtualHost *:80>
            #ab2p css domain name (optional)
            ServerName www.example.com 

            #root = webDir parameter value 
            DocumentRoot /var/www/privoxy


            RewriteEngine on

            # first reverse domain names order
            RewriteRule ^/([^/]*?)\.([^/.]+)(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?/ab2p.css$ /$9/$8/$7/$6/$5/$4/$3/$2/$1/ab2p.css [N]

            # then try to get CSS for current domain
            # if it is unavailable - get CSS for parent domain
            RewriteCond %{DOCUMENT_ROOT}/%{REQUEST_FILENAME} !-f
            RewriteRule (^.*/+)[^/]+/+ab2p.css$ $1ab2p.css [N]
    </VirtualHost>

4) Find out abdlock config files to use. Some download locations

  * EasyList - https://easylist.adblockplus.org/en/
  * Russian AD list - https://code.google.com/p/ruadlist/

5) Run adblock2privoxy providing privoxy dir, web dir and adblock input file urls

6) Restart privoxy and apache to load updated configs

Clone repository from http://projects.zubr.me/adblock2privoxy.git
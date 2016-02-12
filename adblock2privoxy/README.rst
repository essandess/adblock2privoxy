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
`Element hiding <https://adblockplus.org/filters#elemhide>`_  feature requires a webserver to serve CSS files. See Nginx and Apache config examples provided.

Description
-----------

Adblock files specified by [URL]... are converted to privoxy config files and auxiliarly elemHide CSS files. Local file names and http(s) addresses are accepted as URLs.

If no source URLs are specified, task file is used to determine sources: previously processed sources are processed again if any of them is expired. Nothing is done if all sources in the task file are up to date.

Options
-------

  -v,         --version
      Show version number
  -p PATH,    --privoxyDir=PATH
      Privoxy config output path
  -w PATH,    --webDir=PATH
      Css files output path
  -d DOMAIN,  --domainCSS=DOMAIN
      Domain of CSS web server (required for Element Hide functionality)
  -t PATH,    --taskFile=PATH
      Path to task file containing urls to process and options.
  -f,         --forced
      Run even if no sources are expired

If ``taskFile`` is not specified explicilty, ``[privoxyDir]/ab2p.task`` is used.

If task file exists and ``privoxyDir``, ``webDir`` or ``domainCSS`` is not specified, corresponding value is taken from task file.

If ``webDir`` is not specified and cannot be taken from task file, ``privoxyDir`` value is used for ``webDir``.

If ``domainCSS`` is not specified and cannot be taken from task file, `Element Hide <https://adblockplus.org/filters#elemhide>`_ **functionality become disabled**. No webserver is needed in this case.

``domainCSS`` can contain just IP address if CSS web server has no associated domain. Use ``localhost`` or ``127.0.0.1`` if you run your browser on the same machine with webserver.

Usage
-----

Example of first run::

    adblock2privoxy -p /etc/privoxy -w /var/www/privoxy -d www.example.com -t my_ab2b.task https://easylist-downloads.adblockplus.org/easylist.txt https://easylist-downloads.adblockplus.org/advblock.txt my_custom.txt

Example of subsequent runs::

    adblock2privoxy -t my_ab2b.task

The app generates following files

  * privoxyDir:

    * ab2p.system.action
    * ab2p.action
    * ab2p.system.filter
    * ab2p.filter

  * webDir:

    * ab2p.common.css
    * ab2p.css
    * [lot of directories for all levels of domain names]

  * taskFile:

    * special file containing execution details. It can be reused to update privoxy config from same sources with same options. Its name is ``ab2p.task`` by default.

How to apply results
--------------------

#. Install privoxy. Optionally setup it as transparent proxy. See `privoxy installation manual <http://www.privoxy.org/user-manual/installation.html>`_ for details.

#. Change privoxy config file located in
    
    * ``/etc/privoxy/config`` for linux
    * ``C:\Program Files\Privoxy\config.txt`` for windows

    Add following lines::

      actionsfile ab2p.system.action
      actionsfile ab2p.action
      filterfile ab2p.system.filter
      filterfile ab2p.filter

#. In order to make `Element hiding <https://adblockplus.org/filters#elemhide>`_ work you also need a webserver to serve CSS files. You can choose nginx, apache or any other webserver.
   See `nginx installation manual <https://www.nginx.com/resources/wiki/start/topics/tutorials/install/>`_,
   `apache on linux installation manual <https://httpd.apache.org/docs/2.4/install.html>`_
   or `apache on windows intallation manual <http://www.thesitewizard.com/apache/install-apache-2-windows.shtml>`_ for details.

#. Change webserver config. In examples below

   * replace ``www.example.com`` with your domain or IP address (equal to ``--domainCSS`` adblock2privoxy parameter)
   * replace ``/var/www/privoxy`` with your CSS files location (equal to ``--webDir`` adblock2privoxy parameter)
   * remember, these examples are simplified to use by unexperienced people. If you're familiar with webservers administration, you'll find better ways to apply these configs.

   Nginx config: add following lines into http section of ``nginx.conf`` file

    * for linus ``/etc/nginx/nginx.conf`` 
    * for windows ``[nginx location]\conf\nginx.conf``

    ::

      server {
            listen 80;
            #ab2p css domain name (optional, should be equal to --domainCSS parameter)
            server_name www.example.com;

            #root = --webDir parameter value
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


   Apache config: put following lines into
   
    * for linux: ``/etc/apache2/sites-available/000-default.conf`` (replace existing content)
    * for windows: ``C:\Program Files\Apache Group\Apache2\conf\httpd.conf`` (append to the end)
   
    ::

      <VirtualHost *:80>
            #ab2p css domain name (optional, should be equal to --domainCSS parameter)
            ServerName www.example.com

            #root = --webDir parameter value
            DocumentRoot /var/www/privoxy


            RewriteEngine on

            # first reverse domain names order
            RewriteRule ^/([^/]*?)\.([^/.]+)(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?(?:\.([^/.]+))?/ab2p.css$ /$9/$8/$7/$6/$5/$4/$3/$2/$1/ab2p.css [N]

            # then try to get CSS for current domain
            # if it is unavailable - get CSS for parent domain
            RewriteCond %{DOCUMENT_ROOT}/%{REQUEST_FILENAME} !-f
            RewriteRule (^.*/+)[^/]+/+ab2p.css$ $1ab2p.css [N]
      </VirtualHost>

#. Get adblock2privoxy output

     * Either run adblock2privoxy providing privoxy dir, web dir, domain and adblock input file urls such as 

       * `EasyList <https://easylist.adblockplus.org/en/>`_
       * `Russian AD list <https://code.google.com/p/ruadlist/>`_
       * and many others from `official adblock repository <https://easylist.adblockplus.org/en/>`_

     * Or just download processed lists from `downloads page <https://projects.zubr.me/wiki/adblock2privoxyDownloads>`_ and unpack ``privoxy`` to and ``web`` directories content into 

       * ``/var/www/privoxy`` and ``/var/www/privoxy`` for linux
       * ``C:\Program Files\Privoxy`` and ``[your webserver directory]`` for windows

#. Restart privoxy and webserver to load updated configs

Contribution
------------

* Clone repository from http://projects.zubr.me/adblock2privoxy.git.
* `Report bugs <https://projects.zubr.me/newticket?project=adblock2privoxy>`_

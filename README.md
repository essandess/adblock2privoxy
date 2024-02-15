# adblock2privoxy
Convert adblock config files to privoxy format.

This is a fork of Zubr's [adblock2privoxy](https://projects.zubr.me/wiki/adblock2privoxy) repo with minor optimizations for regular expressions, large outputs to the CSS [debug](../../tree/master/css/debug) directory turned off, and upgrades for the latest [ghc](../../../../commercialhaskell/stack) compiler and modules.

The webserver configation file [nginx.conf](./nginx.conf) is used for both element blocking and as a blackhole (including within HTTPS-secured pages) for the Proxy Autoconfiguration (PAC) file in the parent repo [easylist-pac-privoxy](../../../easylist-pac-privoxy).

## Synopsis

```
adblock2privoxy [OPTION...] [URL...]
```

The files in the example [privoxy](../../../adblock2privoxy/tree/master/privoxy) and [css](../../../adblock2privoxy/tree/master/css) directories are created with the command:

```
adblock2privoxy -p ./privoxy -w ./css -d 127.0.0.1:8119 ./easylist/*.txt
```

After installing working binaries (below), an example production run with regular updates looks like:

```
adblock2privoxy -p /usr/local/etc/adblock2privoxy/privoxy -w /usr/local/etc/adblock2privoxy/css -d 10.0.1.3:8119 \
  https://easylist.to/easylist/easyprivacy.txt  \
  https://easylist.to/easylist/easylist.txt  \
  https://easylist.to/easylist/fanboy-annoyance.txt  \
  https://easylist.to/easylist/fanboy-social.txt  \
  https://easylist-downloads.adblockplus.org/antiadblockfilters.txt  \
  https://easylist-downloads.adblockplus.org/malwaredomains_full.txt  \
  https://raw.githubusercontent.com/ryanbr/fanboy-adblock/master/fanboy-antifacebook.txt \
  https://raw.githubusercontent.com/Dawsey21/Lists/master/adblock-list.txt

# then every few days
adblock2privoxy -t /usr/local/etc/adblock2privoxy/privoxy/ab2p.task
# restart privoxy, e.g. sudo port unload privoxy ; sudo port load privoxy
```

# macOS Installation

The macOS repo is part of [MacPorts](https://www.macports.org). The installation details below this section are deprecated.

To install:
```
sudo port install adblock2privoxy
sudo port load adblock2privoxy
```

# Unix Installation

## Quick Build/Install Example

This will build a local `/usr/local/bin/adblock2privoxy` executable from source and templates saved in `/usr/local/etc/adblock2privoxy/adblock2privoxy`.

```
curl -sSL https://get.haskellstack.org/ | sh
sudo mkdir -p /usr/local/etc/adblock2privoxy
sudo rsync -a ./adblock2privoxy* /usr/local/etc/adblock2privoxy
PATH=/usr/bin:$PATH sudo -E bash -c 'export STACK_ROOT=/usr/local/etc/.stack ; cd /usr/local/etc/adblock2privoxy/adblock2privoxy && stack setup --allow-different-user && stack install --local-bin-path /usr/local/bin --allow-different-user'
```

## macOS launchd.plist daemons

macOS launchd.plist daemons to start `nginx` and update the `privoxy` database on a schedule. See [macOS-Fortress](../../../macOS-Fortress) for a working example.

```
sudo install -m 644 ./com.github.essandess.adblock2privoxy.plist /Library/LaunchDaemons
sudo install -m 644 ./com.github.essandess.adblock2privoxy.nginx.plist /Library/LaunchDaemons
sudo launchctl load -w /Library/LaunchDaemons/com.github.essandess.adblock2privoxy.plist
sudo launchctl load -w /Library/LaunchDaemons/com.github.essandess.adblock2privoxy.nginx.plist
sudo launchctl start com.github.essandess.adblock2privoxy
```

## Objectives
AdBlock Plus browser plugin has great block lists provided by big community, but it is client software and cannot work on a server as a proxy.

Privoxy proxy has good potential to block ads at server side, but it experiences acute shortage of updated block lists.

This software converts adblock lists to privoxy config files format.

Almost all adblock features are supported including

* block/unblock requests (on privoxy)
  * all syntax features are supported except for regex templates matching host name
* hide/unhide page elements (via CSS)
  * all syntax features are supported
* all block request options except for outdated ones:
  * Supported: script, image, stylesheet, object, xmlhttprequest, object-subrequest, subdocument,document, elemhide, other, popup, third-party, domain=..., match-case, donottrack
  * Unsupported: collapse, background, xbl, ping and dtd
  * Tested with privoxy version 3.0.21. [Element hiding](https://adblockplus.org/filters#elemhide) feature requires a webserver to serve CSS files. See Nginx and Apache config examples provided.

## Description
Adblock files specified by `[URL]...` are converted to privoxy config files and auxiliarly elemHide CSS files. Local file names and http(s) addresses are accepted as URLs.

If no source URLs are specified, task file is used to determine sources: previously processed sources are processed again if any of them is expired. Nothing is done if all sources in the task file are up to date.

## Options
```
-v, --version
  Show version number
-p PATH, --privoxyDir=PATH
  Privoxy config output path
-w PATH, --webDir=PATH
  Css files output path
-d DOMAIN, --domainCSS=DOMAIN
  Domain of CSS web server (required for Element Hide functionality)
-u, --useHTTP
  Use HTTP for CSS web server; the default is HTTPS to avoid mixed content
-g INT, --debugLevel=INT
  Debug Level. 0: Off; 1: top directory CSS; 2: full directory.
-t PATH, --taskFile=PATH
  Path to task file containing urls to process and options.
-f, --forced
  Run even if no sources are expired
```

If `taskFile` is not specified explicilty, `[privoxyDir]/ab2p.task` is used.

If task file exists and `privoxyDir`, `webDir` or `domainCSS` is not specified, corresponding value is taken from task file.

If `webDir` is not specified and cannot be taken from task file, `privoxyDir` value is used for `webDir`.

If `domainCSS` is not specified and cannot be taken from task file, [Element Hide](https://adblockplus.org/filters#elemhide) **functionality become disabled**. No webserver is needed in this case.

`domainCSS` can contain just IP address if CSS web server has no associated domain. Use `localhost` or `127.0.0.1` if you run your browser on the same machine with webserver.

## Usage
Example of first run:

```
adblock2privoxy -p /etc/privoxy -w /var/www/privoxy -d www.example.com -t my_ab2b.task https://easylist-downloads.adblockplus.org/easylist.txt https://easylist-downloads.adblockplus.org/advblock.txt my_custom.txt
```

Example of subsequent runs:

```
adblock2privoxy -t my_ab2b.task
```

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
  * special file containing execution details. It can be reused to update privoxy config from same sources with same options. Its name is ab2p.task by default.

## How to apply results

1. Install privoxy. Optionally setup it as transparent proxy. See privoxy installation manual for details.

2. Change privoxy config file located in

* `/etc/privoxy/config` for linux
* `/opt/local/etc/privoxy/config` for macOS with [MacPorts](https://www.macports.org)
* `/usr/local/etc/privoxy/config` for macOS with [Homebrew](https://brew.sh)
* `C:\Program Files\Privoxy\config.txt` for windows

Add following lines:

```
actionsfile ab2p.system.action
actionsfile ab2p.action
filterfile ab2p.system.filter
filterfile ab2p.filter
```

3. In order to make [Element hiding](https://adblockplus.org/filters#elemhide) work you also need a webserver to serve CSS files. You can choose nginx, apache or any other webserver. See [nginx installation manual](https://www.nginx.com/resources/wiki/start/topics/tutorials/install/), [apache on linux installation manual](https://httpd.apache.org/docs/2.4/install.html) or [apache on windows intallation manual](http://www.thesitewizard.com/apache/install-apache-2-windows.shtml) for details.

4. Change webserver config. In examples below

  * replace `www.example.com` with your domain or IP address (equal to `--domainCSS` adblock2privoxy parameter)
  * replace `/var/www/privoxy` with your CSS files location (equal to `--webDir` adblock2privoxy parameter)
  * remember, these examples are simplified to use by unexperienced people. If you're familiar with webservers administration, you'll find better ways to apply these configs.

Nginx config: add following lines into http section of `nginx.conf` file

  * for linux `/etc/nginx/nginx.conf`
  * for macOS with [MacPorts](https://www.macports.org) `/opt/local/etc/nginx/nginx.conf`
  * for macOS with [Homebrew](https://brew.sh) `/usr/local/etc/nginx/nginx.conf`
  * for windows `[nginx location]\conf\nginx.conf`

```
server {
      listen www.example.com:443;
      #ab2p css domain name (optional, should be equal to --domainCSS parameter)
      server_name www.example.com;

      ssl on;
      ssl_certificate      certs/adblock2privoxy-nginx.chain.pem;
      ssl_certificate_key  certs/adblock2privoxy-nginx.key.pem.decrypted;
      # use modern crypto
      # https://ssl-config.mozilla.org
      ssl_protocols TLSv1.3;
      ssl_prefer_server_ciphers on;
      ssl_dhparam certs/dhparam.pem;
      ssl_ciphers TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:EECDH+AESGCM:EDH+AESGCM;
      ssl_ecdh_curve secp384r1;
      ssl_session_timeout  180m;
      ssl_session_cache    shared:SSL:20m;
      ssl_session_tickets  off;
      add_header Strict-Transport-Security "max-age=63072000; includeSubDomains; preload";

      # comply with Content Security policy
      add_header Content-Type "text/css";
      add_header X-Content-Type-Options nosniff;

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
```

The CSS web server must use HTTPS to comply with standard Content
Security policies that prohibit mixed content. Example
[nginx.conf](./nginx.conf) and [openssl.cnf](openssl.cnf) files are
included in this repo that generate the necessary PKI. Modify
these as appropriate. Example `openssl` commands:
```bash
mkdir certs && cd certs
touch index.txt
echo 1000 > serial

# CA certificate encrypted key passphrase, both -passin and -passout
sf-pwgen --algorithm memorable --count 2 --length 24 2>/dev/null | paste -s -d -- '-' \
    1>passphrase.txt || true
if [ $(head -1 passphrase.txt | wc -c) < 20 ]; then \
    openssl rand -base64 23 1>passphrase.txt 2>/dev/null; fi
cat passphrase.txt passphrase.txt > passphrase-dbl.txt \
    && mv passphrase-dbl.txt passphrase.txt \
    || rm -f passphrase-dbl.txt
chmod go-rwx passphrase.txt

# CA encrypted key
# EC
openssl genpkey -out ca.key.pem -algorithm EC \
    -pkeyopt ec_paramgen_curve:P-256 -aes256 \
    -pass file:passphrase.txt

# RSA
# # openssl genpkey -out ca.key.pem -algorithm RSA \
# #     -pkeyopt rsa_keygen_bits:2048 -aes256 \
# #     -pass file:passphrase.txt

# CA certificate
openssl req -config openssl.cnf \
    -new -x509 -days 3650 -sha256 -extensions v3_ca -out certs/ca.cert.pem \
    -key ca.key.pem -passin file:passphrase.txt -batch

# CA certificate text verification
openssl x509 -text -noout -in ca.cert.pem

# CA certificate openssl self-verification
openssl verify -CAfile ca.cert.pem ca.cert.pem

# Server certificate encrypted key and decrypted key
openssl genpkey -out adblock2privoxy-nginx.key.pem \
    -algorithm EC -pkeyopt ec_paramgen_curve:P-384 -aes256 \
    -pass file:passphrase.txt
openssl ec -in adblock2privoxy-nginx.key.pem -passin file:passphrase.txt \
    -out adblock2privoxy-nginx.key.pem.decrypted
chmod go-rwx adblock2privoxy-nginx.key.pem.decrypted

# Server certificate CSR
openssl req -config openssl.cnf -new -sha256 -extensions server_cert \
    -key adblock2privoxy-nginx.key.pem -passin file:passphrase.txt \
    -out adblock2privoxy-nginx.csr.pem -batch

# Server certificate (825 days maximum validity)
# https://support.apple.com/en-us/HT210176
openssl ca -config openssl.cnf -days 825 -notext -md sha256 \
    -extensions server_cert -in adblock2privoxy-nginx.csr.pem \
    -out adblock2privoxy-nginx.cert.pem -passin file:passphrase.txt \
    -subj '/CN=adblock2privoxy-nginx' -batch

# Server certificate chain of trust
cat adblock2privoxy-nginx.cert.pem ca.cert.pem > adblock2privoxy-nginx.chain.pem

# Server certificate text
openssl x509 -in adblock2privoxy-nginx.cert.pem -text -noout

# Server certificate and chain validity
openssl verify -CAfile ca.cert.pem adblock2privoxy-nginx.cert.pem
openssl verify -CAfile ca.cert.pem adblock2privoxy-nginx.chain.pem

# DH params
openssl dhparam -out dhparam.pem 2048
```

Apache config: put following lines into

  * for linux: `/etc/apache2/sites-available/000-default.conf` (replace existing content)
  * for windows: `C:\Program Files\Apache Group\Apache2\conf\httpd.conf` (append to the end)

```
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
```

5. Get adblock2privoxy output

  * Either run adblock2privoxy providing privoxy dir, web dir, domain and adblock input file urls such as
    * [EasyList](https://easylist.adblockplus.org/en/)
    * [Russian AD list](https://code.google.com/p/ruadlist/)
    * and many others from [official adblock repository](https://easylist.adblockplus.org/en/)
  * Or just download processed lists from [downloads page](https://projects.zubr.me/wiki/adblock2privoxyDownloads) and unpack privoxy to and web directories content into
    * `/var/www/privoxy` and `/var/www/privoxy` for linux
    * `C:\Program Files\Privoxy` and [your webserver directory] for windows

6. Restart privoxy and webserver to load updated configs

## Contribution

* Clone repository from https://github.com/essandess/adblock2privoxy.git.
* [Report bugs](https://github.com/essandess/adblock2privoxy/issues)

## Adblock2Privoxy installation

### From binary package
There are packages for various systems available at [downloads page](http://projects.zubr.me/wiki/adblock2privoxyDownloads)
  * For linux: you can try RPM or DEB package (depending on your package manager).
  * For windows: Just unzip the file provided. You'll find adblock2privoxy executable is in bin folder.

### From sources
You can build and run adblock2privoxy from sources if there is no binary package for your system.

1. Ensure you have Haskell Stack environment
  * Install [Stack](http://docs.haskellstack.org/en/stable/install_and_upgrade.html) for your platform

2. Build the app:

```
cd adblock2privoxy
stack setup
stack build
```

&lbrack;Note: issuing the command `stack unpack adblock2privoxy` downloads the [original adblock2privoxy](https://hackage.haskell.org/package/adblock2privoxy) from Hackage to the directory `./adblock2privoxy-*`, which does not contain the modifications of this fork.&rbrack;

#### macOS build specifics
    * The `.stack` directory cannot be in a path that contains spaces
    * Use macOS's native gcc compiler in `/usr/bin/gcc`, not Macports (see issues).

```
$ which gcc
/usr/bin/gcc

$ gcc --version
Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
Apple LLVM version 8.1.0 (clang-802.0.42)

export PATH=/usr/bin:$PATH  # ensure that /usr/bin/gcc is found first
export STACK_ROOT=/path/to/local/stack/dir/without/spaces/.stack

stack setup
stack build
```

Install the binary (e.g. to `/usr/local/bin`):

```
sudo -E bash -c 'export PATH=/usr/bin:$PATH ; export STACK_ROOT=/path/to/local/stack/dir/without/spaces/.stack ; stack setup --allow-different-user ; stack install --local-bin-path /usr/local/bin --allow-different-user'
```

3. Run the app:

```
stack exec adblock2privoxy -- [YOUR ARGS]
#for example: stack exec adblock2privoxy -- -p /etc/privoxy -d example.com https://easylist-downloads.adblockplus.org/easylist.txt
```

* From the locally installed binary (with `/usr/local/bin` in `$PATH`):

```
adblock2privoxy -- [YOUR ARGS]
```

## Packaging
You can create your own binary package for adblock2privoxy.

Use scripts from `distribution` folder for your platform.

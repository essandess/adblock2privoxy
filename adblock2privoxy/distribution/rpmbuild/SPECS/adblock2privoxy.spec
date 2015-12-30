Name:    adblock2privoxy
Version: 1.4.1
Release: 1%{?dist}
Summary: Convert adblock config files to privoxy format

License: GPL-3
URL:     https://projects.zubr.me/wiki/adblock2privoxy
Source0: http://hackage.haskell.org/package/adblock2privoxy-1.4.1/adblock2privoxy-1.4.1.tar.gz
Vendor:  Alexey Zubritskiy <adblock2privoxy@zubr.me>
Group:   Web

BuildRequires:  stack
BuildRequires:  zlib-devel

%description
AdBlock Plus browser plugin has great block list files provided by big community,
but it is client software and cannot work on a server as proxy.

Privoxy proxy has good potential to block ads at server side,
but it experiences acute shortage of updated block lists.

This software converts adblock lists to privoxy config files format.

Almost all adblock features are supported including

* block/unblock requests (on privoxy)

all syntax features are supported except for regex templates matching host name

* hide/unhide page elements (via CSS)

all syntax features are supported

* all block request options except for outdated ones:

Supported: script, image, stylesheet, object, xmlhttprequest, object-subrequest, subdocument,
document, elemhide, other, popup, third-party, domain=..., match-case, donottrack

Unsupported: collapse, background, xbl, ping and dtd

%define debug_package %{nil}

%prep
%setup -q -T -D -n root
stack setup
stack install cabal-install


%build
stack build --only-dependencies
stack exec --no-ghc-package-path runhaskell -- Setup.hs configure --user --package-db=clear --package-db=global --package-db="$(stack path --snapshot-pkg-db)" --package-db="$(stack path --local-pkg-db)" --prefix=%{_prefix} --libdir=%{_libdir} --docdir=%{?_pkgdocdir}%{!?_pkgdocdir:%{_docdir}/%{name}-%{version}} --libsubdir='$compiler/$pkgid' --datasubdir='$pkgid'
stack exec --no-ghc-package-path runhaskell -- Setup.hs build


%install
stack exec --no-ghc-package-path runhaskell -- Setup.hs copy --destdir=%{buildroot} -v
cp -r man %{buildroot}%{_mandir}


%files
%doc %{_mandir}
%doc LICENSE README.rst INSTALL.rst changelog
%{_bindir}/%{name}
%{_datadir}/%{name}-%{version}


%changelog
* Wed Dec 30 2015 Alexey Zubritskiy <adblock2privoxy@zubr.me> - 1.4.1
- Rpm release for version 1.4.1 (generated from cabal file)

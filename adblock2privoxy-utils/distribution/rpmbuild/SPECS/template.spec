Name:           $name
Version:        $version
Release:        1%{?dist}
Summary:        $synopsis

License:        $license
URL:            $homepage
Source0:        http://hackage.haskell.org/package/$name-$version/$name-$version.tar.gz
Vendor:         $maintainer
Group:          $category

BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-rpm-macros
BuildRequires:  cabal-install
BuildRequires:  zlib-devel

%description
$description


%prep
%setup -q
cabal update
cabal install --user --only-dependencies --enable-optimization=2


%build
%global cabal_configure_options --user
%global ghc_user_conf 1.
%global ghc_without_dynamic 1
%ghc_bin_build


%install
%ghc_bin_install


%files
%doc $license_file
%doc $(echo $extra_source_files)
%{_bindir}/%{name}
%{_datadir}/%{name}-%{version}


%changelog 
* $(LC_ALL=en_US.utf8 date +'%a %b %d %Y') $maintainer - $version
- Spec file created

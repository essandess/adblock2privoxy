```sh
cd adblock2privoxy
cabal sdist
ls dist-newstyle/sdist
cabal upload --publish dist-newstyle/sdist/adblock2privoxy-3.0.0.tar.gz
cabal clean
```

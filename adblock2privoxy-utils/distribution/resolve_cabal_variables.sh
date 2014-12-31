#!/bin/bash
sed -r "
1 i : <<END
/^[[:alnum:]-]+:/ s/-/_/g
s/(^\w+):/END\nread -d '' \1<<'END'\n/
$ a END
$ a cat - <<END
$ r $1
$ a END
/^$/ d
/^executable\s/,$ d
" ../adblock2privoxy.cabal | bash

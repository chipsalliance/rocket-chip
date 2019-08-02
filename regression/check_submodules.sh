#!/bin/sh

filter='.[]|select(.commit == "\1")|select(.name == "\2")'

git submodule status \
| sed -r "s/\-([a-zA-Z0-9]+)\s([a-zA-Z0-9\-]+)/$filter/g" \
| xargs -d '\n' -I % sh -c "jq -e '%' wit-manifest.json"

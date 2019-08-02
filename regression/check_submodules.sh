#!/bin/sh

filter='.[]'
filter+='|select(.name == \"$path\")'
filter+='|select(.name == \"$name\")'
filter+='|select(.commit == \"$sha1\")'
filter="\"$filter\""

git submodule foreach --quiet "jq -e $filter ../wit-manifest.json" || { \
  echo "git submodules do not match wit dependencies!" && exit 1; \
}

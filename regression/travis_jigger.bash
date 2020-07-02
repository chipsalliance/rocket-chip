# MIT LICENSE
#
# Copyright (c) 2018 Travis CI GmbH <contact+travis-build@travis-ci.org>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# This file came from
# https://github.com/travis-ci/travis-build/blob/73a5393263e0b135f49aceeb40ef6f0d827b9b11/lib/travis/build/bash/travis_jigger.bash

travis_jigger() {
  local cmd_pid="${1}"
  shift
  local timeout="${1}"
  shift
  local count=0

  echo -e "\\n"

  while [[ "${count}" -lt "${timeout}" ]]; do
    count="$((count + 1))"
    echo -ne "Still running (${count} of ${timeout}): ${*}\\r"
    sleep 60
  done

  echo -e "\\n${ANSI_RED}Timeout (${timeout} minutes) reached. Terminating \"${*}\"${ANSI_RESET}\\n"
  kill -9 "${cmd_pid}"
}

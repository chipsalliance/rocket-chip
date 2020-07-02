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
# https://github.com/travis-ci/travis-build/blob/73a5393263e0b135f49aceeb40ef6f0d827b9b11/lib/travis/build/bash/travis_wait.bash

travis_wait() {
  set +e
  local timeout="${1}"

  if [[ "${timeout}" =~ ^[0-9]+$ ]]; then
    shift
  else
    timeout=20
  fi

  local cmd=("${@}")
  local log_file="travis_wait_${$}.log"

  "${cmd[@]}" &>"${log_file}" &
  local cmd_pid="${!}"

  travis_jigger "${!}" "${timeout}" "${cmd[@]}" &
  local jigger_pid="${!}"
  local result

  {
    wait "${cmd_pid}" 2>/dev/null
    result="${?}"
    ps -p"${jigger_pid}" &>/dev/null && kill "${jigger_pid}"
  }

  if [[ "${result}" -eq 0 ]]; then
    printf "\\n${ANSI_GREEN}The command %s exited with ${result}.${ANSI_RESET}\\n" "${cmd[*]}"
  else
    printf "\\n${ANSI_RED}The command %s exited with ${result}.${ANSI_RESET}\\n" "${cmd[*]}"
  fi

  echo -e "\\n${ANSI_GREEN}Log:${ANSI_RESET}\\n"
  cat "${log_file}"

  set -e
  return "${result}"
}

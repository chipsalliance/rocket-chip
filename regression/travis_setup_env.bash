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
# https://github.com/travis-ci/travis-build/blob/73a5393263e0b135f49aceeb40ef6f0d827b9b11/lib/travis/build/bash/travis_setup_env.bash

# shellcheck disable=SC1117

travis_setup_env() {
  export ANSI_RED="\033[31;1m"
  export ANSI_GREEN="\033[32;1m"
  export ANSI_YELLOW="\033[33;1m"
  export ANSI_RESET="\033[0m"
  export ANSI_CLEAR="\033[0K"

  export DEBIAN_FRONTEND=noninteractive

  if [ "${TERM}" = dumb ]; then
    unset TERM
  fi
  : "${SHELL:=/bin/bash}"
  : "${TERM:=xterm}"
  : "${USER:=travis}"
  export SHELL
  export TERM
  export USER

  case $(uname | tr '[:upper:]' '[:lower:]') in
  linux*)
    export TRAVIS_OS_NAME=linux
    ;;
  darwin*)
    export TRAVIS_OS_NAME=osx
    ;;
  msys*)
    export TRAVIS_OS_NAME=windows
    ;;
  freebsd*)
    export TRAVIS_OS_NAME=freebsd
    ;;
  *)
    export TRAVIS_OS_NAME=notset
    ;;
  esac

  case $(uname -m) in
  x86_64*)
    export TRAVIS_CPU_ARCH=amd64
    ;;
  aarch64*)
    export TRAVIS_CPU_ARCH=arm64
    ;;
  ppc64le*)
    export TRAVIS_CPU_ARCH=ppc64le
    ;;
  s390x*)
    export TRAVIS_CPU_ARCH=s390x
    ;;
  *)
    export TRAVIS_CPU_ARCH=notset
    ;;
  esac

  export TRAVIS_DIST=notset
  export TRAVIS_INIT=notset
  TRAVIS_ARCH="$(uname -m)"
  if [[ "${TRAVIS_ARCH}" == x86_64 ]]; then
    TRAVIS_ARCH='amd64'
  fi
  export TRAVIS_ARCH

  if [[ "${TRAVIS_OS_NAME}" == linux ]]; then
    TRAVIS_DIST="$(lsb_release -sc 2>/dev/null || echo notset)"
    export TRAVIS_DIST
    if command -v systemctl >/dev/null 2>&1; then
      export TRAVIS_INIT=systemd
    else
      export TRAVIS_INIT=upstart
    fi
  fi

  export TRAVIS_TEST_RESULT=
  export TRAVIS_CMD=

  TRAVIS_TMPDIR="$(mktemp -d 2>/dev/null || mktemp -d -t 'travis_tmp')"
  mkdir -p "${TRAVIS_TMPDIR}"
  export TRAVIS_TMPDIR

  TRAVIS_INFRA=unknown
  if [[ "${TRAVIS_ENABLE_INFRA_DETECTION}" == true ]]; then
    TRAVIS_INFRA="$(travis_whereami | awk -F= '/^infra/ { print $2 }')"
  fi
  export TRAVIS_INFRA

  if command -v pgrep &>/dev/null; then
    pgrep -u "${USER}" 2>/dev/null |
      grep -v -w "${$}" >"${TRAVIS_TMPDIR}/pids_before" || true
  fi
}

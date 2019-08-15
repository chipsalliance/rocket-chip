#!/bin/sh -x

set -e

export WAKE_PATH=$PATH

echo "Initialize Workspace"

git config --global url."https://github.com/".insteadOf 'git@github.com:'
wit --repo-path $PWD/.. init workspace -a library-util-chipsalliance
cd workspace/

echo "Compile Scala"

wake --init .
wake --no-tty -j1 -dv 'compileScalaModule libraryUtilChipsallianceScalaModule | getPathResult'

cd library-util-chipsalliance/build-rules/sbt
#sbt compile
#can not pass compile in individual lib dir yet.

#!/bin/bash

set -o errexit -o nounset

if [ "$TRAVIS_BRANCH" != "master" ]
then
  echo "This commit was made against the $TRAVIS_BRANCH and not the master! No deploy!"
  exit 0
fi

rev=$(git rev-parse --short HEAD)

mkdir -p stage/_book

command -v texi2any >/dev/null 2>&1 || { echo >&2 "texi2any is required but it's not installed. Aborting."; exit 1; }
texi2any --html manual/clx.texinfo -css-ref=style.css -o stage/_book
cp manual/style.css stage/_book

cd stage/_book


git init
git config user.name "Documentation Bot"
git config user.email "Bender@future.com"

git remote add upstream "https://$GH_TOKEN@github.com/PuercoPop/clx-manual-ci-test.git"
git fetch upstream
git reset upstream/gh-pages

touch .
git add -A .
git commit -m "rebuild pages at ${rev}"
git push -q upstream HEAD:gh-pages

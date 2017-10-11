#!/bin/bash

git checkout master
"export TRAVIS_COMMIT_MSG=\"$(git log --format=%B --no-merges -n 1)\""
git config user.name $GN
git config user.email $GM
git config credential.helper "store --file=.git/credentials"
echo "https://$GT:@github.com" >> .git/credentials

git config push.default matching
git add --force man/*
git commit man DESCRIPTION NAMESPACE -m "update auto-generated documentation [ci skip]" || true
git push
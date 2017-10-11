#!/bin/bash

R --no-save <<< 'pkgdown::build_site()'
git checkout master
export TRAVIS_COMMIT_MSG="$(git log --format=%B --no-merges -n 1)"
git config user.name "$GN"
git config user.email "$COMMIT_AUTHOR_EMAIL"
git config credential.helper "store --file=.git/credentials"
echo "https://$GT:@github.com" >> .git/credentials
git config push.default matching
git add --force man/*
git add --force README.md
git add --force docs/*
git commit man DESCRIPTION NAMESPACE README.md docs -m "update auto-generated documentation [ci skip]" -m "$TRAVIS_COMMIT_MSG" || true
git push

#!/usr/bin/env bash
cd .. && git branch -m master rm
git checkout --orphan master
git add . && git commit -m "--orphan"
git gc --prune=now
git push -f origin master
#!/bin/sh
git checkout stable && git merge master && git push && git checkout master

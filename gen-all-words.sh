#!/bin/sh

set -e

fgrep -v '/' czech-words > czech-words-all.tmp
fgrep '/' czech-words | munchlist -v -c czech.aff -l munch >> czech-words-all.tmp
sort < czech-words-all.tmp | uniq > czech-words-all
rm czech-words-all.tmp

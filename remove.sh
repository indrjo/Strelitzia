#!/usr/bin/env sh

# Get rid of the executable "strelitzia". Albeit this task has its own
# script, under the hood there is just one `rm -rf`. This:
rm -rf $HOME/.local/bin/strelitzia

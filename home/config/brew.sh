#!/bin/sh

if [[ -z `which brew` ]]; then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Programming Langauges
brew install go nvm

# Source Control
brew install git mercurial

# Shell Utilities
brew install source-highlight tree yajl jq jo

# Network utilities
brew install socat lftp imapsync

# Graphic tools
brew install graphviz cairo

# Libraries
brew install cairo pixman
brew link cairo
brew link pixman
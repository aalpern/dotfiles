#!/bin/sh

if [[ -z `which brew` ]]; then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

<<<<<<< HEAD
# Programming Langauges
brew install go nvm

# Source Control
brew install git mercurial

# Shell Utilities
brew install go nvm source-highlight tree

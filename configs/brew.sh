#!/bin/sh

if [[ -z `which brew` ]]; then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew install go nvm source-highlight tree

# -*- mode:shell-script -*-

_load() {
    file=~/.zsh/$1
    if [[ -f $file ]]; then
        source $file
    fi
}

export HOST=`hostname -s`
export OS=`uname -s`

_load env.common
_load env.os.$OS
_load env.host.$HOST
_load options
_load prompt
_load rc.common
_load rc.os.$OS
_load rc.host.$HOST
_load local

if [[ -f /.dockerenv ]]; then
    _load rc.docker
fi

#export NVM_DIR=~/.nvm
#. $(brew --prefix nvm)/nvm.sh

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

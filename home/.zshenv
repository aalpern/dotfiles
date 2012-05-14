# -*- mode:shell-script -*-

# Set up common environment variables
export EDITOR=emacs
export HOST=`hostname -s`
export OS=`uname -s`

# set up OS-specific variables
if [[ -f ~/.zshenv.os.$OS ]]
then
    source ~/.zshenv.os.$OS
fi

# Set up host-specific variables
if [[ -f ~/.zshenv.host.$HOST ]]
then
    source ~/.zshenv.host.$HOST
fi

PATH=$HOME/bin:$PATH
PATH=$HOME/local/bin:$PATH
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

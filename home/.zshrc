# -*- mode:shell-script -*-

# Explicitly source environment variables first
if [[ -f ~/.zshenv ]]
then
    source ~/.zshenv
fi

if [[ -f $HOME/.zhosts ]]
then
  hosts=(`cat $HOME/.zhosts`)   # hostname completion
fi


# ----------------------------------------------------------------------
# SHELL SETTINGS
# ----------------------------------------------------------------------

                                # general zsh options
setopt notify noclobber menu_complete ignoreeof \
 histignoredups correctall all_export nolistbeep

limit core 0              
umask 022

#compctl -k hosts termftp ftp ncftp telnet rlogin  # expand these hosts.
#compctl -u write talk elm
#compctl -u -x "n[-1,@]" -k hosts -- mail pine elm finger talk

compctl -k "($hosts)" telnet ftp ping getit rlogin rsh rcp nslookup ncftp \
 traceroute gopher mail mhmail talk finger

if [[ -f $HOME/.zprompt ]]
then
    source $HOME/.zprompt
fi

                                # misc
set AUTO_PUSHD
TMOUT=0
WATCHFMT='%n %a %l from %m at %t.'
export GZIP="-9"
watch=all

# Colors.
red='\e[0;31m'
RED='\e[1;31m'
green='\e[0;32m' 
GREEN='\e[1;32m'
yellow='\e[0;33m'
YELLOW='\e[1;33m'
blue='\e[0;34m'
BLUE='\e[1;34m'
purple='\e[0;35m'
PURPLE='\e[1;35m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m'

# https://github.com/slashbeast/things/blob/master/configs/DOTzshrc
confirm() {
    local answer
    echo -ne "zsh: sure you want to run '${YELLOW}$@${NC}' [yN]? "
    read -q answer
        echo
    if [[ "${answer}" =~ ^[Yy]$ ]]; then
        command "${=1}" "${=@:2}"
    else
        return 1
    fi
}

confirm_wrapper() {
    if [ "$1" = '--root' ]; then
        local as_root='true'
        shift
    fi

    local runcommand="$1"; shift

    if [ "${as_root}" = 'true' ] && [ "${USER}" != 'root' ]; then
        runcommand="sudo ${runcommand}"
    fi
    confirm "${runcommand}" "$@"
}

#poweroff() { confirm_wrapper --root $0 "$@"; }
#hibernate() { confirm_wrapper --root $0 "$@"; }
reboot() { confirm_wrapper --root $0 "$@"; }

# Filename suffixes to ignore during completion
fignore=(.c~ .cxx~ .hxx~ .h~ .C~ .H~ .java~ .proto~ .py~)

# ----------------------------------------------------------------------
# Archive Management
# ----------------------------------------------------------------------

zless() { zcat $* | more }              # for reading gzipped files
zmore() { zcat $* | more }
tell()  { echo $2 | write $1 }
unsh()  { perl -ne '/^#\!/&&$x++;$x&&print;' $* | /bin/sh }
tarup() { tar zcvfp $1.tgz $1 }
untar() { tar zxvfp $* }
ltar()  { tar ztvf $* | more }
uu()    { uuencode $1 $1 > $1.uu }

unZ() 
{
    for file in $*; do
        mv $file $file.Z
        uncompress $file.Z
    done
}

# ----------------------------------------------------------------------
# git utils
# ----------------------------------------------------------------------

gpull() 
{
    pushd
    for dir in $(find $1 -type d -mindepth 1 -maxdepth 1); do
        echo "Updating $dir..."
        cd $dir
        git pull
    done
    popd
}



# =============================================================================

# Source .zcommon file for host-agnostic aliases and functions
if [[ -f ~/.zcommon ]]
then
    source ~/.zcommon
fi

# Source OS-specific rc file
if [[ -f ~/.zshrc.os.$OS ]]
then
    source ~/.zshrc.os.$OS
fi

# Source host-specific rc file
if [[ -f ~/.zshrc.host.$HOST ]]
then
    source ~/.zshrc.host.$HOST
fi


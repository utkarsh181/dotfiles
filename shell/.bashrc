#
# ~/.bashrc
#

# Shorter version of a common command that it used herein.
_checkexec() {
    command -v "$1" > /dev/null
}

# Include my scripts in the PATH.
if [ -d "$HOME"/bin ]; then
    PATH=$PATH:"$HOME"/bin
fi

if [ -d "$HOME"/.local/bin ]; then
    PATH=$PATH:"$HOME"/.local/bin
fi

# Default editor.
if pgrep -x emacs > /dev/null; then
    export VISUAL="emacsclient -c"
    export EDITOR="emacsclient -t"
elif _checkexec gvim; then
    export VISUAL="gvim"
    export EDITOR=vim
else
    export VISUAL=vim
    export EDITOR=$VISUAL
fi

# Default pager.  Note that the option I pass to it will quit once you
# try to scroll past the end of the file.
export PAGER="less --quit-at-eof"
export MANPAGER="$PAGER"

# Simple prompt
if [ -n "$SSH_CONNECTION" ]; then
	export PS1="\u@\h: \w \$ "
else
	export PS1="\w \$ "
fi
export PS2="> "

# For setting history length see HISTSIZE and HISTFILESIZE in `man bash`.
HISTSIZE=1000
HISTFILESIZE=2000

# Enable automatic color support for common commands that list output
# and also add handy aliases.
alias diff='diff --color=auto'

alias dir='dir --color=auto'
alias vdir='vdir --color=auto'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Make ls a bit easier to read.  Note that the -A is the same as -a but
# does not include implied paths (the current dir denoted by a dot and
# the previous dir denoted by two dots).
alias ls='ls -pv --color=auto --group-directories-first'
alias lsa='ls -pvA --color=auto --group-directories-first'
alias lsl='ls -lhpv --color=auto --group-directories-first'
alias lsla='ls -lhpvA --color=auto --group-directories-first'

# Safer default for cp, mv, rm.  These will print a verbose output of
# the operations.  If an existing file is affected, they will ask for
# confirmation.  This can make things a bit more cumbersome, but is a
# generally safer option.
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -Iv'

# I only ever use Stow to make symlinks with my home dir as the base
if _checkexec stow; then
    alias stow="stow -vt $HOME"
fi

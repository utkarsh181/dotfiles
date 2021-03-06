# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
PROMPT='%F{green}%n%f@%F{magenta}%m%f %F{blue}%B%~%b%f %# '
setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.

# History in cache directory and create if doesn't exit:
HISTSIZE=100000
SAVEHIST=100000
[ -d ~/.cache/zsh ] || mkdir ~/.cache/zsh ; HISTFILE=~/.cache/zsh/history

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/zshnameddirrc"

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# Emacs mode
bindkey -e
export KEYTIMEOUT=1

# Use Emacs keys in tab complete menu:
bindkey -M menuselect '^B' vi-backward-char
bindkey -M menuselect '^P' vi-up-line-or-history
bindkey -M menuselect '^F' vi-forward-char
bindkey -M menuselect '^N' vi-down-line-or-history bindkey -v '^?' backward-delete-char


# Load autosuggestions
#source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh 2>/dev/null

# Load syntax highlighting; should be last.
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null

alias rosetta="arch -x86_64 zsh"
alias gu='git add -A && echo "Commit message:" && read commitMessage  && git commit -m "$commitMessage" && git push'
alias c='clear'

export PATH=/opt/homebrew/bin:/usr/local/bin:/usr/local/opt/gettext/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/timritter/Documents/UserBinaries:/Library/Apple/usr/bin:/usr/local/opt/gettext/bin:/opt/homebrew/bin:/opt/homebrew/sbin
export PATH="/usr/local/opt/gettext/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="~/.foreman/bin:$PATH"

source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh # For autosuggestions

eval "$(starship init zsh)"
neofetch
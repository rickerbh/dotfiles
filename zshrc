# Path to your oh-my-zsh installation.
export ZSH=/Users/rickerbh/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="honukai"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git brew npm)

# User configuration

export PATH="/Users/rickerbh/.rbenv/shims:~/Library/Android/sdk/tools:~/Library/Android/sdk/platform-tools:~/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Applications/Server.app/Contents/ServerRoot/usr/bin:/Applications/Server.app/Contents/ServerRoot/usr/sbin"
# export MANPATH="/usr/local/man:$MANPATH"

alias ll="ls -l"

export PATH="/Users/rickerbh/.local/bin":"/Users/rickerbh/.npm-packages/bin":$PATH
export ANDROID_HOME=/Users/rickerbh/Library/Android/sdk
export NVM_DIR="/Users/rickerbh/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

source $ZSH/oh-my-zsh.sh
. `brew --prefix`/etc/profile.d/z.sh
source ~/.oh-my-zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export EDITOR="/usr/local/bin/et"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export GEM_HOME=$HOME/.gem
export PATH=$GEM_HOME/bin:$PATH # Add path for ruby gems

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export TERM=`ls -1R /usr/share/terminfo | grep "^eterm-color$" || ls -1R /usr/share/terminfo | grep "^aterm$" || ls -1R /usr/share/terminfo | grep "^ansi$" || ls -1R /usr/share/terminfo | grep "^xterm-256color$" || echo "xterm"`

export DEFAULT_USER="rickerbh"

export "GPG_TTY=$(tty)"

# pgcli tools
export PATH="/usr/local/opt/libpq/bin:$PATH"

# go binaries
export PATH=$PATH:~/go/bin

export PATH="$HOME/.fastlane/bin:$PATH"

eval "$(jenv init -)"

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /Users/rickerbh/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/rickerbh/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /Users/rickerbh/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/rickerbh/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/rickerbh/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/rickerbh/Downloads/google-cloud-sdk/path.zsh.inc'; fi
if [ -f '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc' ]; then . '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/rickerbh/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/rickerbh/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
if [ -f '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc' ]; then . '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'; fi

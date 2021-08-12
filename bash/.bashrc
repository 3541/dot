source ~/.bashrc.dist
source ~/.profile

export PS1='\w $ '

shopt -s globstar
set -o vi

if [[ -n "$SSH_CLIENT" ]]; then
	export PS1='[\u@\h] \w $ '
fi

export CDPATH="$CDPATH:.:/home/alex:/home/alex/projects:/home/alex/src:/home/alex/projects/u/comp30022"
source "$HOME/.cargo/env"

if [ ! -S $HOME/.ssh/ssh_auth_sock ]; then
	eval $(ssh-agent)
	ln -sf "$SSH_AUTH_SOCK" $HOME/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK=$HOME/.ssh/ssh_auth_sock

export MY_GPG_KEY=0x1EECFF9EE39ED7AA

alias jp='jq . '

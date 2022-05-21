source ~/.bashrc.dist
source ~/.profile

export PS1='\w $ '

HISTFILESIZE=100000
HISTSIZE=10000

shopt -s histappend
shopt -s checkwinsize
shopt -s extglob
shopt -s globstar
shopt -s checkjobs

alias b='bazel build //...'
alias cpu-poke='sudo cpupower frequency-set -g powersave && sudo cpupower frequency-set -g performance'
alias g='git'
alias jp='jq . '
alias r='bazel run //...'
alias t='bazel test //...'

shopt -s globstar
set -o vi
export CDPATH="$CDPATH:.:$HOME:$HOME/src"
export MY_GPG_KEY=0x1EECFF9EE39ED7AA
export DOTNET_CLI_TELEMETRY_OPTOUT=1

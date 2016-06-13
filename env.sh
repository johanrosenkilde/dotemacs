export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"

PATH=$HOME/local/bin:$HOME/code/scripts:$PATH:$HOME/.cabal/bin
PATH="/home/jsrn/.perl5/bin${PATH+:}${PATH}"; export PATH;
PERL5LIB="/home/jsrn/.perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/jsrn/.perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/jsrn/.perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/jsrn/.perl5"; export PERL_MM_OPT;

export SAGE_PATH=$HOME/code/python:$HOME/mat

# So Unison understands that my "local" copy is the same as earlier
export UNISONLOCALHOSTNAME=mole

# This plays with an ssh-agent systemd-service,
# in .config/systemd/user/ssh-agent.service
# See` https://wiki.archlinux.org/index.php?title=SSH_keys&oldid=398148
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Workaround some weird GTK bus error (seen when launching zathura from term)
export NO_AT_BRIDGE=1

# Set Pager
export PAGER="most"
export LEDGER_PAGER="most"

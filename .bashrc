### Steve's .bashrc

#if [ -f /local/starport/Config/etc/starport.bashrc ]; then
#  .  /local/starport/Config/etc/starport.bashrc
#elif [ -f /usr/local/starport/Config/etc/bashrc ]; then
#  source  /usr/local/starport/Config/etc/bashrc
#fi

PRINTER=lp
VISUAL="emacs -nw"
EDITOR="emacs -nw"
BROWSER=google-chrome:iceweasel:firefox
export VISUAL EDITOR BROWSER

(echo "$PATH"| grep -q "$HOME/bin")  || \
    PATH=$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/bin/X11:$PATH:/opt/bin
if [ -d /opt/java/bin ]; then
    PATH=/opt/java/bin:$PATH
elif [ -d /usr/local/java/bin ]; then
    PATH=/usr/local/java/bin:$PATH
fi

if [ -d $HOME/PIA/bin ]; then
    PATH=$HOME/PIA/bin:$PATH
elif [ -d $HOME/pia/bin ]; then
    PATH=$HOME/pia/bin:$PATH
fi

export PATH

export WWW_HOME=http://localweb/~steve/home.html

export IRCNICK=mdlbear
export IRCNAME="The Mandelbear"
export IRCSERVER=SanJose.CA.US.undernet.org

CVS_RSH=ssh
export CVS_RSH

LC_ALL=C
LC_COLLATE=C
export LC_ALL LC_COLLATE

no_proxy () {
    unset http_proxy
    unset wais_proxy
    unset ftp_proxy
    unset gopher_proxy
    export http_proxy wais_proxy ftp_proxy gopher_proxy
} 

a2i_proxy () {
    export http_proxy=http://http.rahul.net:80
    export no_proxy=localweb
    unset wais_proxy
    unset ftp_proxy
    unset gopher_proxy
    export http_proxy wais_proxy ftp_proxy gopher_proxy no_proxy
} 

#unalias rm

if [ "$PS1" ]; then
  PS1='(\h:$USER\W \!) '

  if [ "$TERM" = 'xterm' ]; then
    PS1='\[\033]1;\h:\u\]\[\033]2;\h:$USER \w\]($USER \!) '
  fi

  pwgrep () {
      grep -i $1 ~/Secret/pwds
  }

  getmail () {
      fetchmail | grep -v skipping | grep -v 'No mail' \
	  >> /home/steve/logs/fetchmail.log
  }

  # cd. cd's to the *real* current directory to make .. work properly
  cd.() {
      if [ "$1" != "" ]; then cd "$1"; fi
      cd `/bin/pwd`
  }

  .() {
     if [ "$1" = "" ]; then 
	cd.; dirs; l
     elif [ -d $1 ]; then
	cd. $1; dirs; l
     elif [ -d ../$1 ]; then
	cd. ../$1; dirs; l
     elif [ -f $1 ]; then
	source $1
     fi
  }

  alias a="audacity --sync"
  alias p=pushd
  alias P=popd
  export ignoreeof=0
  alias jpia=pia_wrapper
  alias ll='ls -alF --color=auto'
  alias lr='ls -aRF --color=auto'
  alias l='ls -aCF --color=auto'
  alias todo='emacs ~/Private/Journals/Dog/to.do'
#  alias chrome="google-chrome --allow-outdated-plugins >& /dev/null"
  alias chrome="google-chrome >& /dev/null"
  alias gnus="emacs -f gnus -name gnus"
  alias pidgin="pidgin -f"
  alias rwbak="ssh root@localhost mount -oremount,rw /media/bak"
  alias robak="ssh root@localhost mount -oremount,ro /media/bak"
  alias bed="ssh root@localhost acpitool -s"

  # enable bash completion in interactive shells
  # (done in starport.bashrc, but might not be present if standalone)
  if [ -f /etc/bash_completion -a -z "$BASH_COMPLETION" ]; then
      . /etc/bash_completion
  fi

  # make an alias for frm in case it's missing.  No use in scripts, of course.
  [ -x /usr/bin/frm ] || [ -x /usr/local/bin/frm ] \
      || alias frm='echo x | mail| tail +2'
fi


[ x"$CONSOLE" != x ] && { unset CONSOLE ; $HOME/bin/idle& }

## This file is executed on login.
##	It runs .bashrc, which is normally executed in non-login shells.
##	after making sure that it's actually running under bash

if [ x$0 = x-bash ]; then
    . $HOME/.bashrc
elif [ x`basename -- $0` = x'bash' ]; then
    . $HOME/.bashrc
fi

export PATH=$HOME/bin:$PATH

LC_ALL=C
LC_COLLATE=C
export LC_ALL LC_COLLATE

#[ -x /usr/bin/X11/ctwm ] && export XWINDOWMANAGER="ctwm -W -n -f $HOME/.ctwmrc.crc"
export XWINDOWMANAGER=/usr/bin/X11/ctwm

#export MANPATH=/usr/X11R6/man:/usr/local/lib/perl5:$MANPATH
#export CLASSPATH=.:/usr/local/src/local/java:/usr/local/java/lib/classes.zip

if [ "$PS1" ]; then

  ## The following display stuff before the first prompt:

  [ -x /usr/games/fortune ] && /usr/games/fortune
  uptime
  if [ true ] || [ -x /usr/bin/frm ]; then
      echo `date`, `frm | grep -v 'no mail.' | wc -l` messages total.
      frm-filter | grep -v -e 'free-\|svlug\|vsbig\|agenda\|[Aa]lsa\|overflow' 
  else
      date
  fi
  [ -x /usr/bin/ical ] && /usr/bin/ical -list
fi





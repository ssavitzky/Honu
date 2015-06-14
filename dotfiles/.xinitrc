#!/bin/bash
# $XFree86: mit/clients/xinit/xinitrc.cpp,v 1.3 1993/04/11 13:50:35 dawes Exp $
# $XConsortium: xinitrc.cpp,v 1.4 91/08/22 11:41:34 rws Exp $
# $Id: xinitrc.starport,v 1.9 1995/06/11 18:35:44 guest Exp steve $

# Note (steve@starport.com) 
#      This init file is shared by both xinit (startx) and xdm.
#      This isn't normal practice, but it makes sense.  The user
#      can override this behavior by providing both a .xinitrc and 
#      .xsession in his/her home directory.

if [ x"$XWINHOME" != x ]; then
    XDIR=$XWINHOME/lib/X11
else
    XDIR=/usr/lib/X11
fi

# Make sure /usr/bin/X11 is in the path
#[ -f $HOME/.bashrc ] && source $HOME/.bashrc

PATH=/usr/bin/X11:$PATH
export PATH

#####################################################################
#
# Figure out which resources to use.
#   Since xrdb uses cpp, the user can include the system ones.

userresources=$HOME/.Xdefaults
sysresources=$XDIR/Xdefaults
usermodmap=$HOME/.Xmodmap
sysmodmap=$XDIR/Xmodmap

if [ -f $userresources ]; then 
#   xrdb -cpp ~/bin/xrdb-cpp $userresources
   xrdb  -retain -all -cpp "/usr/bin/cpp -traditional-cpp" $userresources
else
   xrdb  -retain -all -cpp "/usr/bin/cpp -traditional-cpp" $sysresources
fi

#####################################################################
#
# Merge user and system modmaps if any

[ -f $sysmodmap ]    && xmodmap $sysmodmap
[ -f $usermodmap ]    && xmodmap $usermodmap

#####################################################################
#
# Set up some default colormaps and a nice root window color
# === (xstdcmap commented out because it eats color space that netscape wants)

# xstdcmap -all
xsetroot -solid blue


#####################################################################
#
# Clean out lock files.  Could cause havoc if you're logged in on
#	multiple machines, but the average user doesn't do that.

[ "$DISPLAY" = ':0.0' ] && rm -f $home/.netscape/lock 

#####################################################################
#
# start some useful programs:

# Start by putting ssh agent in the environment.
[ ! -z $SSH_AUTH_SOCK ] ||  eval `ssh-agent`

# Make sure programs on harmony can reach us directly --
#      it's faster, and we use harmony for audio editing

[ `hostname` != harmony ] && xhost +harmony

#   The user can provide a file called .Xapps to start applications.
#   Otherwise, we pick some.

#   We also start a windowmanager.  It's twm by default; the user can
#   override this by setting $XWINDOWMANAGER in .profile

[ x"$XWINDOWMANAGER" != x ] \
||  { [ -f /usr/bin/X11/ctwm ] && XWINDOWMANAGER=/usr/bin/X11/ctwm ; } \
||   XWINDOWMANAGER=/usr/bin/X11/twm

$XWINDOWMANAGER &
sleep 10

[ "x$XSCREENSAVER" != "x" ] \
||  { [ -f /usr/bin/X11/xscreensaver ] && XSCREENSAVER=xscreensaver ; } \
||  { [ -f /usr/bin/xscreensaver ] && XSCREENSAVER=xscreensaver ; } \
||  XSCREENSAVER=true

nice $XSCREENSAVER &

if [ -x $HOME/.Xapps ]; then
    $HOME/.Xapps
else
    sleep 1; xterm -ls -fn 8x16&
    if [ -x /usr/bin/X11/dclock ]; then
       # dclock doesn't look in .Xdefaults.host, only on the server.
       sleep 1; oclock -geometry 50x50+-3+-3& 
       sleep 1; dclock -name dclock&
   else
       sleep 1; oclock -geometry 150x50+-3+-3& 
   fi
   sleep 1; xbiff -shape&
   sleep 1; xman&
fi

#####################################################################
#
# Start a console window.  Hope that by now the old root console has died.
#
#	Note that .profile can detect $CONSOLE to put out some kind of
#	helpful message.  The user can define $XCONSOLE to override the
#	default xterm.
#
sleep 2
if [ "$XCONSOLE" = "" ]; then
   CONSOLE=1 xterm -C -name console -geometry 80x4-50+0 \
       -fn  -adobe-courier-medium-r-normal--10-100-75-75-m-60-iso8859-1
else
   $XCONSOLE
fi

[ -z $SSH_AGENT_PID ] || eval `ssh-agent -k`

### end ###


# Shell functions for tools that deal with xmonad workspaces

# Map workspace numbers and names into resistor color codes
colors () {
    case $1 in
        (0) echo "-fg white -bg black";;
        (1) echo "-fg white -bg brown";;
        (2) echo "-fg black -bg red";;
        (3) echo "-fg black -bg orange";;
        (4) echo "-fg black -bg yellow";;
        (5) echo "-fg black -bg green";;
        (6) echo "-fg black -bg #9999ff";;
        (7) echo "-fg black -bg violet";;
        (8) echo "-fg black -bg grey";;
        (9) echo "-fg black -bg white";;
        (-) echo "-fg black -bg silver";;
        (=) echo "-fg black -bg gold";;
        (*) echo "-bg grey  -fg black";;
    esac
}

dzen_colors () {
    case $1 in
        (0) echo "^fg(white)^bg(black)";;
        (1) echo "^fg(white)^bg(brown)";;
        (2) echo "^fg(black)^bg(red)";;
        (3) echo "^fg(black)^bg(orange)";;
        (4) echo "^fg(black)^bg(yellow)";;
        (5) echo "^fg(black)^bg(green)";;
        (6) echo "^fg(black)^bg(#9999ff)";;
        (7) echo "^fg(black)^bg(violet)";;
        (8) echo "^fg(black)^bg(grey)";;
        (9) echo "^fg(black)^bg(white)";;
        (-) echo "^fg(black)^bg(silver)";;
        (=) echo "^fg(black)^bg(gold)";;
        (*) echo "^bg(greys)^fg(black)";;
    esac
}

# Map workspace into key name: "-" is minus
key () {
    case $1 in
	(-) echo "minus";;
	(=) echo "equal";;
	(*) echo "$1";;
    esac
}

# map workspace into desktop index
desktop_index () {
    case $1 in
	(0) echo 9  ;;
	(-) echo 10 ;;
	(=) echo 11 ;;
	(*) echo $(( $1 - 1 ));;
    esac
}

# Extract the first number on a line.
first_number () {
    sed -e 's/^[^0-9]*//' | sed -e 's/[^0-9].*//' 
}

# get the numeric desktop ID.
#     Note that what you get from xdotool is the id of the desktop
#     that has focus, not the one a particular program is running on.
get_ws () {
    xdotool get_desktop
}

# map numeric desktop ID's, which start with 0 for the first one,
# into corresponding xmonad workspace identifiers.

ws_name_for_desktop () {
    case $1 in
	(9) echo 0;;
	(10) echo -;;
	(11) echo =;;	 
	(*) echo $((1 + $1));;
    esac
}

## Stuff used by bottom-bars, bottom-mobars, and other status reports:

HOST=`hostname`

monitor () {
    case $1 in
	(1) echo '1(w)';;
	(2) echo '2(e)';;
	(3) echo '3(r)';;
	(*) echo $1 ;;
    esac
}

up () {
    echo u$( uptime | cut -d u -f 2- \
	   | sed -e 's/ min/m/' -e 's/ users/u/' -e 's/ days*,/d/' -e 's/load average/ld/'
	   ) 
}

CITY=Freeland,WA
CC=US

weather () {
    echo $CITY:`ansiweather -a false -s true -l $CITY,$CC -u imperial\
         | cut -d '>' -f 2- \
         | sed -e s/Humidity// -e s/Pressure// -e s/Wind// -e 's/ => //g'`
}

FONT="xft:Bitstream Vera Sans:size=12:antialias=true"
BOTTOM_DZEN_OPTS="-bg black -fg #646464 -ta l"

ZPIDS=`ps x | grep "[d]zen2 $BOTTOM_DZEN_OPTS" | cut -d p -f 1`
MPIDS=`ps x | grep "[x]mobarrc-status" | cut -d p -f 1`

### Stuff used by wssetup, which needs to know what's running.

# run a command if it isn't running already
maybeRun () {
    if ps x | grep -q "[ ]$*" ; then :; else $* & sleep 2; fi
}

# run a command with a name that puts it on a given workspace (passed as $1)
#    e.g. maybeRunOn 5 emacs ...
#    This sleeps 0.5 seconds and could probably get away with less.
#    
maybeRunOn () {
    local ws=$1
    shift
    if ps x | grep -q "[ ]$*" ; then :; else (exec -a "WS=$ws $1" $*) & sleep 0.5; fi
}

# see whether a command is running
isRunning () {
    if ps x | grep -q "[ ]$*" ; then :; else false; fi
}

# start a terminal if it isn't already running.
#   $1 is the window name, typically wsN-terminal; we search for it in ps
#   so it has to be unique.  Doing it this way allows for wrappers, e.g.
#   uxterm which is a wrapper for xterm with some arguments. 
maybeTerm () {
    if ps x | grep -q "[ =]$1"; then :;
    else $HOME/.xmonad/ws-terminal ${termName}$1 & sleep 0.5;
    fi
}

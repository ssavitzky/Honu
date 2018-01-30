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
        (6) echo "-fg black -bg blue";;
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
        (6) echo "^fg(black)^bg(blue)";;
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
	(=) echo "equals";;
	(*) echo "$1";;
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

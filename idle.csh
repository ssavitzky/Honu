#!/bin/csh
#	"idle task" run in X console terminal window.
#	does x-related housekeeping chores.
#

alias xterm-font	'echo -n "]50;\!*"'	#font
alias xterm-names	'echo -n "]0;\!*"'	#icon and title
alias show-date		'echo $dt "  " `hostname`'
#alias show-date 	'echo -n `date` "  "; ypcat hosts | grep `hostname`'

# xterm-font nil2
onintr finis

set dt = (`date`)
@ dd = $dt[3] + 0
set dm = xc$dd$dt[2]$dt[6]
set y = $dt[6]

if ($?CONSOLE) then
   if ($CONSOLE == 1) then
      set dm = ""
   else
      show-date
   endif
else
   show-date
endif

while (1)
      set dt = (`date`)
      @ dd = $dt[3] + 0
      set dn = xc$dd$dt[2]$dt[6]
      if ("$dm" != "$dn") then
	 #
	 # This happens on startup and sometime between midnight and 1am
	 #
	 set y = $dt[6]
	 set dm = $dn
	 show-date
	 (cd ; ./bin/daily)
	 # x_notify
      endif
      sleep 3600
end

finis:
  # xterm-font fixed
  exit 0

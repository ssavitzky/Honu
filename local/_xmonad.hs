-- edit as needed.  Symlinked from .xmonad/Local.hs
module Local
    where

import XMonad

-- | Configuration options --
--   These are the things you might want to tweak on different machines.

-- | The Mod key.  see setxkbmap(1), xkeyboard-config(1), xmodmap(1).
--   Typical values are:
--      mod4Mask                   -- Super     (default for Honu)
--      mod3Mask                   -- right Alt (may not be mapped)
--      mod2Mask                   -- Num_Lock
--      mod1Mask                   -- Alt (default for Xmonad out of box)
--      (mod1Mask .|. controlMask) -- Ctrl+Alt
--   Recently Ubuntu and perhaps others have been leaving mod3 unmapped, and
--   mapping mod4 = Super_L (0x85),  Super_R (0x86),  Super_L (0xce),  Hyper_L (0xcf) 
--   The simplest thing is to leave myModMask as mod4, and remap caps-lock as
--   either super or hyper, which works whether or not you have a Super key.
--   e.g. setxkbmap -option caps:hyper
--   If you want Super, e.g. as an Apple key, it would be possible to use, e.g.
--   myModMask = mod3Mask and assign hyper to mod3; you need xmodmap for that.
myModMask = mod4Mask            -- mod = Super

-- | Whether to use xmobar for the top status bar on monitor 1.  Normally we
--   just use a short dzen, which leaves room for a gnome-panel or some such, but
--   if you don't want to waste space on a bottom bar it makes sense to use xmobar.
wantXmobar = False

-- | Whether to make the window names clickable.  Unless you're stuck on an older
--   machine and unwilling to recompile dzen2, you should leave this True.
wsClickable = True              --  clickable workspace names in xmobar/dzen

-- | The "good" terminal.  We default to xterm if it's not installed.  Other reasonable
--   possibilities include rxvt, gnome-terminal, etc.  Not using xfce-terminal anymore
--   because it has too many keybindings that can't be overridden, and only one profile.
goodTerminal = "/usr/bin/gnome-terminal"

-- | The width of the top dzen2 bars.  We define the first one (the one on Screen 1)
--   separately in case we want to allow space for a gnome-panel or such-like, or to
--   account for the fact that floating window positions scale with monitor size and
--   we want to label workspaces with ws
firstTopBarWidth = 950
otherTopBarWidth = 950

-- | also set the height of the top bar on external monitors.  We do this because we use
--   an xclock as a color-coded workspace label (see ws and wssetup).  xmonad scales
--   window geometries according to the screen size, and the xclock ends up being about
--   27 pixels high
otherTopBarHeight = 28

-- | font and colors.  These are defined in .mobarrc, but we use them with dzen2
--   "xft:Bitstream Vera Sans Mono:size=10:bold:antialias=true" -- original.
font    =      "xft:Inconsolata:style=medium:size=12"
bgColor =      "black"
fgColor =      "#646464"	-- normal foreground color for status bars
hiColor =      "#dddddd"	-- highlight color for status bars

-- Local Variables:
--    fill-column:90
-- End:

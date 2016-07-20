-- edit as needed.  Symlinked from .xmonad/Local.hs
module Local
    where

import XMonad

-- | Configuration options --
--   These are the things you might want to tweak on different machines.

-- | The Mod key.  mod3Mask would use right Alt; old keyboards don't have super.
-- Ubuntu's key bindings these days are in /usr/share/X11/xkb/symbols/pc; you
-- have to set Alt-R to Alt-Graphics in order to use right Alt as mod.  Or,...
-- myModMask = (mod1Mask .|. controlMask) -- defines mod as Ctrl-Alt combo.
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

-- | The width of the top dzen2 bar.  This is not just because we can't specify
--   it as a percentage -- sometimes we want to leave room for a gnome-panel,
--   xclock, or something else.
topBarWidth = 920

-- | font and colors.  These are defined in .mobarrc, but we use them with dzen2
--   "xft:Bitstream Vera Sans Mono:size=10:bold:antialias=true" -- original.
font    =      "xft:Inconsolata:style=medium:size=12"
bgColor =      "black"
fgColor =      "#646464"	-- normal foreground color for status bars
hiColor =      "#dddddd"	-- highlight color for status bars


import XMonad

import System.Directory
import System.IO                   -- hPutStrLn scope

import XMonad.Actions.CycleWS 
import XMonad.Actions.UpdatePointer

import XMonad.Hooks.DynamicLog     -- statusbar
import XMonad.Hooks.EwmhDesktops   -- extended window manager hints
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.UrgencyHook    -- window alert bells 

import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat   -- float all windows.
import XMonad.Layout.ShowWName     -- show workspace name when switching
import XMonad.Layout.Spiral        -- spiral layout
import XMonad.Layout.Tabbed        -- tabs, sort of like TWM!

import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run             -- spawnPipe and hPutStrLn

import qualified XMonad.Layout.Circle
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.WorkspaceDir
import XMonad.Layout.IndependentScreens (countScreens)

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

main = do
    -- everything that reads files or environment variables has to go inside of main.

  haveGoodTerminal <- doesFileExist goodTerminal -- detect terminal emulator
  haveXmobar <- doesFileExist "/usr/bin/xmobar"  -- detect status bar program

  let useXmobar = wantXmobar && haveXmobar       -- use it if we want it and have it
  mainbar <- spawnPipe $ myLogCommand useXmobar  -- spawn the status bar.

  -- All screens except the first get a dzen bar.  The first is generally smaller, and
  -- in any case might have a gnome panel, system tray, etc.
  nScreens <- countScreens 
  dzens    <- mapM (spawnPipe .   dzenOnScreen) [2 .. nScreens] 
 
  xmonad $ ewmh $ defaultConfig
    { modMask = myModMask
    , terminal =  if haveGoodTerminal then goodTerminal else "xterm"
    , layoutHook = myLayoutHook
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , workspaces = workspaceNames
    , logHook =  updatePointer (0.5, 0.5) (0, 0)     -- center cursor on newly focused window
                 -- updatePointer (Relative 0.5 0.5) -- unfortunately the API has changed!
                >> myLogHook useXmobar mainbar
                >> mapM_ dzenLogHook dzens
    , handleEventHook = fullscreenEventHook -- makes fullscreen work properly
    }
    `additionalKeys` myAdditionalKeys workspaceNames
    `additionalKeysP`
    [ ("M-<Left>",    prevWS )  -- Moving between workspaces
    , ("M-<Right>",   nextWS )
    , ("M-S-<Left>",  shiftToPrev )
    , ("M-S-<Right>", shiftToNext )
    ]


-- | The available layouts.  Note that each layout in a parenthesized choice group is
--   separated by |||, which denotes layout choice.  Mod-space cycles through layouts.
--   Mod-Shift-space moves the focused window to the master position.
--
--   see https://wiki.haskell.org/Xmonad/Config_archive/Thayer_Williams%27_xmonad.hs
--   for the golden ratio stuff and some other config things.
myLayoutHook = smartBorders $ avoidStruts $ showWName
               -- 0 uses simpleFloat, so you don't have to add a doFloat to the manageHook
               $ onWorkspace "0" float
               -- - is a parking area, since xmonad doesn't use icons.  I also use it
               --   for xterms that I use for things like kinit or keep-screen-on
               $ onWorkspace "-" (Grid ||| spiral ||| Circle)
	       -- 2 is where I keep my to-do list and a terminal
	       $ onWorkspace "2" ( tall ||| wide ||| tabs )
               -- Default layouts for all workspaces not otherwise mentioned
               $ ( tabs ||| tall ||| wide )
  where
    phi     = (2/(1+(toRational(sqrt(5)::Double)))) -- Golden Ratio
    tabs    = named "Tabs" simpleTabbed
    tall    = Tall 1 (2/100) phi     -- args to Tall:  nmaster delta ratio
    wide    = named "Wide" $ Mirror tall
    spiral  = named "Spiral" $ spiralWithDir East CW (6/7)
    float   = named "Float" simpleFloat

-- | The manage hook, which specifies how we treat particular kinds of windows.
--   In addition to className one can use resourceName or title
myManageHook = composeAll
    [ -- doFloat for programs that put up lots of small windows.
      className =? "Dia"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Inkscape"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "XCalc"          --> doFloat
    , className =? "XClock"         --> doFloat
    , className =? "XSane"          --> doFloat
    , className =? "XCal"           --> doFloat
      -- ignore panels, bars, and the like even if they don't set the appropriate WM hint
    , className =? "stalonetray"    --> doIgnore
    , className =? "panel"          --> doIgnore
    , className =? "dzen2"          --> doIgnore
    , className =? "xmobar"         --> doIgnore
      -- set title for special handling.  This is the -name argument for xtoolkit apps
    , title =? "xmonad-ignore"	    --> doIgnore  -- eg xclock -name xmonad-ignore
    , title =? "xmonad-float"	    --> doFloat
    ]

-- | Select the status bar for the main screen.  dzen2 is easier to configure from the
--   command line, but xmobar has its advantages, especially if you're on Ubuntu >=15.10.
myLogCommand mobar = if mobar then "xmobar" else dzenCommand
myLogHook mobar    = if mobar then mobarLogHook else dzenLogHook

-- xmobar log hook configuration

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

-- | make a workspace name clickable for xmobar.
--   Works as long as the first character of the name is the corresponding key.  If you
--   want meaningful names, use things like 1-foo
xmobarClickWrap :: String -> String
xmobarClickWrap ws = wrap start end (xmobarEscape ws)
  where start = "<action=xdotool key super+" ++ [ head ws ] ++ ">"
        end   = "</action>"
mobarLogHook pipe = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn pipe
    , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"  . clickWrap
    , ppHidden  = xmobarColor hiColor ""                  . clickWrap
    , ppHiddenNoWindows = xmobarColor fgColor ""          . clickWrap
    , ppVisible = xmobarColor hiColor "" . wrap "(" ")"   . clickWrap
    , ppUrgent  = xmobarColor "red" "yellow"              . clickWrap
    , ppTitle   = xmobarColor "green"  ""
    }
  where clickWrap = if wsClickable then xmobarClickWrap else id

-- | dzen2 log hook configuration.  Note that in order to have clickable
--   desktop names on older systems it may still be necessary to build
--   the latest version from source, but that should be simpler than xmobar.
--   Note that we use dzenOnScreen for all screens other than the first.
--   The first screen will normally have a trayer or gnome-panel on it.
dzenCommandBase = unwords [ "dzen2 -x '0' -y '0' -h '20' -ta 'l' "
                  	  , "-fg", quote fgColor
			  , "-bg", quote bgColor
			  , "-fn", quote font
		          ]
			  where quote s = wrap "'" "'" s					
dzenCommand  =   unwords [dzenCommandBase, "-w", show topBarWidth]
dzenOnScreen n = unwords [dzenCommandBase, "-xs", show n]

dzenClickWrap ws = wrap start end (dzenEscape ws)
  where start = "^ca(1,xdotool key super+" ++ [ head ws ] ++ ")"
        end   = "^ca()"

dzenLogHook pipe = dynamicLogWithPP defaultPP
    { ppOutput = hPutStrLn pipe
    , ppCurrent = dzenColor "yellow" "" . wrap "[" "]"  . clickWrap
    , ppHidden  = dzenColor hiColor ""                  . clickWrap
    , ppHiddenNoWindows = dzenColor fgColor ""          . clickWrap
    , ppVisible = dzenColor hiColor "" . wrap "(" ")"   . clickWrap
    , ppUrgent  = dzenColor "red" "yellow"              . clickWrap
    , ppTitle   = dzenColor "green"  ""
    }
  where clickWrap = if wsClickable then dzenClickWrap else id
       
-- see http://softwareprocess.es/x/x/xmonad-burn.hs for some good documentation
--
-- | The default number of workspaces (virtual screens) and their names.
--   By default we use numeric strings, but any string may be used as a
--   workspace name. The number of workspaces is determined by the
--   length of this list.  The 1-9 keys are predefined.

-- Because of the way this configuration makes workspaces clickable,
-- the first character of the name needs to be the name of the key.

workspaceNames =    [ "1","2","3","4","5","6","7","8","9" ] -- the predefined workspaces
                 ++ [ "0", "-" ]                            -- extra workspaces.

-- assign keys to the extra workspaces (only "0" and "-" so far, and - isn't clickable in
-- dzen2 apparently because of key naming issues.)
-- has to be done this way in order to pick up the clickable-wrapped actual names.
wsKeys wsNames = zip [ xK_0, xK_minus ] (drop 9 wsNames)

-- key bindings to start programs.  Note that the lock binding is the traditional Ctl-Alt-l.
myAdditionalKeys wsNames =
  [ ((mod1Mask  .|. controlMask, xK_l)     , spawn "gnome-screensaver-command --lock" ) -- lock screen
  , ((myModMask .|. controlMask, xK_e)     , spawn "emacs" )                            -- editor
  ] ++ [ -- regular and shifted bindings for myExtraWorkspaces
    ((myModMask, key), (windows $ W.greedyView ws))
    | (key, ws) <- wsKeys wsNames
  ] ++ [ ((myModMask .|. shiftMask, key), (windows $ W.shift ws)) | (key, ws) <- wsKeys wsNames ]

-- Local Variables:
--    fill-column:90
-- End:
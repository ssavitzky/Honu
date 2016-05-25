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
-- just use a short dzen, which leaves room for a gnome-panel or some such, but
-- if you don't want to waste space on a bottom bar it makes sense to use xmobar.
wantXmobar = False

-- | Whether to make the window names clickable.  Unless you're stuck on an older
-- machine and unwilling to recompile dzen2, you should leave this True.
wsClickable = True              --  clickable workspace names in xmobar/dzen

-- | The "good" terminal.  We default to xterm if it's not installed.  Other reasonable
-- possibilities include rxvt, gnome-terminal, etc.
goodTerminal = "/usr/bin/xfce4-terminal"


main = do
    -- everything that reads files or environment variables has to go inside of main.

  haveGoodTerminal <- doesFileExist goodTerminal -- detect terminal emulator
  haveXmobar <- doesFileExist "/usr/bin/xmobar"  -- detect status bar program

  let useXmobar = wantXmobar && haveXmobar
  mainbar <- spawnPipe $ myLogCommand useXmobar -- spawn the status bar.

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
--   separated by |||, which denotes layout choice. 
--
--   see https://wiki.haskell.org/Xmonad/Config_archive/Thayer_Williams%27_xmonad.hs
--   for the golden ratio stuff and some other config things.
myLayoutHook = smartBorders $ avoidStruts $ showWName
               -- 0 uses simpleFloat, so you don't have to add a doFloat to the manageHook
               $ onWorkspace "0" float
               -- - is a parking area, since xmonad doesn't use icons
               $ onWorkspace "-" (Grid ||| spiral ||| Circle)
               $ ( tabs ||| tall ||| wide ) -- applies to all workspaces not otherwise mentioned
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
      -- ignore panels, bars, and the like even if they don't set the appropriate WM hint
    , className =? "stalonetray"    --> doIgnore
    , className =? "panel"          --> doIgnore
    ]

-- | Select the status bar for the main screen.  We prefer xmobar, but older versions
--   don't support the full configuration file format, so if we're stuck on Ubuntu < 15.04
--   we use dzen2, which is drop-dead easy to recompile from source (see ../opt)
myLogCommand mobar = if mobar then "xmobar" else dzenCommand
myLogHook mobar    = if mobar then mobarLogHook else dzenLogHook

-- xmobar log hook configuration

mobarWorkspaces' = clickable . (map xmobarEscape) $ workspaceNames
  where clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                        (i,ws) <- zip ([1..9] ++ [0]) l,                                        
                        let n = i ]

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
    , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]" . clickWrap
    , ppHidden  = xmobarColor "gray" ""                  . clickWrap
    , ppHiddenNoWindows = xmobarColor "#646464" ""       . clickWrap
    , ppVisible = xmobarColor "gray" "" . wrap "(" ")"   . clickWrap
    , ppUrgent  = xmobarColor "red" "yellow"             . clickWrap
    , ppTitle   = xmobarColor "green"  "" -- xmobar truncates at }{ 
    }
  where clickWrap = if wsClickable then xmobarClickWrap else id

-- | dzen2 log hook configuration.  Note that in order to have clickable
--   desktop names on older systems it may still be necessary to build
--   the latest version from source, but that should be simpler than xmobar.
--   Note that we use dzenOnScreen for all screens other than the first.
--   The first screen is often a laptop, so it might be smaller.
dzenCommandBase = "dzen2 -x '0' -y '0' -h '20' -ta 'l' -fg '#646464' -bg 'black' -fn '"++font++"'"
dzenCommand = dzenCommandBase ++ " -w '1000' "
dzenOnScreen n = dzenCommand ++ " -xs " ++ show n

dzenClickWrap ws = wrap start end (dzenEscape ws)
  where start = "^ca(1,xdotool key super+" ++ [ head ws ] ++ ")"
        end   = "^ca()"

dzenLogHook pipe = dynamicLogWithPP defaultPP
    { ppOutput = hPutStrLn pipe
    , ppCurrent = dzenColor "yellow" "" . wrap "[" "]" . clickWrap
    , ppHidden  = dzenColor "gray" ""                  . clickWrap
    , ppHiddenNoWindows = dzenColor "#646464" ""       . clickWrap
    , ppVisible = dzenColor "gray" "" . wrap "(" ")"   . clickWrap
    , ppUrgent  = dzenColor "red" "yellow"             . clickWrap
    , ppTitle   = dzenColor "green"  ""
    }
  where clickWrap = if wsClickable then dzenClickWrap else id

-- font and colors from xmobarrc
font    =      "xft:Bitstream Vera Sans Mono:size=10:bold:antialias=true"
bgColor =      "black"
fgColor =      "#646464"
       
-- see http://softwareprocess.es/x/x/xmonad-burn.hs for some good documentation
--
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the
-- length of this list.

-- Because of the way this configuration makes workspaces clickable,
-- the first character of the name needs to be the name of the key.

workspaceNames =    [ "1","2","3","4","5","6","7","8","9" ] -- the predefined workspaces
                 ++ [ "0", "-" ]                            -- extra workspaces.

-- assign keys to the extra workspaces (only "0" and "-" so far)
-- has to be done this way in order to pick up the clickable-wrapped actual names.
wsKeys wsNames = zip [ xK_0, xK_minus ] (drop 9 wsNames)

-- took me hours to figure out that modMask was bound to something unhelpful here
-- You have to pass the fully-munged workspace names, which are only known after
-- we've figured out which status bar to mung them for.
myAdditionalKeys wsNames =
  [ ((myModMask .|. controlMask, xK_l)     , spawn "gnome-screensaver-command --lock" ) -- lock screen
  , ((myModMask .|. controlMask, xK_e)     , spawn "emacs" )                            -- editor
  ] ++ [ -- regular and shifted bindings for myExtraWorkspaces
    ((myModMask, key), (windows $ W.greedyView ws))
    | (key, ws) <- wsKeys wsNames
  ] ++ [ ((myModMask .|. shiftMask, key), (windows $ W.shift ws)) | (key, ws) <- wsKeys wsNames ]
    
-- END OF FILE ------------------------------------------------------------------------

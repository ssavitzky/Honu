import XMonad

import System.Directory
import System.IO                   -- hPutStrLn scope

import XMonad.Hooks.DynamicLog     -- statusbar 
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.UrgencyHook    -- window alert bells 

import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Layout.ShowWName     -- show workspace name when switching
import XMonad.Layout.Tabbed        -- tabs, sort of like TWM!

import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run             -- spawnPipe and hPutStrLn

import qualified XMonad.Layout.Circle
import qualified XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.WorkspaceDir


-- | Configuration options --

myModMask = mod4Mask  -- mod3Mask would use right Alt; old keyboards don't have super.
                      -- The bindings these days are in /usr/share/X11/xkb/symbols/pc
usingMobar = True     -- This should really come from an environment variable.
goodTerminal = "/usr/bin/xfce4-terminal" -- Use xterm if we don't have this


main = do
    -- everything that reads files or environment variables has to go inside of main.

  xmproc <- spawnPipe myLogBar -- spawn the status bar.
  haveGoodTerminal <- doesFileExist goodTerminal -- detect terminal emulator

  xmonad $ defaultConfig
        { modMask = myModMask
        , terminal =  if haveGoodTerminal then goodTerminal else "xterm"
	, layoutHook = myLayoutHook
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , workspaces = myWorkspaces
        , logHook = myLogHook xmproc
        }
        `additionalKeys` myAdditionalKeys
        -- could use `additionalKeysP` for emacs-like key names; you can't mix them.

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice. 

-- the default layout is simpleTabbed with smartborders applied to all
-- see https://wiki.haskell.org/Xmonad/Config_archive/Thayer_Williams%27_xmonad.hs
-- for the golden ratio stuff and some other config things.
-- showWName $ looks awful with clickable workspace strings.
myLayoutHook = smartBorders $ avoidStruts ( full ||| tiled ||| mtiled )
  where
    full    = named "Tabs" simpleTabbed
    mtiled  = named "Wide" $ Mirror tiled
    tiled   = Tall 1 (3/100) (2/(1+(toRational(sqrt(5)::Double))))
    -- sets default tile as: Tall nmaster (delta) (golden ratio)

-- | The manage hook, which specifies how we treat particular kinds of windows.
--   Graphics programs tend to put up multiple windows, so they like a floating layout.    
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "XCalc"          --> doFloat
    , className =? "Dia"            --> doFloat
    , className =? "Inkscape"       --> doFloat
    , className =? "XClock"         --> doFloat
    , className =? "stalonetray"    --> doIgnore
    , className =? "panel"          --> doIgnore
    ]

-- | The status bar, controlled by usingMobar.  Mostly we use xmobar, but older versions
--   don't support the full configuration file format, so if we're stuck on Ubuntu 12.04
--   we use dzen2.  It doesn't display status, so most people format a conky with a
--   similar color scheme and layout, sitting next to it.
myWorkspaces :: [String]
myWorkspaces = if usingMobar then mobarWorkspaces else dzenWorkspaces
myLogBar = if usingMobar then "xmobar" else "dzen2" ++ dzenArgs
myLogHook= if usingMobar then mobarLogHook else dzenLogHook

-- xmobar log hook configuration

mobarWorkspaces = clickable . (map xmobarEscape) $ workspaceNames
  where clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                        (i,ws) <- zip ([1..9] ++ [0]) l,                                        
                        let n = i ]

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

mobarLogHook pipe = dynamicLogWithPP xmobarPP    { ppOutput = hPutStrLn pipe
                                                 , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                                                 , ppHiddenNoWindows = xmobarColor "grey" ""
                                                 , ppTitle   = xmobarColor "green"  "" . shorten 50
                                                               --possibly 40 on laptops
                                                 , ppVisible = wrap "(" ")"
                                                 , ppUrgent  = xmobarColor "red" "yellow"
                                                 }

-- dzen2 log hook configuration.

dzenWorkspaces = clickable $ workspaceNames
  where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                        (i,ws) <- zip [1..] l,
                        let n = i ]
dzenArgs = " -x '0' -y '0' -h '20' -w '1000' -ta 'l' -fg '#646464' -bg 'black' -fn '"++font++"'"

dzenLogHook pipe = dynamicLogWithPP defaultPP    { ppOutput = hPutStrLn pipe
                                                 , ppCurrent = dzenColor "yellow" "" . wrap "[" "]"
                                                 , ppHiddenNoWindows = dzenColor "grey" ""
                                                 , ppTitle   = dzenColor "green"  "" . shorten 50
                                                               --possibly 40 on laptops
                                                 , ppVisible = wrap "(" ")"
                                                 , ppUrgent  = dzenColor "red" "yellow"
                                                 }

-- font and colors from xmobarrc
font    =      "xft:Bitstream Vera Sans Mono:size=10:bold:antialias=true"
bgColor =      "black"
fgColor =      "#646464"
       
-- see http://softwareprocess.es/x/x/xmonad-burn.hs for some good documentation
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
-- https://arch-ed.dk/xmobar-clickable-workspaces/ makes workspaces clickable
-- by the horrible hack of wrapping the names in <action...> tags.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
-- 
workspaceNames = ["1","2","3","4","5","6","7","8","9" ] -- the predefined workspaces
                 ++ [ "0" ]                             -- extras workspaces.

-- assign keys to the extra workspaces (only "0" so far)
-- has to be done this way in order to pick up the clickable-wrapped actual names.
myExtraWorkspaces = zip [ xK_0 ] (drop 9 myWorkspaces)

-- took me hours to figure out that modMask was bound to something unhelpful here
myAdditionalKeys =
  [
    ((myModMask .|. controlMask, xK_l)     , spawn "gnome-screensaver-command --lock" ) -- lock screen
  ] ++ [                        -- regular and shifted bindings for myExtraWorkspaces
    ((myModMask, key), (windows $ W.greedyView ws))
    | (key, ws) <- myExtraWorkspaces
    ] ++ [
    ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
    | (key, ws) <- myExtraWorkspaces
    ]
    
-- END OF FILE ------------------------------------------------------------------------

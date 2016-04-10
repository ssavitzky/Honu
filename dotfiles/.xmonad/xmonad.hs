import XMonad

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

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { modMask = myModMask
        , terminal = "xterm"
	, layoutHook = myLayoutHook
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , workspaces = myWorkspaces
        , logHook = myLogHook xmproc
        -- more changes
        }
        `additionalKeys` myAdditionalKeys
        -- could use `additionalKeysP` for emacs-like key names; you can't mix them.

myModMask = mod4Mask  -- mod3Mask to use right Alt; old keyboards don't have super
                      -- unfortunately, right alt is normally bound to mod1, and using
                      -- xmodmap to rebind it breaks X's VT switching on Ubuntu.  Grump.

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice. 

-- the default layout is simpleTabbed with smartborders applied to all
-- see https://wiki.haskell.org/Xmonad/Config_archive/Thayer_Williams%27_xmonad.hs
-- for the golden ratio stuff and some other config things.
myLayoutHook = showWName $ smartBorders $ avoidStruts ( full ||| tiled ||| mtiled )
  where
    full    = named "Tabs" simpleTabbed
    mtiled  = named "Wide" $ Mirror tiled
    tiled   = Tall 1 (3/100) (2/(1+(toRational(sqrt(5)::Double))))
    -- sets default tile as: Tall nmaster (delta) (golden ratio)

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

myLogHook = \xmproc -> dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc
                                      , ppTitle = xmobarColor "green" "" . shorten 50
                                      }
            
-- see http://softwareprocess.es/x/x/xmonad-burn.hs for some good documentation
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
-- 
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
               ++ (map snd myExtraWorkspaces)

myExtraWorkspaces = [(xK_0, "0")] -- list of (key, name)

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

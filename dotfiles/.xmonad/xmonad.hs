import XMonad

import XMonad.Layout.Tabbed

import XMonad.Hooks.ManageDocks    -- dock/tray mgmt

import qualified XMonad.Layout.Circle
import qualified XMonad.Layout.PerWorkspace
import qualified XMonad.Layout.WorkspaceDir
 
import XMonad.Hooks.UrgencyHook    -- window alert bells 
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients

main = xmonad $ defaultConfig
        { modMask = mod4Mask -- mod3Mask to use right Alt; old keyboards don't have super
                    -- unfortunately, right alt is normally bound to mod1, and using
                    -- xmodmap to rebind it breaks X's VT switching on Ubuntu.  Grump.
        , terminal = "xterm"
	, layoutHook = myLayoutHook
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , workspaces = myWorkspaces
        -- more changes
        }

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice. 

-- the default layout is fullscreen with smartborders applied to all
-- see https://wiki.haskell.org/Xmonad/Config_archive/Thayer_Williams%27_xmonad.hs
-- for the golden ratio stuff.
myLayoutHook = smartBorders $ avoidStruts ( full ||| mtiled ||| tiled )
  where
    full    = simpleTabbed
    mtiled  = Mirror tiled
    tiled   = Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
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
 
-- see http://softwareprocess.es/x/x/xmonad-burn.hs for some good documentation
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- Note that the keybindings have to be added for anything past 9
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9", "0"]


-- END OF FILE ------------------------------------------------------------------------

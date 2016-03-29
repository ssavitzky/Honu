import XMonad

import XMonad.Layout.Tabbed
import           XMonad.Layout.Accordion

import XMonad.Hooks.ManageDocks    -- dock/tray mgmt

import qualified XMonad.Layout.Accordion
import qualified XMonad.Layout.Circle
import qualified XMonad.Layout.PerWorkspace
import qualified XMonad.Layout.WorkspaceDir
 
import XMonad.Hooks.UrgencyHook    -- window alert bells 
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients

main = xmonad $ defaultConfig
        { modMask = mod4Mask -- Use Super instead of Alt
        , terminal = "xterm"
	, layoutHook = myLayoutHook
        -- more changes
        }

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice. 

-- the default layout is fullscreen with smartborders applied to all
myLayoutHook = smartBorders $ avoidStruts ( full ||| mtiled ||| tiled ||| Accordion )
  where
    full    = simpleTabbed
    mtiled  = Mirror tiled
    tiled   = Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    -- sets default tile as: Tall nmaster (delta) (golden ratio)

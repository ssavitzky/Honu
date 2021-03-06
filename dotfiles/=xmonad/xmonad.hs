import XMonad

import Data.List                   -- provides isPrefixOf, isSuffixOf, and isInfixOf
import System.Directory
import System.IO                   -- hPutStrLn scope

import XMonad.Actions.CycleWS 
import XMonad.Actions.UpdatePointer

import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog     -- statusbar
import XMonad.Hooks.EwmhDesktops   -- extended window manager hints
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.ManageHelpers  -- help in placing windows
import XMonad.Hooks.Place	   -- control window placement
import XMonad.Hooks.UrgencyHook    -- window alert bells 

import XMonad.Layout.Circle
import XMonad.Layout.FixedColumn
import XMonad.Layout.Grid
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat   -- float all windows.
import XMonad.Layout.ShowWName     -- show workspace name when switching
import XMonad.Layout.Tabbed        -- tabs, sort of like TWM!

import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run             -- spawnPipe and hPutStrLn

import qualified XMonad.Layout.Circle
import qualified XMonad.Layout.Spiral as Sp      -- spiral layout

import qualified XMonad.StackSet as W
import qualified XMonad.Layout.WorkspaceDir
import XMonad.Layout.IndependentScreens (countScreens)

-- | The local configuration options are in .xmonad/lib/Local.hs, wich in my
--    configuration is a symlink to ../../../local/local_xmonad.hs (or, in other
--    words, Honu/local/local_xmonad.hs).  It gets initialized when you install
--    Honu; the intent is that you can customize xmonad for a particular machine.
--    This is particularly useful if you have multiple laptop and desktop systems
--    with different screen sizes and keyboard layouts.  See Honu/local/_xmonad.hs
--    for the default values.
--   
import Local			   -- local configuration options

-- | There is a lot of good information on the wiki, at
--   https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips

-- | Xmonad is, well... a monad.  So you need do notation in order to get anything done.
--   It's an alias for IO, so everything that  reads files or environment variables is
--   here.  
main = do
  -- see whether we have some of the programs we might be using
  haveXmobar <- doesFileExist "/usr/bin/xmobar"  -- detect status bar program

  -- Spawn the status bar for the first screen.  Might be xmobar, but probably not.
  let useXmobar = wantXmobar && haveXmobar       -- use it if we want it and have it
  mainbar <- spawnPipe $ myLogCommand useXmobar  -- spawn the status bar.

  -- All screens except the first get a dzen bar.  The first is generally smaller, and
  -- in any case might have a gnome panel, system tray, etc.
  nScreens <- countScreens 
  dzens    <- mapM (spawnPipe .   dzenOnScreen) [2 .. nScreens] 
 
  xmonad $ ewmh $ desktopConfig -- defaultConfig
    { modMask = myModMask
    , terminal =  "ws-terminal"
    , layoutHook = myLayoutHook
    , manageHook = manageHook desktopConfig
                   <+> myManageHooks
                   <+> manageDocks
      -- manageDocks <+> myManageHook <+> manageHook defaultConfig
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
    , ("M-<Up>",      prevScreen ) -- Moving between screens
    , ("M-<Down>",    nextScreen )
    , ("M-S-<Up>",    shiftPrevScreen )
    , ("M-S-<Down>",  shiftNextScreen )
    ]


-- | The available layouts.  Note that each layout in a parenthesized choice group is
--   separated by |||, which denotes layout choice.  Mod-space cycles through layouts.
--   Mod-enter moves the focused window to the master position.
--
--   see https://wiki.haskell.org/Xmonad/Config_archive/Thayer_Williams%27_xmonad.hs
--   for the golden ratio stuff and some other config things.
myLayoutHook = smartBorders $ avoidStruts $ showWName
               -- 0 uses simpleFloat, so I don't have to add a doFloat to the manageHook
	       --   when I'm trying something new.
               $ onWorkspace "0" float
               -- - is a parking area, since xmonad doesn't use icons.  I also use it
               --   for the xterm that I use for things like kinit or keep-screen-on
               $ onWorkspace "-" (Grid ||| spiral ||| Circle)
	       -- 2 is where I keep my to-do list and a terminal; The todo layout is
               --   like tall except that it forces the master window to 90 columns.
	       $ onWorkspace "2" ( todo ||| wide ||| tabs )
	       $ onWorkspace "3" ( todo ||| wide ||| tabs )
               -- Default layouts for all workspaces not otherwise mentioned
               $ ( tabs ||| tall ||| wide )
  where
    phi     = (2/(1+(toRational(sqrt(5)::Double)))) -- Golden Ratio
    tabs    = named "Tabs" simpleTabbed
    -- the ratios for tall and wide probably ought to be local parameters
    tall    = Tall 1 (2/100) (1/2)     -- args to Tall:  nmaster delta ratio
    wide    = named "Wide" $ Mirror $ Tall 1 (2/100) phi
    spiral  = named "Spiral" $ Sp.spiralWithDir Sp.East Sp.CW (6/7)
    float   = named "Float" simpleFloat
    -- I like the emacs window for to.do to be fixed at 90 columns
    -- This sets the geometry in characters, to 90x??, so Emacs adds the fringe
    todo    = named "todo" $ FixedColumn 1 5 90 10

-- | The manage hooks that which specifies how we treat particular kinds of windows.
--   In addition to className one can use resourceName or title
myManageHooks = composeAll $
    [ -- doFloat for programs that put up many small windows or want a fixed size
      className =? "Dia"            --> doFloat
    , className =? "Gimp"           --> doFloat
    ,     title =? "gsimplecal"	    --> placeHook upperRight <+> doFloat
    , className =? "Inkscape"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "XCalc"          --> doFloat
    , className =? "XClock"         --> doFloat
    , className =? "XSane"          --> doFloat
    , className =? "XCal"           --> doFloat
      -- ignore panels, bars, and the like even if they don't set the appropriate WM hint
    ,     title =? "Desktop"        --> doIgnore
    , className =? "stalonetray"    --> doIgnore
    , className =? "panel"          --> doIgnore
    ,     title =? "panel"          --> doIgnore
    , className =? "lxpanel"        --> doIgnore
    , className =? "dzen2"          --> doIgnore
    , className =? "xfce4-panel"    --> doIgnore
    ,     title =? "xfce4-panel"    --> doIgnore
    , className =? "xmobar"         --> doIgnore
    , className =? "xvnc"	    --> doIgnore
      -- set title for special handling.  This is the -name argument for xtoolkit apps
    , title =? "xmonad-ignore"	    --> doIgnore  -- eg xclock -name xmonad-ignore
    , title =? "xmonad-float"	    --> doFloat
    , title =? "scratchpad"	    --> doFloat
    ]
    -- shift anything with a title containing "xmonad-ws=N" or starting WS=N to WS N
    ++ [fmap (("xmonad-ws="++(show n)) `isInfixOf`) title --> doShift (show n) | n <- [0..9]]
    ++ [fmap (("WS="++(show n)) `isPrefixOf`) title --> doShift (show n) | n <- [0..9]]
    ++ [fmap (("ws"++(show n)++"-") `isPrefixOf`) title --> doShift (show n) | n <- [0..9]]
    where
      upperRight = (withGaps (20,20,20,20) (fixed (1, 0)))
      lowerRight = (withGaps (20,20,20,20) (fixed (1, 1)))

-- | Select the status bar for the main screen.  dzen2 is easier to configure from the
--   command line, but xmobar has its advantages, especially if you're on Ubuntu >=15.10.
myLogCommand mobar = if mobar then "xmobar" else dzenCommand
myLogHook mobar    = if mobar then xmobarLogHook else dzenLogHook

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
  where start = "<action=xdotool key super+" ++ wsKeyName ws ++ ">"
        end   = "</action>"
xmobarLogHook pipe = dynamicLogWithPP xmobarPP
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
dzenCommandBase = unwords [ "dzen2 -dock -x '0' -y '0' -ta 'l' "
                          , "-e"
                               ++"   button1=exec:toggle\\ xcalc"
                               ++"\\;button2=exec:xdotool\\ key\\ super+shift+space"
                               ++"\\;button3=exec:xdotool\\ key\\ super+ctrl+c"
			       ++"\\;button4=exec:xdotool\\ key\\ super+Left"
			       ++"\\;button5=exec:xdotool\\ key\\ super+Right"
                  	  , "-fg", quote fgColor
			  , "-bg", quote bgColor
			  , "-fn", quote font
		          ]
			  where quote s = wrap "'" "'" s
dzenCommand  =   unwords [dzenCommandBase
                         , "-w", show firstTopBarWidth
                         , "-h", show 24
			 , "-xs", "1"
                         ]
dzenOnScreen n = unwords [dzenCommandBase
                         , "-w", show otherTopBarWidth
                         , "-h", show otherTopBarHeight -- because ws labels get scaled
                         , "-xs", show n
                         ]

-- all the work of switching workspaces, including mapping ws names to keys and 
--   identifying the mod key, is done by ~/.xmonad/ws.
dzenClickWrap ws = wrap start end (dzenEscape ws)
  where start = "^ca(1,sh -c '~/.xmonad/ws -L " ++ ws ++ "')"
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

workspaceNames =    [ "1","2","3","4","5","6","7","8","9" ] -- workspace names/keys
                 ++ [ "0", "-", "=" ]                       -- extra workspace keys.

-- assign keys to the extra workspaces ("0", "-", and "=")
-- has to be done this way in order to pick up the clickable-wrapped actual names.
wsKeys wsNames = zip [ xK_0, xK_minus, xK_equal ] (drop 9 wsNames)

-- map a workspace name into a key name that makes sense to xdotool.
wsKeyName::String -> String
wsKeyName ws = case head ws of { '=' -> "equal"; '-' -> "minus"; x -> [x]; }

-- key bindings to start programs.  Note that the lock binding is the traditional Ctl-Alt-l.
myAdditionalKeys wsNames =
  [ ((mod1Mask  .|. controlMask, xK_l     ), spawn "gnome-screensaver-command --lock" ) -- lock screen
  , ((myModMask .|. controlMask, xK_c     ), spawn "gsimplecal" )                       -- calendar
  , ((myModMask,                 xK_c     ), spawn "toggle xcalc" )                     -- calculator
  , ((myModMask,                 xK_slash ), spawn "show-keys" )                        -- show bindings
  , ((myModMask .|. controlMask, xK_e     ), spawn "emacs" )                            -- editor
  , ((myModMask .|. controlMask, xK_t     ), spawn "scratchpad" )                       -- scratchpad
  ] ++ [ -- regular and shifted bindings for myExtraWorkspaces
    ((myModMask, key), (windows $ W.greedyView ws))
    | (key, ws) <- wsKeys wsNames
  ] ++ [ ((myModMask .|. shiftMask, key), (windows $ W.shift ws)) | (key, ws) <- wsKeys wsNames ]

-- Local Variables:
--    fill-column:90
-- End:

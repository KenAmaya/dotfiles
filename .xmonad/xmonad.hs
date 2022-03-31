-- Ken's Note: Original from https://wiki.haskell.org/Xmonad/Config_archive#Quick_Introductions_to_Haskell
-- Imports

  -- Base
import XMonad
import qualified XMonad.StackSet as W

  -- Actions
import XMonad.Actions.MouseResize

  -- Contols
import Control.Monad (join, when)

  -- Data
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (maybeToList)

  -- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

  -- Layouts
import XMonad.Layout.Gaps ( gaps )
import XMonad.Layout.Accordion
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Fullscreen (fullscreenSupport, fullscreenManageHook)

  -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts)
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

  -- Utilities
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

import Graphics.X11.ExtraTypes.XF86
-- import Colors.DoomElectricOutrun --TODO: Learn how to import my own modules in Haskell

myTerminal :: String
myTerminal = "terminator"

myBrowser :: String
myBrowser = "firefox &"

myEmacs :: String
myEmacs = "emacsclient -c &"

myMsgApp :: String
myMsgApp = "flatpak run com.discordapp.Discord &"

myFileMgr :: String
myFileMgr = "nautilus"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = True

myBorderWidth :: Dimension
myBorderWidth = 1

myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces    = ["1","2","3","4","M&Ms"]
-- Workspaces Legend:
-- 1 to 4 : Main workspaces
-- M&Ms : For eMails, Music players, and Messages
myNormalBorderColor :: String
myFocusedBorderColor :: String
myNormalBorderColor  = "#400d66" -- Deep Purple
myFocusedBorderColor = "#f02ef0" -- Bright Magenta

-- Pick a random wallpaper. 'no-xinerama' so that feh will extend the wallpaper to my monitors
wallpaperChange :: String
wallpaperChange = "feh --bg-fill --no-xinerama --randomize ~/Pictures/Wallpaper"

-- Atom and ewmh full screen
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

-- eww commands
ewwSidebar :: MonadIO m => m ()
ewwSidebar = spawn "exec ~/bin/eww open-many weather_side time_side smol_calendar player_side sys_side sliders_side"

ewwCenter :: MonadIO m => m ()
ewwCenter = spawn "exec ~/bin/eww open-many blur_full weather profile quote search_full incognito-icon vpn-icon home_dir screenshot full reboot_full lock_full logout_full suspend_full"

ewwClose :: MonadIO m => m ()
ewwClose = spawn "exec ~/bin/eww close-all"

clipboardy :: MonadIO m => m () -- Don't question it
clipboardy = spawn "rofi -modi \"\63053 :greenclip print\" -show \"\63053 \" -run-command '{cmd}' -theme ~/.config/rofi/launcher/style.rasi"

rofi_launcher :: MonadIO m => m ()
rofi_launcher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"Sweet-Rainbow\" "

maimcopy :: MonadIO m => m ()
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"

maimsave :: MonadIO m => m ()
maimsave = spawn "maim -s ~/Pictures/Capture/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Pictures\" -i flameshot"



------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch Doom Emacs client
    , ((modm,               xK_o     ), spawn myEmacs)

    -- launch Firefox
    , ((modm,               xK_f     ), spawn myBrowser)

    -- launch Discord
    , ((modm .|. shiftMask, xK_f     ), spawn myMsgApp)

    -- launch Nautilus
    , ((modm,               xK_d     ), spawn myFileMgr)

    -- launch dmenu
    , ((modm,               xK_p     ), rofi_launcher)
    , ((modm .|. shiftMask, xK_p     ), spawn "dmenu_run")

    -- launch eww dashboard
    --, ((modm,               xK_b     ), ewwCenter)
    --, ((modm .|. shiftMask, xK_b     ), ewwClose)

    -- launch eww sidebar
    , ((modm,               xK_s     ), ewwSidebar)
    , ((modm .|. shiftMask, xK_s     ), ewwClose)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modm,               xK_slash   ), sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm,               xK_q     ), spawn "~/bin/powermenu.sh")

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    -- Ken's Additional Keybindings
    -- Enable the keyboard's volume keys
    , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 5%-")
    , ((modm, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 2%-")
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 5%+")
    , ((modm, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 2%+")
    , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")

    -- Screenshot buttons
    , ((0,                  xK_Print ), maimcopy)
    , ((modm,               xK_Print ), maimsave)

    , ((modm,               xK_b     ), spawn wallpaperChange)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e}, Switch to physical screens 0 or 1
    -- mod-shift-{w,e}, Move client to screen 0 or 1
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [1,0] -- Ken: Changed monitor order
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts Related Settings:

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myFont :: String
myFont = "xft:Source Code Pro:size=12:regular:antialias=true:hinting=true"

-- Setting colors for tabs layout and tabs sublayout.
myTabTheme :: Theme
myTabTheme = def { fontName            = myFont
                 , activeBorderColor   = "#fo2ef0" --my own setting: Bright Magenta
                 , inactiveBorderColor = "#400d66" --my own setting: Deep Purple
                 , activeTextColor     = "#f2f3f7"
                 , inactiveTextColor   = "#7984d1" --from panelTitle.inactiveForeground"
                 }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def { swn_font      = "xft:Fira Code:bold:size=42"
                       , swn_fade      = 1.0
                       , swn_bgcolor   = "f02ef0"
                       , swn_color     = "#f2f3f3"
                       }

-- Ken's layouts:
tall    = renamed [Replace "tall"]
          -- $ smartBorders
          $ windowNavigation
          $ addTabs shrinkText myTabTheme
          $ subLayout [] (smartBorders Simplest)
          $ limitWindows 8
          -- $ mySpacing 3
          $ ResizableTall 1 (3/100) (2/5) []

floats  = renamed [Replace "floats"]
          -- $ smartBorders
          $ limitWindows 20 simplestFloat

threeCol = renamed [Replace "threeCol"]
         $ smartBorders
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ limitWindows 8
         -- $ mySpacing 3
         $ ThreeCol 1 (3/100) (1/3)

tallAccordion = renamed [Replace "tallAccordion"]
              $ Accordion

wideAccordion = renamed [Replace "wideAccordion"]
              $ Mirror Accordion


-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
           $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
         where
           myDefaultLayout =     withBorder myBorderWidth tall
                             ||| threeCol
                             ||| tallAccordion
                             ||| wideAccordion

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook :: ManageHook
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Org.gnome.Nautilus"         --> doRectFloat ( W.RationalRect 0.2 0.2 0.5 0.5) -- Float the nautilus window somewhere in the middle(0.2) sized at half the width and height of the monitor(0.5)
    , className =? "discord"        --> doShift "M&Ms"
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> doFullFloat
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    spawnOnce "/usr/bin/emacs --daemon &" -- Launch the emacs server daemon for the emacs client
    spawnOnce wallpaperChange
    setWMName "LG3D"
    spawnOnce "~/bin/eww daemon &"
    spawn "xsetroot -cursor_name left_ptr &"
    spawnOnce "picom --experimental-backends"
    spawnOnce "greenclip daemon"
    spawnOnce "dunst"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do 
  xmproc <- spawnPipe "$HOME/bin/launch_tint2.sh &" -- xmobar settings for main display
--  xmprox <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/.xmobarrc-2nd" --- xmobar settings for 2nd display
  xmonad $ fullscreenSupport $ ewmh $ docks docksDefaults

docksDefaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        manageHook         = myManageHook,
        layoutHook         = gaps [(L,0), (R,20), (U,35), (D,5)] $ mySpacing 5 $ smartBorders $ showWName' myShowWNameTheme $ myLayout,
        --layoutHook         = showWName' myShowWNameTheme $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook >> addEWMHFullscreen
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run

import XMonad.Hooks.EwmhDesktops
--import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Loggers

import System.Process
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Unsafe
import Data.Maybe

--import XMonad.Hooks.StatusBar
--import XMonad.Hooks.StatusBar.PP
--import qualified XMonad.StackSet as W

main::IO ()

main =  do
        xrdbVal <- getXrdb
        xmproc0 <- spawnPipe "xmobar -A 130"
        --xmproc1 <- spawnPipe "xmessage"
        --putStr xrdbVal
        -- xmonad . ewmh =<< statusBar myBar myXmobarPP toggleStrutsKey myConfig
        --xmonad . ewmh =<< statusBar myBar myXmobarPP toggleStrutsKey myConfig xmproc0
        xmonad $ ewmh $ myConfig xrdbVal xmproc0
        --xmonad $ myConfig xrdbVal xmproc0
        -- . ewmh =<< statusBar "xmobar" def toggleStrutsKey myConfig
        -- . withEasySB (statusBarProp "xmobar" (pure def)) toggleStrutsKey 
        -- $ myConfig
        where
                toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
                toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myBar = "xmobar"
--myXmobarPP :: PP
myXmobarPP colVals proc = dynamicLogWithPP $ xmobarPP
                { ppSep             = col6 " • "
                , ppOutput = \x -> hPutStrLn proc x
                , ppTitleSanitize   = xmobarStrip
                , ppTitle           = col3 . shorten 40
                -- , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
                , ppCurrent         = wrap (col2 "[") (col2 "]")
                , ppHidden          = white . wrap " " ""
                , ppHiddenNoWindows = col8 . wrap " " ""
                , ppUrgent          = col3 . wrap (col6 "!") (col6 "!")
                , ppVisible         = wrap (col1 "(") (col1 ")")
                -- , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
                -- , ppOutput                = writeFile "~/output.txt"
                --, ppExtras          = [logTitles formatFocused formatUnfocused]
                --, ppExtras          = [formatFocused formatUnfocused]
                }
        where
                -- formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
                -- formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow
                -- | Windows should have *some* title, which should not not exceed a
                -- sane length.
                -- ppWindow :: String -> String
                -- ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30


                blue, lowWhite, magenta, red, white, yellow :: String -> String
                col0  = xmobarColor (luxr "*color0" colVals) ""
                col1  = xmobarColor (luxr "*color1" colVals) ""
                col2  = xmobarColor (luxr "*color2" colVals) ""
                col3  = xmobarColor (luxr "*color3" colVals) ""
                col4  = xmobarColor (luxr "*color4" colVals) ""
                col5  = xmobarColor (luxr "*color5" colVals) ""
                col6  = xmobarColor (luxr "*color6" colVals) ""
                col7  = xmobarColor (luxr "*color7" colVals) ""
                col8  = xmobarColor (luxr "*color8" colVals) ""
                col9  = xmobarColor (luxr "*color9" colVals) ""
                --magenta  = xmobarColor "#ff79c6" ""
                magenta  = xmobarColor (luxr "*color3" colVals) ""
                --blue     = xmobarColor "#bd93f9" ""
                blue     = xmobarColor (luxr "*color6" colVals) ""
                --blue     = xmobarColor (luxr "*color3") ""
                white    = xmobarColor "#f8f8f2" ""
                yellow   = xmobarColor "#f1fa8c" ""
                --yellow   = xmobarColor (luxr "*color33") ""
                red      = xmobarColor "#ff5555" ""
                lowWhite = xmobarColor "#bbbbbb" ""

getXrdb = readFile "/home/brunocca/.walxrvals"
mapVals :: String -> Map.Map String String
mapVals vals = Map.fromList valt
	where valt = (mapMaybe tuppy (map (splitOn ":\t") (lines vals)))

luxr :: String -> String -> String
luxr n vals = if isNothing res then "#000000" else fromJust res
		where res = Map.lookup n (mapVals vals)
tuppy :: [a] -> Maybe (a,a)
tuppy x = if length x == 2 then Just (tuppyif x) else Nothing
  
tuppyif :: [a] -> (a,a)
tuppyif [x,y] = (x,y)

myConfig colVals logHandle = defaultConfig
        { terminal    = "urxvt"
        , modMask     = mod4Mask
        , borderWidth = 3
        , logHook = myXmobarPP colVals logHandle
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
        , handleEventHook = fullscreenEventHook <+> handleEventHook defaultConfig <+> docksEventHook
        , focusedBorderColor = luxr "*color6" colVals
        , normalBorderColor = luxr "*color8" colVals
        --, focusedBorderColor = "#ff0000" 
        --, startupHook = xrdbVal >>= xmessage
        }
        `additionalKeysP`
        [ ("M-d", spawn "rofi -show drun")
        , ("<Print>", spawn "scrot -o 'lastScrot.png' -e 'xclip -selection clipboard -t image/png < $f'")
        , ("C-<Print>", unGrab *> spawn "scrot -o 'lastScrot.png' -ue 'xclip -selection clipboard -t image/png < $f'")
        , ("S-<Print>", unGrab *> spawn "scrot -o 'lastScrot.png' -s -e 'xclip -selection clipboard -t image/png < $f'")
        , ("M-q", spawn "xmonad --recompile; pkill xmobar; xmonad --restart")
        , ("M-b", sendMessage ToggleStruts)
        , ("M-f", sendMessage $ JumpToLayout "Full")
        --, ("M-x", sendMessage NextLayout >> (dynamicLogString xmobarPP >>= xmessage))
        --, ("M-x", spawn ("xmessage \"" ++ xrdbVal ++ "\""))
        --, ("M-a", spawn ("echo \"" ++ xrdbVal ++ "\" | xmessage -file -"))
        --, ("M-x",  xrCommand)
        --, ("M-x",  xmessage xrdbVal)
        --, ("M-x", writeFile "/tmp/test.txt" xrdbVal)
        --, ("M-x", spawn ("echo \"" ++ xrdbVal ++ "\" > /tmp/test.txt"))
        ]


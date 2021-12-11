#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi
xrdb -merge "$userresources"
xmodmap "$usermodmap"

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi
#
#twm &
#xclock -geometry 50x50-1+1 &
#xterm -geometry 80x50+494+51 &
#xterm -geometry 80x20+494-0 &
#exec xterm -geometry 80x66+0+0 -name login
#xrandr --output DVI-D-1 --auto --right-of VGA-1
#xrandr --output VGA-1 --auto
#xrandr --output DisplayPort-1 --auto
#xrandr --output HDMI-0 --off
export QT_QPA_PLATFORMTHEME=qt5ct
#for desktop monitor config
xrandr --output DP-2 --primary
xrandr --output VGA-1-1 --auto
xrandr --output VGA-1-1 --left-of DP-2
#feh --bg-scale ~/bg/2291206.png
picom &
#update this with desired wallpaper before starting up. or comment out line
~/scripts/setbg.sh ~/bg/wallhaven-4x86j3_3840x1080.png
#start pulseaudio
#pulseaudio -D
pipewire &
pipewire-pulse &
wireplumber &
#remove if not using ortho split keeb
setxkbmap us -variant colemak
#log errrors to file
exec xmonad > errs
#exec i3

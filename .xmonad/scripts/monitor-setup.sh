#!/bin/bash

xrandr --output DP-0 --primary --mode 1920x1080 --rate 144.00
xrandr --output HDMI-0 --mode 1600x900 --rotate right --left-of DP-0

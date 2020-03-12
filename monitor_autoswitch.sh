#!/bin/bash

# Prevent racing
xrandr | grep "DP1 connected"
if [[ $? == 0 ]]; then
  # is connected
  xrandr --output DP1 --left-of eDP1 --auto
else
  # not connected
  xrandr --output DP1 --auto
fi

xrandr | grep "DP2 connected"
if [[ $? == 0 ]]; then
  # is connected
  xrandr --output DP2 --left-of DP1 --auto
else
  # not connected
  xrandr --output DP2 --auto
fi

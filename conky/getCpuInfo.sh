#!/bin/bash

CORE=$1
echo $(echo "scale = 2; $(cat /proc/cpuinfo | grep 'cpu MHz' -m $(($CORE+1)) | tail -n 1 | sed 's/.*: //g') / 1000" | bc)GHz
#echo $(echo "scale = 2; $(cat /sys/devices/system/cpu/cpu$CORE/cpufreq/cpuinfo_cur_freq) / 1000000" | bc)GHz

#!/bin/bash

[[ $1 == "temp"      ]] && echo $(grep "yweather:condition"  ~/.cache/weather.xml | grep -o "temp=\"[^\"]*\""     | grep -o "\"[^\"]*\"" | grep -o "[^\"]*")
[[ $1 == "pressure"  ]] && echo $(grep "yweather:atmosphere" ~/.cache/weather.xml | grep -o "pressure=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*") 
[[ $1 == "humidity"  ]] && echo $(grep "yweather:atmosphere" ~/.cache/weather.xml | grep -o "humidity=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*")
[[ $1 == "wind"      ]] && echo $(grep "yweather:wind"       ~/.cache/weather.xml | grep -o "speed=\"[^\"]*\""    | grep -o "\"[^\"]*\"" | grep -o "[^\"]*")
[[ $1 == "condition" ]] && echo $(grep "yweather:condition"  ~/.cache/weather.xml | grep -o "text=\"[^\"]*\""     | grep -o "\"[^\"]*\"" | grep -o "[^\"]*")

[[ $1 == "forecastday1" ]] && echo $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "day=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==1' )
[[ $1 == "forecastday2" ]] && echo $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "day=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==2' )
[[ $1 == "forecastday3" ]] && echo $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "day=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==3' )

[[ $1 == "forecasttemp1" ]] && echo $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "low=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==1')° / $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "high=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==1')°
[[ $1 == "forecasttemp2" ]] && echo $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "low=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==2')° / $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "high=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==2')°
[[ $1 == "forecasttemp3" ]] && echo $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "low=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==3')° / $(grep "yweather:forecast" ~/.cache/weather.xml | grep -o "high=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*" | awk 'NR==3')°

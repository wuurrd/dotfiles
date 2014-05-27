#!/usr/bin/env bash
ITUNES_TRACK=$(osascript <<EOF
set trackname to missing value
tell application "iTunes"
  if player state is playing then
    set trackname to name of the current track
  end if
end tell

tell application "Spotify"
  if player state is playing then
    set trackname to name of the current track
  end if
end tell

trackname
EOF)
 
if test "x$ITUNES_TRACK" != "xmissing value"; then
ITUNES_ARTIST=$(osascript <<EOF
set trackname to missing value
tell application "iTunes"
  if player state is playing then
    set trackname to artist of the current track
  end if
end tell

tell application "Spotify"
  if player state is playing then
    set trackname to artist of the current track
  end if
end tell

trackname
EOF)
 
    echo '♫' $ITUNES_TRACK '-' $ITUNES_ARTIST
fi

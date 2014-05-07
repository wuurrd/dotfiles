#!/usr/bin/env bash
ITUNES_TRACK=$(osascript <<EOF
tell application "iTunes"
  if player state is playing then
    get the name of the current track
  end if
end tell
tell application "Spotify"
  if player state is playing then
    get the name of the current track
  end if
end tell
EOF)
 
if test "x$ITUNES_TRACK" != "x"; then
ITUNES_ARTIST=$(osascript <<EOF
tell application "iTunes"
  if player state is playing then
    get the artist of the current track 
  end if
end tell
tell application "Spotify"
  if player state is playing then
    get the artist of the current track 
  end if
end tell
EOF)
 
    echo '♫' $ITUNES_TRACK '-' $ITUNES_ARTIST
fi

# yes, we need a way to flush the file on disconnect; i don't know one
# yes, that's flush is not atomic (but good enough for me)

ssh localhost "tail -n 10 .irssi/fnotify ; : > .irssi/fnotify2; tail -f .irssi/fnotify2 " | sed -u 's/[<@&]//g' | while read heading message do notify-send -i gtk-dialog-info -t 3000

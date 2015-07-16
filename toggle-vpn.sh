#! /bin/sh

if [ "$(pidof vpnc-connect)"  ]
then
    echo "ENABLED"
    gksudo vpnc-disconnect
else
    echo "DISABLED"
    gksudo vpnc-connect
fi

#!/bin/bash

# customize these
WGET=wget
ICS2ORG=ical2orgpy
ICSFILE=$(mktemp)
ORGFILE=~/.emacs.d/calendar.org
URL=https://p29-calendarws.icloud.com/ca/subscribe/1/liatr2up1-Y9mMEzUnSdg15kNizkHYfOMgZGDJJ-AqxvvFhHx9hksEr7txobz6ET

# no customization needed below

$WGET -O $ICSFILE $URL
$ICS2ORG $ICSFILE $ORGFILE
rm -f $ICSFILE

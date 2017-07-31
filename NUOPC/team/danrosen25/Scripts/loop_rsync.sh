#!/bin/bash

while true
do
 time=`TZ=America/Denver date +"%r"`
 printf "###############\n# $time #\n###############\n"
 ps ux | grep "rsync"
 sleep 60
done


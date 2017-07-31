#!/bin/bash

while true
do
 time=`TZ=America/Denver date +"%r"`
 printf "###############\n# $time #\n###############"
 qstat -u $USER
 sleep 60
done


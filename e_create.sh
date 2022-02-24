#!/bin/bash

if [ -e $1 ];
then
    echo "File already exists!"
else
    echo -e "$(date +'%Y.%m.%d %A')\n$1\n" > "$1.txt"
fi

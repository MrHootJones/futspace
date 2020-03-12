#!/bin/bash

#Requirements: need imagemagick installed for the conversion process

for file in maps/*.png; 
do
    FILENAME=${file:5}
    #echo "Processing $file.."
    if test -f converted-maps/$FILENAME; then
        : #echo "Already converted $file. Continuing.."
    else
        convert $file -define png:color-type=2 converted-maps/$FILENAME
    fi
    #echo "Converting to futhark array.."
    if test -f program-inputs/${FILENAME%.*}.in; then
        : #echo "Already converted program-inputs/$FILENAME. Continuing.."
    else
        python png2data.py converted-maps/$FILENAME program-inputs/${FILENAME%.*}.in
    fi
done
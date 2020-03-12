#!/bin/bash
#used specifically to test futspace.fut in the tests folder.
#Usage: ./runfutspace.sh C1W
COLORMAP=$1
HEIGHTMAP=${COLORMAP#C}
HEIGHTMAP=${HEIGHTMAP%W}

cat ../data/binary-fut-arrays/$COLORMAP.in ../data/binary-fut-arrays/D$HEIGHTMAP.in | ./futspace -b -D > img_map.data
python data2png.py img_map.data success.png
rm img_map.data
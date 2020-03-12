#!/usr/bin/env python3
#
# Turn a PNG image into a Futhark value of type [height][width]i32 encoded in
# the binary data format.
# Usage: ./png2data image.png output.data
#
# Absolutely no error checking is done.
#
# png2data currently supports 24-bit RGB PNGs. Transparency, grayscale, multiple
# palettes might work.
# most other features of PNG is not supported.

import sys
import numpy as np
import png

def indexToRGB(image, palette):
    returnImage = []
    for i in image:
        tempArray = []
        for j in i:
            (r,g,b) = palette[j]
            tempArray.append(r)
            tempArray.append(g)
            tempArray.append(b)
            
        returnImage.append(tempArray)
    return returnImage
    
if __name__ == '__main__':
    infile = sys.argv[1]
    outfile = sys.argv[2]

    r = png.Reader(infile)
    (width, height, img, info) = r.read()
    image_2d = np.vstack(map(np.int32, img))
	
    if info.keys()[0] == "palette" :
        image_2d = indexToRGB(image_2d, info["palette"])
	
    image_3d = np.reshape(image_2d,
                             (height, width, 3))


    array = np.empty((height, width), dtype=np.int32)
    array = array | (image_3d[:,:,0] << 16)
    array = array | (image_3d[:,:,1] << 8)
    array = array | (image_3d[:,:,2])

    with open(outfile, 'wb') as f:
        f.write(b'b')
        f.write(np.int8(2))
        f.write(np.int8(2))
        f.write(b' i32')
        f.write(np.uint64(height))
        f.write(np.uint64(width))
        array.tofile(f)

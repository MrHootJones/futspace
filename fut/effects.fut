import "../lib/github.com/athas/matte/colour"

let interpolate [h][w] (pd: i32) (rendered_image: [h][w]i32) : [h][w]i32 =
    let m1 = 1.0
    let m2 = 1.0
    let smoothed_image = 
        map (\y ->
            map (\x -> 
                    let c = unsafe rendered_image[y, x]
                    let u = unsafe rendered_image[(y-pd)%h, x]
                    let d = unsafe rendered_image[(y+pd)%h, x]
                    let l = unsafe rendered_image[y, (x-pd)%h]
                    let r = unsafe rendered_image[y, (x+pd)%h]
                    let ul = unsafe rendered_image[(y-pd)%h, (x-pd)%h]
                    let ur = unsafe rendered_image[(y-pd)%h, (x+pd)%h]
                    let dl = unsafe rendered_image[(y+pd)%h, (x-pd)%h]
                    let dr = unsafe rendered_image[(y+pd)%h, (x+pd)%h]
                    in argb.mix m1 c m2 (argb.mix m1 u m1 (argb.mix m1 d m1 (argb.mix m1 r m1 (argb.mix m1 l m1 (argb.mix m1 ul m1 (argb.mix m1 ur m1 (argb.mix m1 dl m1 dr)))))))
                    ) (0..<w)
                        ) (0..<h)
    in smoothed_image

let modulate [h][w] (num: f32) (height_map: [h][w]i32) = 
    map (\heights -> 
        map2 (\height j -> 
            height + i32.f32 ((f32.sin num * (f32.i32 j)) * f32.i32 height)
            ) heights (iota w)
                ) height_map

let does_occlude (height1: f32, distance1: f32, sun_dy1: f32, occludes1: bool) (height2: f32, distance2: f32, sun_dy2: f32, occludes2: bool) : (f32, f32, f32, bool) =
    if distance1 * sun_dy1 < height1 then
        (height2, distance2, sun_dy1, occludes1 || occludes2 || true)
    else
        (height2, distance2, sun_dy1, occludes1 || occludes2 || false)

let shadowmap_reduce [h][w] (height_map: [h][w]i32) (color_map: [h][w]i32) (sun_dy: f32) : [h][w]i32=
    map3 (\height_row color_row y -> 
            map3 (\height color x -> 
                    let elements_before = height_row[0:x:1]
                    let tuples = map2 (\elem idx -> (f32.i32 elem, f32.i32 (x-idx), sun_dy, false)) elements_before (0..<x)
                    let (_,_,_,occludes) = reduce (does_occlude) (0.0, -1.0, sun_dy, false) tuples
                    in
                    if occludes == true then
                        (argb.mix 1.0 argb.black 1.0 color)
                    else
                        color
                    ) height_row color_row (0..<w)
            ) height_map color_map (0..<h)


--heightmap axis-aligned shadows by scanning across the height and color maps and adjusting the colors of the colormap based on whether the height of the previous voxel
--intersects with a vector representing the sun
let shade (color1: i32, height1: f32, sun_descent1: f32) (color2: i32, height2: f32, sun_descent2: f32) : (i32, f32, f32)=
    if height1 > height2 then
        --((argb.from_rgba 0.0 0.0 0.0 (f32.max 0 (f32.min 1 (f32.abs (height1 - height2))))), height1 - sun_descent1, sun_descent1)
        ((argb.mix (1.5 * (f32.max 0 (f32.min 1 (f32.abs (height1 - height2))))) argb.black 1.0 color2), height1 - sun_descent1, sun_descent1)
    else
        (color2, height2, sun_descent1)

let sunlight_sequential [h][w] (sun_height: f32) (sun_descent: f32) (color_map: [h][w]i32) (height_map: [h][w]i32) : [h][w]i32 =
    let neutral = replicate h (replicate w (argb.from_rgba 0.0 0.0 0.0 0.0))
    let tuples = map2 (\color_row height_row -> map2 (\x y -> (x, f32.i32 y, sun_descent)) color_row height_row) color_map height_map
    let shadowed = map (\rows -> map (\elem -> elem.0) (scan (shade) (0, 0.0, sun_descent) rows)) tuples
    --let smoothed_shadows = interpolate 2 shadowed
    in shadowed--smoothed_shadows--map2 (\row1 row2 -> map2 (\col1 col2 -> argb.add_linear col1 col2) row1 row2) color_map smoothed_shadows

--shades based only on the previous height value
let sunlight_deprecated [h][w] (color_map: [h][w]i32) (height_map: [h][w]i32) : [h][w]i32 = 
    let height_map = height_map
    let color_map = color_map
    in map3 (\color_row height_row prev_height_row-> 
            map3 (\color height prev_height -> if prev_height > height then (argb.mix 0.9 argb.black 1.0 color) else color) color_row height_row prev_height_row
            ) color_map height_map (map (\row -> rotate (-1) row) height_map)

--more sophisticated than the deprecated version. scans rows of terrain to attempt to determine when a voxel should be shaded 
--based on the most-recently found highest voxel height. calculation is wrong though

--keep track of tallest height encountered, check if sun projection from tallest height intersects with following height
let shade_old (color1: i32, height1: i32, sun_height1: f32, sun_descent1: f32) (color2: i32, height2: i32, sun_height2: f32, sun_descent2: f32) : (i32, i32, f32, f32)=
    if height1 > height2 then
        if sun_height1 > 0.0 then
            ((argb.mix (sun_height1) argb.black 1.0 color2), height1, sun_height1 - sun_descent1, sun_descent1)
        else
            ((argb.mix (0.8) argb.black 1.0 color2), height2, sun_height1, sun_descent1)
    else (color2, height2, sun_height2, sun_descent1)
--for decent results use sun_descent=0.05-0.10 and sun_height>0
let sunlight_old [h][w] (sun_height: f32) (sun_descent: f32) (color_map: [h][w]i32) (height_map: [h][w]i32) : [h][w]i32 = 
    let tuples = map2 (\color_row height_row -> map2 (\x y -> (x, y, sun_height, sun_descent)) color_row height_row) color_map height_map
    let shadowed = map (\rows -> map (\elem -> elem.0) (scan (shade_old) (0, 0, 0.0, sun_descent) rows ) ) tuples
    in shadowed